;; This program is intended to automate the tasks of building chicken
;; out of git sources, run salmonella on the full set of eggs and
;; generate the HTML reports.  It organizes the salmonella reports in
;; a directory tree like:
;;
;; web-dir/chicken-core-branch/software-platform/hardware-platform/year/month/day
;;
;; For example:
;;
;; /var/www/salmonella-reports/master/linux/x86/2012/01/28


;; The following external tools are required:
;; - salmonella
;; - salmonella-html-report
;; - salmonella-feeds
;; - henrietta-cache (only in local-mode)
;; - svn (only in local-mode)
;; - git
;; - bzip2
;; - chicken tools (chicken, csi, chicken-install)
;; - graphviz (dot program, for dependencies graphs generation -- salmonella-html-report)

;; TODO
;; - loop reading commands output port instead of read-all
;; - substitute system `rm' by some scheme code
;; - local-mode
;; - notify vandusen

(use posix utils srfi-1 irregex http-client)

;;
;; User-configurable parameters
;;
(define tmp-dir
  ;; Temporary directory used by this program
  (make-parameter
   (make-pathname (current-directory) "salmonella-run-publish")))

(define chicken-bootstrap-prefix
   ;; Chicken installation prefix to be used to build the bootstrap
   ;; compiler. When `#f', the chicken tools from path are picked.
  (make-parameter #f))

(define log-file
  (make-parameter (make-pathname (tmp-dir) "run-salmonella.log")))

(define chicken-core-git-uri
  (make-parameter "git://code.call-cc.org/chicken-core"))

(define chicken-core-branch
  (make-parameter "master"))

(define make-program
  (make-parameter
   (if (eq? (software-version) 'linux)
       "make"
       "gmake")))

(define keep-repo?
  ;; salmonella's --keep-repo option
  (make-parameter #f))

(define skip-eggs
  ;; salmonella's --skip-eggs
  (make-parameter
   (if (eq? (software-version) 'macosx)
       '(proccpuinfo)
       '(macosx objc hfs+ osxattr))))

(define henrietta-uri
  (make-parameter "http://code.call-cc.org/cgi-bin/henrietta.cgi"))

(define local-mode? ;; FIXME: #t is not implemented yet
  ;; In local mode, egg sources and documentation are available
  ;; locally (no Internet access required)
  (make-parameter #f))

(define web-dir
  ;; Directory where to publish the salmonella report
  (make-parameter
   (make-pathname (current-directory) "salmonella-reports")))

(define verbose?
  (make-parameter #t))

(define software-platform (symbol->string (software-version)))

(define hardware-platform (symbol->string (machine-type)))


(define program-available?
  (let ((paths (string-split (get-environment-variable "PATH") ":"))) ;; no windows support
    (lambda (program)
      (let loop ((paths paths))
        (if (null? paths)
            #f
            (let ((path (car paths)))
              (or (file-exists? (make-pathname path program))
                  (loop (cdr paths)))))))))


(define (check-required-programs!)
  (let* ((required-programs
          (append '("bzip2"
                    "dot"
                    "git"
                    "salmonella"
                    "salmonella-feeds"
                    "salmonella-html-report")
                  (if (local-mode?)
                      '("henrietta-cache"
                        "svn")
                      '())
                  (if (chicken-bootstrap-prefix)
                      '()
                      '("csi"
                        "chicken"))))
         (missing-programs (remove program-available? required-programs)))
    (when (chicken-bootstrap-prefix)
      (unless (and (file-exists?
                    (make-pathname (list (chicken-bootstrap-prefix) "bin") "csi"))
                   (file-exists?
                    (make-pathname (list (chicken-bootstrap-prefix) "bin") "chicken")))
        (die "Could not find `csi' or `chicken'. "
             "They are required to build the chicken-core bootstrap compiler.")))
    (unless (null? missing-programs)
      (with-output-to-port (current-error-port)
        (lambda ()
          (print "The following programs are required but could not be found:")
          (for-each print missing-programs)))
      (exit 1))))


(define (debug . things)
  (let ((msg (string-intersperse (map ->string things) "")))
    (when (verbose?)
      (print msg))
    (when (log-file)
      (with-output-to-file (log-file)
        (lambda ()
          (print msg))
        append:))))


(define (pad-number n zeroes)
  (define (pad num len)
    (let ((str (if (string? num) num (number->string num))))
      (if (string-null? str)
          ""
          (if (>= (string-length str) len)
              str
              (string-pad str len #\0)))))

  (let ((len (string-length (->string n))))
    (if (= len zeroes)
        (number->string n)
        (pad n zeroes))))


(define (! cmd #!optional dir)
  (let ((cmd (string-intersperse (map ->string cmd)))
        (cwd (and dir (current-directory))))
    (when dir
      (change-directory dir)
      (debug "@" dir))
    (debug cmd)
    (let* ((p (open-input-pipe (sprintf "~A 2>&1" cmd)))
           (output (read-all p))
           (exit-status (arithmetic-shift (close-input-pipe p) -8)))
      (when dir (change-directory cwd))
      (unless (zero? exit-status)
        (debug "Command '" cmd "' exited abnormally with status " exit-status)
        (debug output)
        (exit exit-status))
      (cons exit-status output))))


(define (build-chicken-core chicken-core-dir chicken-prefix)
  (let ((chicken-bootstrap
         (if (chicken-bootstrap-prefix)
             (make-pathname (list (chicken-bootstrap-prefix) "bin") "chicken")
             "chicken")))

    ;; Get the most recent version of the chicken-core
    (if (file-exists? chicken-core-dir)
        (! `(git pull) chicken-core-dir)
        (! `(git clone -b ,(chicken-core-branch) ,(chicken-core-git-uri)) (tmp-dir)))

    ;; make boot-chicken
    (! `(,(make-program) ,(string-append "PLATFORM=" software-platform
                                         " CHICKEN=" chicken-bootstrap)
         spotless clean confclean boot-chicken)
       chicken-core-dir)

    ;; make install
    (! `(,(make-program) ,(string-append "PLATFORM=" software-platform
                                         " PREFIX=" chicken-prefix
                                         " CHICKEN=./chicken-boot")
         spotless install)
       chicken-core-dir)

    ;; make check
    (! `(,(make-program) ,(string-append "PLATFORM=" software-platform
                                         " PREFIX=" chicken-prefix
                                         " CHICKEN=./chicken-boot")
         check)
       chicken-core-dir)))


(define (list-eggs)
  (with-input-from-request
   (string-append (henrietta-uri) "?list=1")
   #f
   read-file))


(define (run-salmonella)
  (let ((salmonella-repo-dir (make-pathname (tmp-dir) "salmonella-repo"))
        (chicken-core-dir (make-pathname (tmp-dir) "chicken-core"))
        (chicken-prefix (make-pathname (tmp-dir) "chicken")))
    ;; Remove previous run data
    (for-each (lambda (file)
                (! `(rm -rf ,file) (tmp-dir)))
              `(,chicken-prefix
                salmonella.log
                salmonella.log.bz2
                salmonella-repo
                ,(log-file)
                salmonella-report))

    ;; Build chicken
    (build-chicken-core chicken-core-dir chicken-prefix)

    ;; Run salmonella
    (! `(rm -rf ,salmonella-repo-dir))
    (! `(,(string-append
           "LC_ALL=C LANG=C PATH=" (make-pathname chicken-prefix "bin") ":$PATH"
           " salmonella"
           (if (null? skip-eggs)
               ""
               (string-append " --skip-eggs="
                              (string-intersperse (map ->string (skip-eggs)) ",")))
           (if (keep-repo?)
               " --keep-repo"
               "")
           " --repo-dir=" salmonella-repo-dir
           " --chicken-installation-prefix=" chicken-prefix
           " "
           (string-intersperse (map symbol->string (list-eggs))))))))


(define (publish-results)
  (let* ((now (seconds->local-time))
         (day (pad-number (vector-ref now 3) 2))
         (month (pad-number (add1 (vector-ref now 4)) 2))
         (year (number->string (+ 1900 (vector-ref now 5))))
         (publish-dir (make-pathname (list (web-dir)
                                           (chicken-core-branch)
                                           software-platform
                                           hardware-platform
                                           year
                                           month)
                                     day))
         (feeds-dir (make-pathname (list (web-dir)
                                         "feeds"
                                         (chicken-core-branch)
                                         software-platform)
                                   hardware-platform))
         (today-path (make-pathname (list year month) day)))

    (unless (file-exists? publish-dir)
            (create-directory publish-dir 'with-parents))

    (unless (file-exists? feeds-dir)
            (create-directory feeds-dir 'with-parents))

    ;; Generate the atom feeds
    (! `(salmonella-feeds --log-file=salmonella.log
                          --feeds-server=http://tests.call-cc.org
                          ,(string-append "--feeds-web-dir="
                                          (make-absolute-pathname
                                           (list "feeds"
                                                 (chicken-core-branch)
                                                 software-platform)
                                           hardware-platform))
                          ,(string-append "--salmonella-report-uri=http://tests.call-cc.org"
                                          (make-absolute-pathname
                                           (list (chicken-core-branch)
                                                 software-platform
                                                 hardware-platform
                                                 year
                                                 month
                                                 day)
                                           "salmonella-report"))
                          ,(string-append "--feeds-dir="
                                          (make-pathname
                                           (list (web-dir)
                                                 "feeds"
                                                 (chicken-core-branch)
                                                 software-platform)
                                           hardware-platform)))
       (tmp-dir))

    ;; Generate the HTML report
    (! `(salmonella-html-report salmonella.log salmonella-report) (tmp-dir))

    (! `(bzip2 -9 salmonella.log) (tmp-dir))

    (for-each (lambda (file)
                (! `(cp -R ,file ,publish-dir) (tmp-dir)))
              `("salmonella-report"
                "salmonella.log.bz2"
                ,(log-file)))))


(define (usage #!optional exit-code)
  (print "Usage: " (pathname-strip-directory (program-name)) " <config file>")
  (when exit-code
    (exit exit-code)))


(define (die . msg)
  (with-output-to-port (current-error-port)
    (lambda ()
      (for-each display msg)
      (newline)
      (flush-output)))
  (exit 1))



(let ((args (command-line-arguments)))

  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))

  ;; Load config file if provided
  (unless (null? args)
    (load (car args)))

  (check-required-programs!)

  ;; Create the tmp dir if it does not exists
  (unless (file-exists? (tmp-dir))
    (create-directory (tmp-dir) 'parents-too))

  ;; Change to the tmp dir
  (change-directory (tmp-dir))

  (run-salmonella)
  (publish-results))
