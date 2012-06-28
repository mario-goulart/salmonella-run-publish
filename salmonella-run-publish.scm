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
                    "salmonella-diff"
                    "salmonella-feeds"
                    "salmonella-html-report"
                    "svn")
                  (if (local-mode?)
                      '("henrietta-cache")
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
      (debug output)
      (when dir (change-directory cwd))
      (unless (zero? exit-status)
        (debug "Command '" cmd "' exited abnormally with status " exit-status)
        (publish-results)
        (exit exit-status))
      (cons exit-status output))))


(define (build-chicken-core chicken-core-dir chicken-prefix)
  (let ((chicken-bootstrap
         (if (chicken-bootstrap-prefix)
             (make-pathname (list (chicken-bootstrap-prefix) "bin") "chicken")
             "chicken")))

    ;; Get the most recent version of the chicken-core
    (if (file-exists? chicken-core-dir)
        (begin
          (! `(git checkout ,(chicken-core-branch)) chicken-core-dir)
          (! '(git pull) chicken-core-dir)
          (! '(git clean -f) chicken-core-dir)
          (! '(git checkout -f) chicken-core-dir))
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
                custom-feeds
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


(define publish-base-dir
  ;; Publishing directory, without yyyy/mm/dd.  It has to be a
  ;; procedure because we need to delay the determination of the
  ;; publish dir until the `chicken-core-branch' parameter is set.
  (let ((dir #f))
    (lambda ()
      (unless dir
        (set! dir (make-pathname (list (web-dir)
                                       (chicken-core-branch)
                                       software-platform)
                                 hardware-platform)))
      dir)))


(define publish-dir
  ;; Final publishing directory, with yyyy/mm/dd
  (let ((dir #f))
    (lambda ()
      (unless dir
        (let* ((now (seconds->local-time))
               (day (pad-number (vector-ref now 3) 2))
               (month (pad-number (add1 (vector-ref now 4)) 2))
               (year (number->string (+ 1900 (vector-ref now 5)))))
          (set! dir (make-pathname (list (publish-base-dir)
                                         year
                                         month)
                                   day))))
      dir)))


(define publish-web-dir
  ;; Final publishing directory, with yyyy/mm/dd
  (let ((dir #f))
    (lambda ()
      (unless dir
        (let* ((now (seconds->local-time))
               (day (pad-number (vector-ref now 3) 2))
               (month (pad-number (add1 (vector-ref now 4)) 2))
               (year (number->string (+ 1900 (vector-ref now 5)))))
          (set! dir (make-absolute-pathname
                     (list (chicken-core-branch)
                           software-platform
                           hardware-platform
                           year
                           month)
                     day))))
      dir)))


(define (diff)
  (let* ((yesterday (seconds->local-time (- (current-seconds) (* 24 60 60))))
         (yesterday-day (pad-number (vector-ref yesterday 3) 2))
         (yesterday-month (pad-number (add1 (vector-ref yesterday 4)) 2))
         (yesterday-year (number->string (+ 1900 (vector-ref yesterday 5))))
         (yesterday-path (make-pathname (list yesterday-year yesterday-month)
                                        yesterday-day))
         (yesterday-clog (make-pathname (list (publish-base-dir) yesterday-path)
                                        "salmonella.log.bz2"))
         (today-log (make-pathname (tmp-dir) "salmonella.log")))
    (when (file-exists? yesterday-clog)
      ;; Uncompress yesterday's log
      (! `(bzip2 -d -c ,yesterday-clog > yesterday.log)
         (tmp-dir))
      (! `(salmonella-diff --out-dir=yesterday-diff
                           --label1=Yesterday
                           --label2=Today
                           yesterday.log
                           ,today-log)
         (tmp-dir)))))


(define (process-results)
  (let* ((feeds-dir (make-pathname (list (web-dir)
                                         "feeds"
                                         (chicken-core-branch)
                                         software-platform)
                                   hardware-platform))
         (custom-feeds-dir (make-pathname (tmp-dir) "custom-feeds")))

    (unless (file-exists? (publish-dir))
            (create-directory (publish-dir) 'with-parents))

    (unless (file-exists? feeds-dir)
            (create-directory feeds-dir 'with-parents))

    (! `(svn co --username=anonymous --password=
             https://code.call-cc.org/svn/chicken-eggs/salmonella-custom-feeds
             custom-feeds)
       (tmp-dir))

    (when (file-exists? (make-pathname (tmp-dir) "salmonella.log"))
      (let* ((feeds-web-dir
              (make-absolute-pathname (list "feeds"
                                            (chicken-core-branch)
                                            software-platform)
                                      hardware-platform))
             (custom-feeds-web-dir
              (make-absolute-pathname feeds-web-dir "custom")))
        ;; Generate the atom feeds
        (! `(salmonella-feeds --log-file=salmonella.log
                              --feeds-server=http://tests.call-cc.org
                              ,(string-append "--feeds-web-dir=" feeds-web-dir)
                              ,(string-append "--salmonella-report-uri=http://tests.call-cc.org"
                                              (make-pathname (publish-web-dir)
                                                             "salmonella-report"))
                              ,(string-append "--feeds-dir="
                                              (make-pathname
                                               (list (web-dir)
                                                     "feeds"
                                                     (chicken-core-branch)
                                                     software-platform)
                                               hardware-platform))
                              ,(string-append "--custom-feeds-dir=" custom-feeds-dir)
                              ,(string-append "--custom-feeds-web-dir=" custom-feeds-web-dir)
                              ,(string-append "--custom-feeds-out-dir="
                                              (make-pathname feeds-dir "custom")))
           (tmp-dir)))

      ;; Generate the HTML report
      (! `(salmonella-html-report salmonella.log salmonella-report) (tmp-dir))

      ;; Generate diff against yesterday's log (if it exists)
      (diff)

      (! `(bzip2 -9 salmonella.log) (tmp-dir)))))


(define (publish-results)
  (for-each (lambda (file)
              (when (file-exists? file)
                (! `(cp -R ,file ,(publish-dir)) (tmp-dir))))
            `(,(log-file)
              "yesterday-diff"
              "salmonella-report"
              "salmonella.log.bz2")))


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
  (process-results)
  (publish-results))
