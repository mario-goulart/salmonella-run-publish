;; This program is intended to automate the tasks of building chicken
;; out of git sources, run salmonella on the full set of eggs and
;; generate the HTML reports.  It organizes the salmonella reports in
;; a directory tree like:
;;
;; web-dir/chicken-core-branch/c-compiler/software-platform/hardware-platform/year/month/day
;;
;; For example:
;;
;; /var/www/salmonella-reports/master/gcc/linux/x86/2012/01/28


;; The following external tools are required:
;; - salmonella
;; - salmonella-html-report
;; - salmonella-feeds
;; - salmonella-diff
;; - henrietta-cache (only in local-mode)
;; - svn (only in local-mode)
;; - git
;; - bzip2
;; - chicken tools (chicken, csi, chicken-install)
;; - graphviz (dot program, for dependencies graphs generation -- salmonella-html-report)
;; - tar
;; - gzip

;; TODO
;; - loop reading commands output port instead of read-all
;; - substitute system `rm' by some scheme code
;; - local-mode
;; - notify vandusen

(module salmonella-run-publish-app ()

(import chicken scheme)
(use posix utils srfi-1 srfi-13 irregex data-structures ports files extras)
(use http-client salmonella-run-publish-params)
(declare (uses chicken-syntax))

(define software-platform (symbol->string (software-version)))

(define hardware-platform (symbol->string (machine-type)))

(define make-platform
  (or (get-environment-variable "PLATFORM")
      (if (string-suffix? "bsd" software-platform)
          "bsd"
          software-platform)))

(define program-available?
  (let ((paths (string-split (get-environment-variable "PATH") ":"))) ;; no windows support
    (lambda (program)
      (let loop ((paths paths))
        (if (null? paths)
            #f
            (let ((path (car paths)))
              (or (file-exists? (if (absolute-pathname? path)
				    path
				    (make-pathname path program)))
                  (loop (cdr paths)))))))))

(define (program-path program)
  ;; Select programs in the following order of preference:
  ;; 1. (chicken-bootstrap-prefix)
  ;; 2. this program's prefix
  ;; 3. $PATH
  (call/cc
   (lambda (return)
     (when (chicken-bootstrap-prefix)
       (let ((path (make-pathname (list (chicken-bootstrap-prefix) "bin")
                                  program)))
           (when (file-exists? path)
             (return path))))
     (let* ((this-prefix (pathname-directory (pathname-directory (chicken-home))))
            (path (make-pathname (list this-prefix "bin") program)))
       (when (file-exists? path)
         (return path)))
     (return program))))

(define (check-required-programs!)
  (let* ((required-programs
          (append `("bzip2"
                    "dot"
                    "git"
                    "gzip"
                    ,(or (salmonella-path) (program-path "salmonella"))
                    ,(program-path "salmonella-diff")
                    ,(program-path "salmonella-feeds")
                    ,(program-path "salmonella-html-report")
                    "svn"
                    "tar")
                  (if (local-mode?)
                      `(,(program-path "henrietta-cache"))
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


(define (! cmd #!optional dir publish-dir)
  (let ((cmd (string-intersperse (map ->string cmd)))
        (cwd (and dir (current-directory))))
    (when dir
      (change-directory dir)
      (debug "@" (current-directory)))
    (debug cmd)
    (let-values (((in out pid) (process (sprintf "~A 2>&1" cmd))))
      (when (and publish-dir (hanging-process-killer-program))
        (system (sprintf "~a ~a ~a &"
                         (hanging-process-killer-program)
                         pid
                         (make-pathname publish-dir "hanging-processes.log"))))

      (let ((output (read-all in)))
	(let-values (((pid exit-normal? status) (process-wait pid)))
          (close-input-port in)
          (close-output-port out)
          (debug output)
          (when dir (change-directory cwd))
          (unless (zero? status)
            (fprintf (current-error-port) "Error executing '~a'.  Exit code: ~a.\n"
                     cmd status)
            (exit status))
          (cons status output))))))


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

    ((before-make-bootstrap-hook) chicken-core-dir)

    ;; make boot-chicken
    (! `(,(make-program) ,(string-append "PLATFORM=" make-platform
                                         " C_COMPILER=" (c-compiler)
                                         " CXX_COMPILER=" (c++-compiler)
                                         " CHICKEN=" chicken-bootstrap)
         spotless clean confclean boot-chicken)
       chicken-core-dir)

    ;; make install
    (! `(,(make-program) ,(string-append "PLATFORM=" make-platform
                                         " C_COMPILER=" (c-compiler)
                                         " CXX_COMPILER=" (c++-compiler)
                                         " PREFIX=" chicken-prefix
                                         " CHICKEN=./chicken-boot")
         spotless install)
       chicken-core-dir)

    (! `(,(make-program) ,(string-append "PLATFORM=" make-platform
                                         " C_COMPILER=" (c-compiler)
                                         " CXX_COMPILER=" (c++-compiler)
                                         " PREFIX=" chicken-prefix
                                         " CHICKEN=./chicken-boot")
         check)
       chicken-core-dir)

    ((after-make-check-hook) chicken-prefix)))


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
           "PATH="
           (make-pathname chicken-prefix "bin") ":$PATH"
           " "
	   (or (salmonella-path) (program-path "salmonella"))
           (if (null? skip-eggs)
               ""
               (string-append " --skip-eggs="
                              (string-intersperse (map ->string (skip-eggs)) ",")))
           (if (keep-repo?)
               " --keep-repo"
               " --clear-chicken-home")
           " --repo-dir=" salmonella-repo-dir
           " --chicken-installation-prefix=" chicken-prefix
           " "
           (string-intersperse (map symbol->string ((list-eggs))))))
       "."
       (tmp-dir))))


(define yesterday-log-path
  ;; Return the path to the yesterday's log file if it exists; or #f
  ;; if doesn't exist.
  ;;
  ;; The uncompressed log file is stored under (tmp-dir).
  (let ((log-path 'not-set))
    (lambda (publish-base-dir yesterday-dir)
      (if (eq? log-path 'not-set)
          (let ((yesterday-clog
                 (make-pathname (list publish-base-dir yesterday-dir)
                                "salmonella.log.bz2")))
            (cond ((file-exists? yesterday-clog)
                   (! `(bzip2 -d -c ,yesterday-clog > yesterday.log)
                      (tmp-dir))
                   (set! log-path (make-pathname (tmp-dir) "yesterday.log")))
                  (else (set! log-path #f))))
          log-path))))


(define (diff publish-base-dir publish-web-dir yesterday-dir yesterday-web-dir)
  (let ((yesterday-log (yesterday-log-path publish-base-dir yesterday-dir))
        (today-log (make-pathname (tmp-dir) "salmonella.log")))
    (when yesterday-log
      (! `(,(program-path "salmonella-diff")
           --out-dir=yesterday-diff
           --label1=Yesterday
           --label2=Today
           ,(if (salmonella-diff-link-mode?)
                (string-append "--report-uri1="
                               (make-pathname yesterday-web-dir
                                              "salmonella-report")
                               " "
                               "--report-uri2="
                               (make-pathname publish-web-dir
                                              "salmonella-report"))
                "")
           ,yesterday-log
           ,today-log)
         (tmp-dir)))))


(define (process-results publish-base-dir publish-web-dir today-dir
                         yesterday-dir yesterday-web-dir feeds-dir
                         feeds-web-dir)
  (let ((custom-feeds-dir (make-pathname (tmp-dir) "custom-feeds")))

    (create-directory feeds-dir 'with-parents)

    (! `(svn co --username=anonymous --password=
             https://code.call-cc.org/svn/chicken-eggs/salmonella-custom-feeds
             custom-feeds)
       (tmp-dir))

    (when (file-exists? (make-pathname (tmp-dir) "salmonella.log"))
      (let ((custom-feeds-web-dir
             (make-absolute-pathname feeds-web-dir "custom")))
        ;; Generate the atom feeds
        (! `(,(program-path "salmonella-feeds")
             --log-file=salmonella.log
             ,(string-append "--feeds-server=http://" (feeds-server))
             ,(string-append "--feeds-web-dir=" feeds-web-dir)
             ,(string-append "--salmonella-report-uri=http://"
                             (feeds-server)
                             (make-absolute-pathname publish-web-dir
                                                     "salmonella-report"))
             ,(string-append "--feeds-dir=" feeds-dir)
             ,(string-append "--custom-feeds-dir=" custom-feeds-dir)
             ,(string-append "--custom-feeds-web-dir=" custom-feeds-web-dir)
             ,(string-append "--custom-feeds-out-dir="
                             (make-pathname feeds-dir "custom"))
             ,@(let ((yesterday-log (yesterday-log-path publish-base-dir yesterday-dir)))
                 (if yesterday-log
                     (list
                      (string-append "--diff-against=" yesterday-log)
                      (string-append "--diff-feed-file-path="
                                     (make-pathname (list feeds-dir "diff")
                                                    "yesterday"
                                                    "xml"))
                      (string-append "--diff-feed-web-file-path="
                                     (make-pathname (list feeds-web-dir "diff")
                                                    "yesterday"
                                                    "xml"))
                      (string-append "--diff-against-report-uri="
                                     (make-pathname yesterday-web-dir "salmonella-report"))
                      (sprintf "--diff-label1='yesterday (~a)'" yesterday-dir)
                      (sprintf "--diff-label2='today (~a)'" today-dir))
                     '())))
           (tmp-dir)))

      ;; Generate the HTML report
      (let ((compressed (if (compress-report?)
                            `(--compress-html --compress-graphics)
                            '())))
        (! `(,(program-path "salmonella-html-report")
             ,@compressed salmonella.log salmonella-report)
           (tmp-dir)))

      ;; Generate diff against yesterday's log (if it exists)
      (diff publish-base-dir publish-web-dir yesterday-dir yesterday-web-dir))))


(define (publish-results publish-dir)
  (create-directory publish-dir 'with-parents)
  (! `(bzip2 -9 salmonella.log) (tmp-dir))
  (! `(gzip -9 -f -S z ,(log-file)) (tmp-dir))
  (for-each (lambda (file)
              (when (file-exists? file)
                (! `(cp -R ,file ,publish-dir) (tmp-dir))))
            `(,(string-append (log-file) "z")
              "hanging-processes.log"
              "yesterday-diff"
              "salmonella-report"
              "salmonella.log.bz2"))
  (when (create-report-tarball)
    (! (append
        (case (create-report-tarball)
          ((gzip) '(GZIP=-9))
          ((bzip2) '(BZIP2=-9))
          (else '()))
        `(tar ,(case (create-report-tarball)
                 ((gzip) 'czf)
                 ((bzip2) 'cjf)
                 (else 'cf))
              ,(string-append "salmonella-report.tar"
                              (case (create-report-tarball)
                                ((gzip) ".gz")
                                ((bzip2) ".bz2")
                                (else "")))
              "salmonella-report"))
       publish-dir)
    (! `(rm -rf "salmonella-report") publish-dir)))


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
  (create-directory (tmp-dir) 'parents-too)

  ;; Change to the tmp dir
  (change-directory (tmp-dir))

  (let* ((path-layout
          (make-pathname (list ((branch-publish-transformer) (chicken-core-branch))
                               (or (c-compiler-publish-name) (pathname-file (c-compiler)))
                               software-platform)
                         hardware-platform))

         ;; Publishing directory, without yyyy/mm/dd.
         (publish-base-dir (make-pathname (web-dir) path-layout))

         ;; Date stuff
         (now (seconds->local-time))
         (day (pad-number (vector-ref now 3) 2))
         (month (pad-number (add1 (vector-ref now 4)) 2))
         (year (number->string (+ 1900 (vector-ref now 5))))
         (today-dir (make-pathname (list year month) day))

         ;; Yesterday
         (yesterday (seconds->local-time (- (current-seconds) (* 24 60 60))))
         (yesterday-day (pad-number (vector-ref yesterday 3) 2))
         (yesterday-month (pad-number (add1 (vector-ref yesterday 4)) 2))
         (yesterday-year (number->string (+ 1900 (vector-ref yesterday 5))))
         (yesterday-dir (make-pathname (list yesterday-year yesterday-month)
                                       yesterday-day))
         (yesterday-web-dir (make-absolute-pathname path-layout yesterday-dir))

         ;; Final publishing directory (filesystem), with yyyy/mm/dd
         (publish-dir (make-pathname publish-base-dir today-dir))

         ;; Final web publishing directory, with yyyy/mm/dd
         (publish-web-dir (make-absolute-pathname path-layout today-dir))

         ;; Feeds stuff
         (feeds-dir (make-pathname (list (web-dir) "feeds") path-layout))
         (feeds-web-dir (make-absolute-pathname "feeds" path-layout)))

    (handle-exceptions exn
      (begin
        (print-call-chain (current-error-port))
        (print-error-message exn (current-error-port))
        (publish-results publish-dir)
        (exit 1))
      (begin
        (when (run-salmonella?)
          (run-salmonella))
        (process-results publish-base-dir publish-web-dir today-dir
                         yesterday-dir yesterday-web-dir feeds-dir
                         feeds-web-dir)))

    (publish-results publish-dir)))

) ;; end module
