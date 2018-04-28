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
;; - If hanging-process-killer-program is set, salmonella-run-publish
;;   will check if it is available.

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

(define find-program
  (let ((cache '())
        ;; no windows support
        (paths (string-split (get-environment-variable "PATH") ":"))
        (not-found
         (lambda (program)
           (with-output-to-port (current-error-port)
             (lambda ()
               (printf "find-program: error: ~a not found.\n" program)))
           #f)))
    (lambda (program)
      (if (absolute-pathname? program)
          (if (file-execute-access? program)
              program
              (not-found program))
          (or (and-let* ((path (alist-ref program cache equal?)))
                path)
              (let loop ((paths paths))
                (if (null? paths)
                    (not-found program)
                    (let* ((path (car paths))
                           (program-path (make-pathname path program)))
                      (cond ((file-execute-access? program-path)
                             (set! cache (cons (cons program program-path)
                                               cache))
                             program-path)
                            (else (loop (cdr paths))))))))))))

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
                        "chicken"))
                  (if (hanging-process-killer-program)
                      `(,(program-path (hanging-process-killer-program)))
                      '())
                  ))
         (missing-programs (remove find-program required-programs)))
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

(define (reuse-environment vars/vals)
  ;; vars/vals is an alist mapping environment variable names to their
  ;; values.  The bindings get appended to the current environment
  ;; variables/values and converted to a format suitable for
  ;; `process''s ENVIRONMENT-LIST.
  (map (lambda (var/val)
         (conc (car var/val) "=" (cdr var/val)))
       (append (get-environment-variables)
               vars/vals)))

(define (! cmd args #!key dir publish-dir (env '()) output-file)
  (let ((args (map ->string args))
        (cwd (and dir (current-directory))))
    (when dir
      (change-directory dir)
      (debug "@" (current-directory)))
    (debug (string-intersperse (cons cmd args)))
    (let-values (((in out pid)
		  (process (find-program cmd) args (reuse-environment env))))
      (when (and publish-dir (hanging-process-killer-program))
        (system
         (sprintf "~a ~a &"
                  (program-path (hanging-process-killer-program))
                  (string-intersperse
                   (map ->string
                        ((hanging-process-killer-program-args)
                         pid (make-pathname publish-dir
                                            "hanging-processes.log")))))))
      (let ((output (read-all in)))
	(let-values (((pid exit-normal? status) (process-wait pid)))
          (close-input-port in)
          (close-output-port out)
          (debug output)
          (when output-file
            (with-output-to-file output-file (cut display output)))
          (when dir (change-directory cwd))
          (unless (zero? status)
            (fprintf (current-error-port) "Error executing '~a'.  Exit code: ~a.\n"
                     cmd status)
            (exit status))
          (cons status output))))))


(define (build-chicken-core chicken-core-dir chicken-prefix)
  (cond
   ((pre-built-chicken)
    ;; When a pre-built CHICKEN is provided, just run hooks in order
    ((before-make-bootstrap-hook) chicken-core-dir)
    ((after-make-check-hook) chicken-prefix))
   (else
    (let ((chicken-bootstrap
           (if (chicken-bootstrap-prefix)
               (make-pathname (list (chicken-bootstrap-prefix) "bin") "chicken")
               "chicken")))

      ;; Get the most recent version of the chicken-core
      (if (file-exists? chicken-core-dir)
          (begin
            (! "git" `(checkout ,(chicken-core-branch)) dir: chicken-core-dir)
            (! "git" '(pull) dir: chicken-core-dir)
            (! "git" '(clean -f) dir: chicken-core-dir)
            (! "git" '(checkout -f) dir: chicken-core-dir))
          (! "git" `(clone -b ,(chicken-core-branch) ,(chicken-core-git-uri))
             dir: (tmp-dir)))

      ((before-make-bootstrap-hook) chicken-core-dir)

      ;; make boot-chicken
      (! (make-program) `(,(string-append "PLATFORM=" make-platform)
                          ,(string-append "C_COMPILER=" (c-compiler))
                          ,(string-append "CXX_COMPILER=" (c++-compiler))
                          ,(string-append "CHICKEN=" chicken-bootstrap)
                          spotless clean confclean boot-chicken)
         dir: chicken-core-dir)

      ;; make install
      (! (make-program) `(,(string-append "PLATFORM=" make-platform)
                          ,(string-append "C_COMPILER=" (c-compiler))
                          ,(string-append "CXX_COMPILER=" (c++-compiler))
                          ,(string-append "PREFIX=" chicken-prefix)
                          "CHICKEN=./chicken-boot"
                          spotless install)
         dir: chicken-core-dir)

      ;; make check
      (! (make-program) `(,(string-append "PLATFORM=" make-platform)
                          ,(string-append "C_COMPILER=" (c-compiler))
                          ,(string-append "CXX_COMPILER=" (c++-compiler))
                          ,(string-append "PREFIX=" chicken-prefix)
                          ,(string-append "CHICKEN=./chicken-boot")
                          check)
         dir: chicken-core-dir)

      ((after-make-check-hook) chicken-prefix)))))


(define (run-salmonella)
  (let ((salmonella-repo-dir (make-pathname (tmp-dir) "salmonella-repo"))
        (chicken-core-dir (make-pathname (tmp-dir) "chicken-core"))
        (chicken-prefix (or (pre-built-chicken)
                            (make-pathname (tmp-dir) "chicken"))))
    ;; Remove previous run data
    (for-each (lambda (file)
                (unless (equal? file (pre-built-chicken))
                  (! "rm" `(-rf ,file) dir: (tmp-dir))))
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
    (! "rm" `(-rf ,salmonella-repo-dir))
    (let ((env `(("PATH" . ,(sprintf "~a:~a"
                                     (make-pathname chicken-prefix "bin")
                                     (get-environment-variable "PATH")))))
          (args
           (append
            (if (null? (skip-eggs))
                '()
                (list
                 (string-append "--skip-eggs="
                                (string-intersperse
                                 (map ->string (skip-eggs))
                                 ","))))
            (list
             (if (keep-repo?)
                 "--keep-repo"
                 "--clear-chicken-home")
             (string-append "--repo-dir=" salmonella-repo-dir)
             (string-append "--chicken-installation-prefix=" chicken-prefix))
            (map symbol->string ((list-eggs))))))
      (! (or (salmonella-path) (program-path "salmonella")) args
         env: env
         dir: "."
         publish-dir: (tmp-dir)))))

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
                                "salmonella.log.bz2"))
                (yesterday-clog-tmp
                 (make-pathname (tmp-dir) "yesterday.log.bz2")))
            (cond ((file-exists? yesterday-clog)
                   (file-copy yesterday-clog
                              yesterday-clog-tmp
                              'clobber)
                   (! "bzip2" `(-d ,yesterday-clog-tmp) dir: (tmp-dir))
                   (set! log-path (make-pathname (tmp-dir) "yesterday.log")))
                  (else (set! log-path #f))))
          log-path))))


(define (diff publish-base-dir publish-web-dir yesterday-dir yesterday-web-dir)
  (let ((yesterday-log (yesterday-log-path publish-base-dir yesterday-dir))
        (today-log (make-pathname (tmp-dir) "salmonella.log")))
    (when yesterday-log
      (! "salmonella-diff"
	 (append
	  `(--out-dir=yesterday-diff
	    --label1=Yesterday
	    --label2=Today)
	  (if (salmonella-diff-link-mode?)
	      (list
	       (string-append "--report-uri1="
			      (make-pathname yesterday-web-dir
					     "salmonella-report"))
	       (string-append "--report-uri2="
			      (make-pathname publish-web-dir
					     "salmonella-report")))
	      '())
	  (list yesterday-log
		today-log))
         dir: (tmp-dir)))))

(define (process-results publish-base-dir publish-web-dir today-dir
                         yesterday-dir yesterday-web-dir feeds-dir
                         feeds-web-dir)
  (let ((custom-feeds-dir (make-pathname (tmp-dir) "custom-feeds")))
    (create-directory feeds-dir 'with-parents)
    (! "svn" '(co
               --username=anonymous
               --password=
               "https://code.call-cc.org/svn/chicken-eggs/salmonella-custom-feeds"
               custom-feeds)
       dir: (tmp-dir))

    (when (file-exists? (make-pathname (tmp-dir) "salmonella.log"))
      (let ((custom-feeds-web-dir
             (make-absolute-pathname feeds-web-dir "custom")))
        ;; Generate the atom feeds
        (! (program-path "salmonella-feeds")
           `(--log-file=salmonella.log
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
           dir: (tmp-dir)))

      ;; Generate the HTML report
      (let ((compressed (if (compress-report?)
                            `(--compress-html --compress-graphics)
                            '())))
        (! (program-path "salmonella-html-report")
           `(,@compressed salmonella.log salmonella-report)
           dir: (tmp-dir)))

      ;; Generate diff against yesterday's log (if it exists)
      (diff publish-base-dir publish-web-dir yesterday-dir yesterday-web-dir))))


(define (publish-results publish-dir)
  (create-directory publish-dir 'with-parents)
  (! "bzip2" `(-9 salmonella.log) dir: (tmp-dir))
  (! "gzip" `(-9 -f -S z ,(log-file)) dir: (tmp-dir))
  (for-each (lambda (file)
              (when (file-exists? file)
                (! "cp" `(-R ,file ,publish-dir) dir: (tmp-dir))))
            `(,(string-append (log-file) "z")
              "hanging-processes.log"
              "yesterday-diff"
              "salmonella-report"
              "salmonella.log.bz2"))
  (when (create-report-tarball)
    (let ((env
           (case (create-report-tarball)
             ((gzip) '(("GZIP" . "-9")))
             ((bzip2) '(("BZIP2" . "9")))
             (else '())))
          (tar-args
           (case (create-report-tarball)
             ((gzip) 'czf)
             ((bzip2) 'cjf)
             (else 'cf))))
      (! "tar" `(,tar-args
                 ,(string-append "salmonella-report.tar"
                                 (case (create-report-tarball)
                                   ((gzip) ".gz")
                                   ((bzip2) ".bz2")
                                   (else "")))
                 "salmonella-report")
         env: env
         dir: publish-dir))
    (! "rm" `(-rf "salmonella-report") dir: publish-dir)))


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
