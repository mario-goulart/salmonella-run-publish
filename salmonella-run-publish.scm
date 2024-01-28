;; This program is intended to automate the tasks of building chicken
;; out of git sources, run salmonella on the full set of eggs and
;; generate the HTML reports.  It organizes the salmonella reports in
;; a directory tree like:
;;
;; If a commit is given:
;; web-dir/"commits"/c-compiler/software-platform/hardware-platform/year/month/day/commit-hash
;;
;; If a commit is NOT given:
;; web-dir/chicken-core-branch/c-compiler/software-platform/hardware-platform/year/month/day
;;
;; Examples:
;;
;; If a commit is given:
;; /var/www/salmonella-reports/commits/gcc/linux/x86/2012/01/28/1fc501c164b933e4aa1d8ebc979b948140b0fff6
;;
;; If a commit is NOT given:
;; /var/www/salmonella-reports/master/gcc/linux/x86/2012/01/28


;; The following external tools are required:
;; - salmonella
;; - salmonella-html-report
;; - salmonella-feeds
;; - salmonella-diff
;; - svn (only in non-offline mode)
;; - git (only in non-offline mode)
;; - bzip2
;; - chicken tools (chicken, csi, chicken-install)
;; - graphviz (dot program, for dependencies graphs generation -- salmonella-html-report)
;; - tar
;; - gzip
;; - timeout

;; TODO
;; - substitute system `rm' by some scheme code
;; - notify vandusen

(module salmonella-run-publish-app ()

(import scheme)
(cond-expand
  (chicken-4
   (import (rename chicken (rename-file c4-rename-file)))
   (import (rename setup-api
                   (copy-file c4-copy-file)
                   ;; The following renamed procedures are not used --
                   ;; just to prevent warnings.
                   (move-file c4-move-file)
                   (program-path c4-program-path)
                   (find-program c4-find-program)))
   (use posix utils srfi-1 srfi-13 irregex data-structures ports files extras)
   (use http-client salmonella-run-publish-params)
   (declare (uses chicken-syntax))
   (define copy-file file-copy)
   (define move-file file-move)
   (define (rename-file old new #!optional clobber)
     (c4-rename-file old new))
   (define file-executable? file-execute-access?))
  (chicken-5
   (import (chicken base)
           (chicken condition)
           (chicken file)
           (chicken format)
           (chicken io)
           (chicken pathname)
           (chicken platform)
           (chicken port)
           (chicken process)
           (chicken process-context)
           (chicken string)
           (chicken time)
           (chicken time.posix))
   (import http-client srfi-1 srfi-13)
   (import salmonella-run-publish-params))
  (else
   (error "Unsupported CHICKEN version.")))

(define software-platform (symbol->string (software-version)))

(define hardware-platform (symbol->string (machine-type)))

(define make-platform
  (or (get-environment-variable "PLATFORM")
      (if (string-suffix? "bsd" software-platform)
          "bsd"
          software-platform)))

;; Will be set once `work-dir' is determined
(define log-file (make-parameter #f))

(define (save-excursion dir proc)
  (let ((current-dir (current-directory)))
    (change-directory dir)
    (let ((out (proc)))
      (change-directory current-dir)
      out)))

(define find-program
  (let ((cache '())
        ;; no windows support
        (paths (string-split (get-environment-variable "PATH") ":"))
        (not-found
         (lambda (program)
           (error 'find-program "Program not found" program))))
    (lambda (program)
      (if (absolute-pathname? program)
          (if (and (file-exists? program) (file-executable? program))
              program
              (not-found program))
          (or (and-let* ((path (alist-ref program cache equal?)))
                path)
              (let loop ((paths paths))
                (if (null? paths)
                    (not-found program)
                    (let* ((path (car paths))
                           (program-path (make-pathname path program)))
                      (cond ((and (file-exists? program-path)
                                  (file-executable? program-path))
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

(define salmonella-program
  (let ((prog #f))
    (lambda ()
      (unless prog
        (set! prog
              (if (salmonella-path)
                  (if (instances)
                      (make-pathname (pathname-directory (salmonella-path))
                                     "salmonella-epidemy")
                      (salmonella-path))
                  (program-path
                   (if (instances)
                       "salmonella-epidemy"
                       "salmonella")))))
      prog)))

(define (check-required-programs!)
  (let* ((required-programs
          (append `("bzip2"
                    "dot"
                    "gzip"
                    "tar"
                    ,(salmonella-program)
                    ,(program-path "salmonella-diff")
                    ,(program-path "salmonella-feeds")
                    ,(program-path "salmonella-html-report"))
                  (if (chicken-bootstrap-prefix)
                      '()
                      '("csi"
                        "chicken"))
                  (if (chicken-source-dir)
                      '()
                      (list "git"))
                  (if (salmonella-custom-feeds-dir)
                      '()
                      (list "svn"))
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

(define (current-time)
  (time->string (seconds->local-time)))

(define (report step fmt . args)
  (apply printf (cons (string-append "~a [~a] " fmt "\n")
                      (append (list (current-time) step) args))))

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
  (cond-expand
   (chicken-4
    (map (lambda (var/val)
           (conc (car var/val) "=" (cdr var/val)))
         (append (get-environment-variables)
                 vars/vals)))
   (chicken-5
    (append (get-environment-variables)
            vars/vals))))

(define (! cmd args #!key dir publish-dir (env '()) (abort-on-non-zero? #t) collect-output timeout)
  (let ((args (map ->string args))
        (cwd (and dir (current-directory)))
        (start (current-seconds))
        (cmd-line (string-intersperse (cons cmd (map ->string args)))))
    (when dir
      (change-directory dir)
      (report 'CD (current-directory)))
    (report 'RUN cmd-line)
    (let-values (((in out pid)
		  (if timeout
		      (process (find-program "timeout")
			       (append (list "-v"
					     "-k"
					     (or (and-let* ((t (timeout/kill)))
						   (number->string t))
						 "60")
					     (number->string timeout)
					     (find-program cmd))
				       args)
			       (reuse-environment env))
		      (process (find-program cmd)
			       args
			       (reuse-environment env)))))
      (let ((output
             (if collect-output
                 (with-input-from-port in read-string)
                 (begin
                   (let loop ()
                     (let ((line (read-line in)))
                       (unless (eof-object? line)
                         (printf "~a ~a\n" (current-time) line)
                         (loop))))
                   #f))))
        (let-values (((pid exit-normal? status) (process-wait pid)))
          (close-input-port in)
          (close-output-port out)
          (when dir (change-directory cwd))
          (unless (zero? status)
            (fprintf (current-error-port) "Error executing '~a'.  Exit code: ~a.\n"
                     cmd status)
            (when abort-on-non-zero?
              (error '! (sprintf "Command '~a ~a' exited ~a."
                                 cmd args status))))
          (report 'TIME "~a seconds. Command: ~a"
                  (- (current-seconds) start) cmd-line)
          (cons status output))))))


(define (get-chicken-version chicken-prefix)
  ;; Return the CHICKEN version
  (cdr (! (make-pathname (list chicken-prefix "bin") "csi")
          '(-p
            "(handle-exceptions exn \
               (chicken-version) \
               (eval '(begin (import chicken.platform) (chicken-version))))")
          collect-output: #t)))


(define (fetch-chicken-core chicken-core-dir commit-hash)
  ;; Fetch the the CHICKEN source code from the chicken-core git
  ;; repository.  If commit-hash is non-#f, check out that commit.
  (unless (chicken-source-dir)
    (if (file-exists? chicken-core-dir)
        (begin
          (! "git" '(fetch --all) dir: chicken-core-dir)
          (! "git" `(checkout ,(chicken-core-branch)) dir: chicken-core-dir)
          (! "git" '(clean -f) dir: chicken-core-dir)
          (! "git" '(checkout -f) dir: chicken-core-dir)
          (when commit-hash
            (! "git" `(checkout ,commit-hash) dir: chicken-core-dir)))
        (let ((clone-args
               (append
                `(clone
                  ,(chicken-core-git-uri)
                  --branch ,(chicken-core-branch))
                (if (and (git-clone-depth) (not commit-hash))
                    `(--depth ,(git-clone-depth))
                    '()))))
          (! "git" clone-args dir: (work-dir))
          (when commit-hash
            (! "git" `(checkout ,commit-hash) dir: chicken-core-dir))))))


(define (build-chicken-core chicken-core-dir chicken-prefix commit-hash)
  ;; Build CHICKEN and return its version
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

      (fetch-chicken-core chicken-core-dir commit-hash)

      ((before-make-bootstrap-hook) chicken-core-dir)
      (change-directory chicken-core-dir)

      (let ((common-params
             (lambda (chicken)
               (append
                (list
                 (string-append "PLATFORM=" make-platform)
                 (string-append "PREFIX=" chicken-prefix)
                 (string-append "C_COMPILER=" (c-compiler))
                 (string-append "CXX_COMPILER=" (c++-compiler))
                 (string-append "CHICKEN=" chicken))
                (if (debug-build?)
                    (list "DEBUGBUILD=1")
                    '())
                (if (optimize-for-speed?)
                    (list "OPTIMIZE_FOR_SPEED=1")
                    '()))))
            (build-params
             (if (make-jobs)
                 `(-j ,(make-jobs))
                 '()))
            (make
             (lambda (args #!key timeout)
               (! (make-program) args dir: chicken-core-dir timeout: timeout))))

        ;; cleanup
        (make (append (common-params chicken-bootstrap) '(spotless clean confclean)))

        ;; make boot-chicken
        (make (append (common-params chicken-bootstrap) build-params '(boot-chicken))
	  timeout: (timeout/chicken-build))

        ((after-make-bootstrap-hook) chicken-core-dir)
        (change-directory chicken-core-dir)

        ;; make spotless
        (make (append (common-params "./chicken-boot") '(spotless)))

        ;; make all
        (make (append (common-params "./chicken-boot") build-params '(all))
	  timeout: (timeout/chicken-build))

        ;; make install
        (make (append (common-params "./chicken-boot") '(install))
	  timeout: (timeout/chicken-install))

        ;; make check
        (make (append (common-params "./chicken-boot") '(check))
	  timeout: (timeout/chicken-test))

        ((after-make-check-hook) chicken-prefix)
        (change-directory chicken-core-dir)))))
  (get-chicken-version chicken-prefix))

(define (tweak-setup-defaults chicken-prefix)
  ;; 1. Make a backup copy of the <chicken-prefix>/share/chicken/setup.defaults file
  ;; 2. Tweak setup.defaults (currently only append a `(location ...)' form to it)
  ;; 3. Return a pair (<setup-defaults> . <setup-defaults-backup>) so that the
  ;;    original setup.defaults file can be restored when needed
  (let ((setup-defaults
         (make-pathname (list chicken-prefix "share" "chicken")
                        "setup.defaults"))
        (setup-defaults-backup (create-temporary-file "salmonella-run-publish")))
    (copy-file setup-defaults setup-defaults-backup 'clobber)
    (with-output-to-file setup-defaults
      (lambda ()
        (write `(location ,(eggs-source-dir)))
        (newline))
      append:)
    (cons setup-defaults setup-defaults-backup)))


(define (run-salmonella commit-hash)
  (let ((salmonella-repo-dir (make-pathname (work-dir) "salmonella-repo"))
        (chicken-core-dir (or (chicken-source-dir)
                              (make-pathname (work-dir) "chicken-core")))
        (chicken-prefix (or (pre-built-chicken)
                            (make-pathname (work-dir) "chicken"))))
    ;; Remove previous run data
    (save-excursion (work-dir)
      (lambda ()
        (for-each (lambda (file)
                    (unless (equal? file (pre-built-chicken))
                      (! "rm" `(-rf ,file))))
                  `(,chicken-prefix
                    salmonella.log
                    salmonella.log.bz2
                    salmonella-repo
                    custom-feeds
                    salmonella-report))))

    ;; Build chicken
    (let* ((chicken-version
            (build-chicken-core chicken-core-dir chicken-prefix commit-hash))
           (chicken-major-version
            (string->number (car (string-split chicken-version "."))))
           (restore-setup-defaults #f))

      ;; Run salmonella
      (! "rm" `(-rf ,salmonella-repo-dir))
      (let ((args
             (append
              (if (eggs-source-dir)
                  (case chicken-major-version
                    ((4)
                     (list
                      (string-append
                       "--chicken-install-args="
                       (sprintf "-debug -t local -l ~a -prefix <repo> -test"
                                (qs (eggs-source-dir))))))
                    ((5)
                     (set! restore-setup-defaults
                           (tweak-setup-defaults chicken-prefix))
                     '())
                    (else
                     (error 'run-salmonella "Unsupported CHICKEN major version"
                            chicken-major-version)))
                  '())
              (if (eggs-doc-dir)
                  (list (sprintf "--eggs-doc-dir=~a" (qs (eggs-doc-dir))))
                  '())
              (if (instances)
                  (list (conc "--instances=" (instances)))
                  '())
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
               "--log-file=salmonella.log"
               (string-append "--repo-dir=" salmonella-repo-dir)
               (string-append "--chicken-installation-prefix=" chicken-prefix))
              (map symbol->string ((list-eggs))))))
        (let ((status/output
               (! (salmonella-program) args
                  dir: (work-dir)
                  abort-on-non-zero?: #f
                  publish-dir: (work-dir)
		  timeout: (if (keep-repo?)
			       (timeout/salmonella-cached)
			       (timeout/salmonella-not-cached)))))
          (when restore-setup-defaults
            (move-file (cdr restore-setup-defaults)
                       (car restore-setup-defaults)
                       'clobber))
          status/output)))))

(define yesterday-log-path
  ;; Return the path to the yesterday's log file if it exists; or #f
  ;; if doesn't exist.
  ;;
  ;; The uncompressed log file is stored under (work-dir).
  (let ((log-path 'not-set))
    (lambda (publish-base-dir yesterday-dir)
      (if (eq? log-path 'not-set)
          (let ((yesterday-clog
                 (make-pathname (list publish-base-dir yesterday-dir)
                                "salmonella.log.bz2"))
                (yesterday-clog-tmp
                 (make-pathname (work-dir) "yesterday.log.bz2")))
            (cond ((file-exists? yesterday-clog)
                   (copy-file yesterday-clog
                              yesterday-clog-tmp
                              'clobber)
                   (! "bzip2" `(-d ,yesterday-clog-tmp)
                      dir: (work-dir)
                      abort-on-non-zero?: #f)
                   (set! log-path (make-pathname (work-dir) "yesterday.log")))
                  (else (set! log-path #f))))
          log-path))))


(define (diff publish-base-dir publish-web-dir yesterday-dir yesterday-web-dir)
  (let ((yesterday-log (yesterday-log-path publish-base-dir yesterday-dir))
        (today-log (make-pathname (work-dir) "salmonella.log")))
    (when yesterday-log
      (! (program-path "salmonella-diff")
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
         abort-on-non-zero?: #f
         dir: (work-dir)))))


(define (run-salmonella-html-report)
  ;; Generate the HTML report
  (let ((compressed (if (compress-report?)
                        `(--compress-html --compress-graphics)
                        '())))
    (! (program-path "salmonella-html-report")
       `(,@compressed salmonella.log salmonella-report)
       abort-on-non-zero?: #f
       dir: (work-dir))))


(define (process-results-for-commit)
  (run-salmonella-html-report))


(define (process-results publish-base-dir publish-web-dir today-dir
                         yesterday-dir yesterday-web-dir feeds-dir
                         feeds-web-dir)
  (let ((custom-feeds-dir (or (salmonella-custom-feeds-dir)
                              (make-pathname (work-dir) "custom-feeds"))))
    (unless (salmonella-custom-feeds-dir)
      (create-directory feeds-dir 'with-parents)
      (! "svn" '(co
                 --username=anonymous
                 --password=
                 "https://code.call-cc.org/svn/chicken-eggs/salmonella-custom-feeds"
                 custom-feeds)
         abort-on-non-zero?: #f
         dir: (work-dir)))

    (when (file-exists? (make-pathname (work-dir) "salmonella.log"))
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
           abort-on-non-zero?: #f
           dir: (work-dir)))

      (run-salmonella-html-report)

      ;; Generate diff against yesterday's log (if it exists)
      (diff publish-base-dir publish-web-dir yesterday-dir yesterday-web-dir))))


(define (publish-results publish-dir commit-hash)
  ;; When calling ! here, use abort-on-non-zero?: #f, so that logs get
  ;; published even when something fails.
  (report 'INFO "Publishing results to ~a" publish-dir)
  (create-directory publish-dir 'with-parents)
  (! "bzip2" `(-9 salmonella.log)
     abort-on-non-zero?: #f
     dir: (work-dir))
  (! "gzip" `(-9 -f -S z ,(log-file))
     abort-on-non-zero?: #f
     dir: (work-dir))

  ;; The OpenBSD implementation of gzip appends a `.<suffix>' string
  ;; to filenames when given `-S <suffix>'.  That behavior differs
  ;; from the busybox and GNU implementations.  The code below works
  ;; around the case in OpenBSD.
  (let ((bad-filename (string-append (log-file) ".z")))
    (when (file-exists? bad-filename)
      (rename-file bad-filename (string-append (log-file) "z") 'clobber)))

  (let ((to-publish
         (append
          (list
           (string-append (log-file) "z")
           "salmonella-report"
           "salmonella.log.bz2")
          (if commit-hash
              '()
              (list "yesterday-diff")))))
    (save-excursion (work-dir)
      (lambda ()
        (for-each (lambda (file)
                    (when (file-exists? file)
                      (! "cp" `(-R ,file ,publish-dir)
                         abort-on-non-zero?: #f)))
                  to-publish))))

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
         dir: publish-dir
         abort-on-non-zero?: #f))
    (! "rm" `(-rf "salmonella-report")
       abort-on-non-zero?: #f
       dir: publish-dir)))


(define (usage #!optional exit-code)
  (let* ((port (if (and exit-code (not (zero? exit-code)))
                   (current-error-port)
                   (current-output-port)))
         (prog (pathname-strip-directory (program-name)))
         (msg #<#EOF
Usage: #prog [<options>] <config file> ...

Command line options clobber settings in configuration files.

<options>:

--commit-hash <commit hash>
  If provided #prog will build the code from the chicken-core
  repository at <commit hash>.  Results will be generated under
  the "commits" directory in `(web-dir)'.

--work-dir <directory>
  Directory where salmonella-run-publish will write the files for
  processing before publishing.  Equivalent to the `work-dir`
  parameter.  If the given directory doesn't exist, it will be
  created.

EOF
))
    (fprintf port msg)
    (when exit-code (exit exit-code))))


(define (die . msg)
  (with-output-to-port (current-error-port)
    (lambda ()
      (for-each display msg)
      (newline)
      (flush-output)))
  (exit 1))



(let ((commit-hash #f)
      (%work-dir #f)
      (config-files '()))

  (let loop ((args (command-line-arguments)))
    (unless (null? args)
      (let ((arg (car args)))
        (cond ((member arg '("-h" "-help" "--help"))
               (usage 0))
              ((string=? arg "--commit-hash")
               (when (null? (cdr args))
                 (die "--commit-hash: missing argument"))
               (set! commit-hash (cadr args))
               (loop (cddr args)))
              ((string=? arg "--work-dir")
               (when (null? (cdr args))
                 (die "--work-dir: missing argument"))
               (set! %work-dir (cadr args))
               (loop (cddr args)))
              (else
               (set! config-files (append config-files (list arg)))
               (loop (cdr args)))))))

  ;; Load config file if provided
  (for-each load config-files)

  ;; --work-dir clobbers `work-dir` in config files
  (when %work-dir
    (work-dir %work-dir))

  (create-directory (work-dir) 'with-parents)

  (log-file (make-pathname (work-dir) "run-salmonella.log"))

  ;; Specifying a pre-built CHICKEN and a commit hash doesn't make
  ;; sense
  (when (and (pre-built-chicken) commit-hash)
    (die "Both `(pre-build-chicken)' and --commit-hash have been provided."))

  (check-required-programs!)

  ;; Create the tmp dir if it does not exists
  (create-directory (work-dir) 'parents-too)

  ;; Change to the tmp dir
  (change-directory (work-dir))

  (let* ((path-layout
          (make-pathname
           (append (list (if commit-hash
                             "commits"
                             ((branch-publish-transformer) (chicken-core-branch)))
                         (or (c-compiler-publish-name)
                             (pathname-file (c-compiler)))
                         software-platform)
                   (if commit-hash
                       (list hardware-platform)
                       '()))
           (or commit-hash hardware-platform)))

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
        (publish-results publish-dir commit-hash)
        (exit 1))
      (begin
        (when (run-salmonella?)
          (run-salmonella commit-hash))
        (if commit-hash
            (process-results-for-commit)
            (process-results publish-base-dir
                             publish-web-dir
                             today-dir
                             yesterday-dir
                             yesterday-web-dir
                             feeds-dir
                             feeds-web-dir))))

    (publish-results publish-dir commit-hash)))

) ;; end module
