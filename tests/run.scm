(import scheme)
(cond-expand
  (chicken-4
   (import chicken)
   (use files posix utils)
   (use salmonella-run-publish-params)
   (use http-client simple-sha1 test))
  (chicken-5
   (import (chicken base))
   (import salmonella-run-publish-params)
   (import http-client simple-sha1 test))
  (else
   (error "Unsupported CHICKEN version.")))


(load "test.conf")

(define-record log uri sum)

(define log1
  (make-log
   "http://salmonella-linux-x86-64.call-cc.org/master-debugbuild/gcc/linux/x86-64/2014/11/26/salmonella.log.bz2"
   "c0d33872db0c9cf3d652e4f35aed1840c10bfb1b"))

(define log2
  (make-log
   "http://salmonella-linux-x86-64.call-cc.org/master-debugbuild/gcc/linux/x86-64/2014/11/27/salmonella.log.bz2"
   "62602b355c4fc0c20fd8bf55670d0c6471dca986"))

(define software-platform (symbol->string (software-version)))

(define hardware-platform (symbol->string (machine-type)))

(define (! cmd #!optional dir)
  (let ((cmd (string-intersperse (map ->string cmd)))
        (cwd (and dir (current-directory))))
    (when dir
      (change-directory dir)
      (info "@~a" dir))
    (info cmd)
    (let* ((p (open-input-pipe (sprintf "~A 2>&1" cmd)))
           (output (read-all p))
           (exit-status (arithmetic-shift (close-input-pipe p) -8)))
      (info output)
      (when dir (change-directory cwd))
      (cons exit-status output))))

(define (info fmt . args)
  (apply printf (cons (string-append fmt "\n") args)))

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

(define (cleanup!)
  (info "Cleaning up")
  (when (directory-exists? (web-dir))
    (delete-directory (web-dir) 'recursive))
  (when (file-exists? (web-dir))
    (delete-file (web-dir)))
  (when (directory-exists? (tmp-dir))
    (delete-directory (tmp-dir) 'recursive))
  (when (file-exists? (tmp-dir))
    (delete-file (tmp-dir))))

(define (now)
  (let* ((now (seconds->local-time))
         (day (pad-number (vector-ref now 3) 2))
         (month (pad-number (add1 (vector-ref now 4)) 2))
         (year (number->string (+ 1900 (vector-ref now 5)))))
    (values year month day)))

(define (create-yesterday-dir)
  (let-values (((year month day) (now)))
    (let* ((path-layout
            (make-pathname (list ((branch-publish-transformer) (chicken-core-branch))
                                 (or (c-compiler-publish-name) (pathname-file (c-compiler)))
                                 software-platform)
                           hardware-platform))

           ;; Publishing directory, without yyyy/mm/dd.
           (publish-base-dir (make-pathname (web-dir) path-layout))

           ;; Yesterday
           (yesterday (seconds->local-time (- (current-seconds) (* 24 60 60))))
           (yesterday-day (pad-number (vector-ref yesterday 3) 2))
           (yesterday-month (pad-number (add1 (vector-ref yesterday 4)) 2))
           (yesterday-year (number->string (+ 1900 (vector-ref yesterday 5))))
           (yesterday-dir (make-pathname (list yesterday-year yesterday-month)
                                         yesterday-day))
           (yesterday-publish-dir (make-pathname publish-base-dir yesterday-dir)))
      ;; Create the directory layout expected by salmonella-run-publish,
      ;; as we will not build CHICKEN and run salmonella (we'll just
      ;; download log files from a salmonella machine, to save time).
      (create-directory yesterday-publish-dir 'with-parents)
      yesterday-publish-dir)))

(define (download-log log log-out-dir)
  (info "Downloading ~a" (log-uri log))
  (let ((out-file (make-pathname log-out-dir "salmonella.log.bz2")))
    (with-output-to-file out-file
      (lambda ()
        (display (with-input-from-request (log-uri log) #f read-string))))
    (info "Checking sum of ~a" out-file)
    (let ((out-file-sum (sha1sum out-file)))
      (unless (equal? out-file-sum (log-sum log))
        (error 'download-log
               (sprintf "Invalid check sum for ~a.  Expected ~a, got ~a."
                        out-file (log-sum log) out-file-sum))))))

(define (download-logs yesterday-publish-dir)
  (download-log log1 yesterday-publish-dir)
  (download-log log2 (tmp-dir)))

(define (salmonella-run-publish #!optional with-yesterday?)
  (cleanup!)
  (create-directory (tmp-dir) 'with-parents)
  (if with-yesterday?
      (download-logs (create-yesterday-dir))
      (download-log log2 (tmp-dir)))
  (info "Uncompressing today's log")
  (! '(bzip2 -d salmonella.log.bz2) (tmp-dir))
  (info "Running salmonella-run-publish")
  (system* "salmonella-run-publish test.conf"))


;;; The actual tests
(define results-dir
  (let-values (((year month day) (now)))
    (make-pathname (list "salmonella-reports"
                         "master"
                         "gcc"
                         "linux"
                         "x86-64"
                         year
                         month)
                   day)))

(define feeds-dir "salmonella-reports/feeds/master/gcc/linux/x86-64/")

(define (run-tests #!optional check-yesterday?)
  (for-each (lambda (file)
              (test-assert (sprintf "Checking ~a" file)
                           (file-exists? (make-pathname results-dir file))))
            (append
             '("run-salmonella.logz"
               "salmonella.log.bz2"
               "salmonella-report.tar.gz")
             (if check-yesterday?
                 '("yesterday-diff")
                 '())))

  (test-assert "Checking feeds directory"
               (directory-exists? feeds-dir))

  (when check-yesterday?
      (test-assert "Checking yesterday-diff feed"
               (file-exists? (make-pathname (list feeds-dir "diff")
                                            "yesterday.xml")))))

(test-begin "salmonella-run-publish")

(test-group "Yesterday's results don't exist"
  (salmonella-run-publish)
  (run-tests))

(test-group "Yesterday's results exist"
  (salmonella-run-publish 'with-yesterday)
  (run-tests 'check-yesterday))

(test-end "salmonella-run-publish")

(test-exit)
