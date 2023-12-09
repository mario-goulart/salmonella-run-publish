(module salmonella-run-publish-params
 (work-dir chicken-bootstrap-prefix chicken-core-git-uri
  chicken-core-branch make-program keep-repo? skip-eggs henrietta-uri
  web-dir verbose? compress-report? c-compiler c++-compiler
  branch-publish-transformer c-compiler-publish-name feeds-server
  create-report-tarball salmonella-diff-link-mode? chicken-release
  run-salmonella? salmonella-path
  list-eggs pre-built-chicken instances debug-build? optimize-for-speed?
  git-clone-depth make-jobs

  ;;; Offline mode
  eggs-source-dir eggs-doc-dir salmonella-custom-feeds-dir chicken-source-dir

  ;;; Hooks
  after-make-check-hook
  before-make-bootstrap-hook
  after-make-bootstrap-hook

  ;;; Timeouts
  timeout/kill
  timeout/chicken-build
  timeout/chicken-install
  timeout/chicken-test
  timeout/salmonella-cached
  timeout/salmonella-not-cached
  )

(import scheme)
(cond-expand
  (chicken-4
   (import chicken)
   (use extras posix files)
   (use http-client)
   (define read-list read-file))
  (chicken-5
   (import (chicken base)
           (chicken io)
           (chicken format)
           (chicken pathname)
           (chicken platform)
           (chicken process-context))
   (import http-client))
  (else
   (error "Unsupported CHICKEN version.")))

;;
;; Helper guard procedures
;;
(define (ensure-integer val)
  ;; This procedure actually also accepts #f, not only integers.
  (cond ((not val)
         #f)
        ((integer? val)
         (inexact->exact val))
        (else (error 'ensure-integer "Invalid value" val))))

;;
;; User-configurable parameters
;;
(define work-dir
  ;; Work directory used by this program
  (make-parameter
   (make-pathname (current-directory) "salmonella-run-publish")))

(define chicken-bootstrap-prefix
   ;; Chicken installation prefix to be used to build the bootstrap
   ;; compiler. When `#f', the chicken tools from path are picked.
  (make-parameter #f))

(define chicken-core-git-uri
  ;; This will only be used if chicken-source-dir is #f.
  (make-parameter "git://code.call-cc.org/chicken-core"))

(define chicken-core-branch
  ;; This will only be used if chicken-source-dir is #f.
  (make-parameter "master"))

(define git-clone-depth
  ;; This parameter gets mapped to the --depth git command line option
  ;; when cloning the chicken-core repository.  If #f, --depth will
  ;; not be passed to git.
  ;;
  ;; Note that if this parameter is non-#f, not all branches will be
  ;; fetched, so most likely it won't be feasible to reuse a
  ;; previously cloned copy of the git repository to switch between
  ;; remote branches.
  (make-parameter 1 ensure-integer))

(define make-program
  (make-parameter
   (if (eq? (software-version) 'linux)
       "make"
       "gmake")))

(define instances
  ;; If bound to an integer, use salmonella-epidemy instead of
  ;; salmonella.  The integer number represents the number of
  ;; salmonella instances to run.
  (make-parameter #f))

(define make-jobs
  ;; If bound to an integer, it'll be mapped to the -j option for make
  ;; when building the `all' and `boot-chicken' targets of the
  ;; chicken-core build system (i.e., parallel builds).  Note that
  ;; parallel builds only work starting from 5329d3554 in
  ;; chicken-core.
  (make-parameter #f ensure-integer))

(define keep-repo?
  ;; salmonella's --keep-repo option
  (make-parameter #f))

(define skip-eggs
  ;; salmonella's --skip-eggs
  (make-parameter
   (let ((linux-only '(proccpuinfo udev))
         (macosx-only '(macosx objc hfs+ osxattr))
         (windows-only '(win32-msgbox matlab)))
     (case (software-version)
       ((linux) (append macosx-only windows-only))
       ((bsd) (append linux-only macosx-only windows-only))
       ((macosx) (append linux-only windows-only))
       ((windows) (append linux-only macosx-only))
       (else '())))))

(define henrietta-uri
  (make-parameter "http://code.call-cc.org/cgi-bin/henrietta.cgi"))

(define web-dir
  ;; Directory where to publish the salmonella report
  (make-parameter
   (make-pathname (current-directory) "salmonella-reports")))

(define verbose?
  (make-parameter #t))

(define compress-report?
  (make-parameter #t))

(define c-compiler
  (make-parameter "gcc"))

(define c++-compiler
  (make-parameter "g++"))

(define debug-build?
  (make-parameter #f))

(define optimize-for-speed?
  (make-parameter #f))

(define c-compiler-publish-name
  ;; This parameter can be useful to publish the compiler name without
  ;; the version, for example.  Sometimes you need to set c-compiler
  ;; to, say, gcc-4.6 but want reports to be published under the `gcc'
  ;; name.  So, you just set c-compiler-publish-name to "gcc".
  (make-parameter #f))

(define branch-publish-transformer
  (make-parameter
   (lambda (branch)
     branch)))

(define feeds-server
  (make-parameter "tests.call-cc.org"))

(define create-report-tarball
  (make-parameter #f
                  (lambda (v)
                    ;; #t is equivalent to tar (no compression)
                    (if (or (boolean? v)
                            (memq v '(tar gzip bzip2)))
                        v
                        (error 'create-report-tarball? "Invalid value" v)))))

(define salmonella-diff-link-mode?
  (make-parameter #f))

;; Currently only used to build up the henrietta URI to list eggs
(define chicken-release
  (make-parameter 4 ensure-integer))

(define run-salmonella?
  (make-parameter #t))

(define salmonella-path
  ;; Path to the salmonella binary.  #f means "use salmonella from
  ;; chicken-bootstrap-prefix".  If chicken-bootstrap-prefix is #f,
  ;; use salmonella from $PATH.
  (make-parameter #f))

(define list-eggs
  (make-parameter
   (lambda ()
     (with-input-from-request
      (sprintf "~a?release=~a&list=1" (henrietta-uri) (chicken-release))
      #f
      read-list))))

(define pre-built-chicken
  ;; Path to the installation prefix of a pre-built CHICKEN.  If this
  ;; parameter is not #f, salmonella-run-publish will use the CHICKEN
  ;; installation pointed by the value of this parameter instead of
  ;; building CHICKEN from git.  CHICKEN build-related hooks will be
  ;; just executed in order, without any intermediate steps between
  ;; them.
  ;;
  ;; This parameter is mostly useful to speed-up tests of salmonella
  ;; and/or salmonella-run-publish itself.
  (make-parameter #f))

;;; Hooks
(define before-make-bootstrap-hook
  (make-parameter
   (lambda (chicken-source-dir)
     (void))))

(define after-make-bootstrap-hook
  (make-parameter
   (lambda (chicken-source-dir)
     (void))))

(define after-make-check-hook
  (make-parameter
   (lambda (chicken-installation-prefix)
     (void))))


;;; Offline mode

(define eggs-source-dir
  ;; Directory containing sources of all eggs, in the same layout as
  ;; generated by henrietta-cache (i.e.,
  ;; <eggs-source-dir>/<egg>/<sources>).  If this parameter is set to
  ;; a non-#f value, it will be used as the location where
  ;; chicken-install will get egg sources from.  Spaces in the path
  ;; yielded by this parameter are not supported.
  (make-parameter #f))

(define eggs-doc-dir
  ;; Directory containing documentation of eggs (files named after egg
  ;; names).  If this parameter is set to a non-#f value, it will be
  ;; given as argument to salmonella's --eggs-doc-dir parameter.
  (make-parameter #f))

(define salmonella-custom-feeds-dir
  ;; If non-#f, should yield the the to a directory where salmonella
  ;; custom feeds configuration files can be found.  If #f, custom
  ;; feeds will be retrieved with svn.
  (make-parameter #f))

(define chicken-source-dir
  ;; If non-#f, should yield the the to a directory where the source
  ;; code of CHICKEN can be found.  If #f, the code will be retrieved
  ;; with git.
  (make-parameter #f))


;;; Default timeouts (in seconds)

(define timeout/kill
  ;; Argument to timeout's -k parameter
  (make-parameter 60))

(define timeout/chicken-build
  (make-parameter (* 40 60)))

(define timeout/chicken-install
  (make-parameter (* 5 60)))

(define timeout/chicken-test
  (make-parameter (* 40 60)))

(define timeout/salmonella-cached
  (make-parameter (* 7 60 60)))

(define timeout/salmonella-not-cached
  (make-parameter (* 26 60 60)))


) ;; end module
