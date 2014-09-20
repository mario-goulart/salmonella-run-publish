(module salmonella-run-publish-params
 (tmp-dir chicken-bootstrap-prefix log-file chicken-core-git-uri
  chicken-core-branch make-program keep-repo? skip-eggs henrietta-uri
  local-mode? web-dir verbose? compress-report? c-compiler
  branch-publish-transformer c-compiler-publish-name feeds-server
  create-report-tarball salmonella-diff-link-mode? chicken-release)

(import chicken scheme)
(use posix files)

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

(define compress-report?
  (make-parameter #t))

(define c-compiler
  (make-parameter "gcc"))

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
  (make-parameter 4))

) ;; end module
