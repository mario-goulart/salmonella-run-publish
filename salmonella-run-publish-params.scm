(module salmonella-run-publish-params
 (tmp-dir chicken-bootstrap-prefix log-file chicken-core-git-uri
  chicken-core-branch make-program keep-repo? skip-eggs henrietta-uri
  local-mode? web-dir verbose? compress-report? c-compiler
  branch-publish-transformer)

(import chicken scheme posix files)

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
   (cons 'matlab
         (if (eq? (software-version) 'macosx)
             '(proccpuinfo win32-msgbox)
             '(macosx objc hfs+ osxattr win32-msgbox)))))

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

(define branch-publish-transformer
  (make-parameter
   (lambda (branch)
     branch)))

) ;; end module
