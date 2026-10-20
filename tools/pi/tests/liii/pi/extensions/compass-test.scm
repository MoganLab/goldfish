(import (liii check)
  (liii path)
  (liii pi extension)
) ;import

(check-set-mode! 'report-failed)

(define (get-compass-path)
  (cond
    ((file-exists? "tools/pi/liii/pi/extensions/compass.scm")
     "tools/pi/liii/pi/extensions/compass.scm")
    ((file-exists? "liii/pi/extensions/compass.scm")
     "liii/pi/extensions/compass.scm")
    (else "../../liii/pi/extensions/compass.scm")
  ) ;cond
) ;define

;; 1. 加载 compass.scm 扩展
(let* ((api (make-extension-api))
       (res (pi-load-extension (get-compass-path) api)))
  (check (pair? res) => #t)
  (check (eq? (cdr (assq 'status res)) 'ok) => #t)

  ;; 2. 验证 flags
  (let ((flag-codemap (find-extension-flag api "--codemap"))
        (flag-refresh (find-extension-flag api "--refresh"))
        (flag-no-inject (find-extension-flag api "--no-codemap-inject"))
        (flag-file (find-extension-flag api "--codemap-file"))
        (flag-tour (find-extension-flag api "--tour")))
    (check (extension-flag-def? flag-codemap) => #t)
    (check (eq? (extension-flag-def-key flag-codemap) 'codemap) => #t)
    (check (extension-flag-def? flag-refresh) => #t)
    (check (eq? (extension-flag-def-key flag-refresh) 'refresh) => #t)
    (check (extension-flag-def? flag-no-inject) => #t)
    (check (eq? (extension-flag-def-key flag-no-inject) 'no-codemap-inject) => #t)
    (check (extension-flag-def? flag-file) => #t)
    (check (eq? (extension-flag-def-key flag-file) 'codemap-file) => #t)
    (check (extension-flag-def? flag-tour) => #t)
    (check (eq? (extension-flag-def-key flag-tour) 'tour) => #t)
  ) ;let

  ;; 3. 验证 tools
  (check (extension-tool-def? (find-extension-tool api "codebase_map")) => #t)
  (check (extension-tool-def? (find-extension-tool api "code_tour")) => #t)

  ;; 4. 验证 flows
  (let ((flow (find-extension-flow api 'compass-local)))
    (check (extension-flow-def? flow) => #t)
    (check (eq? (extension-flow-def-name flow) 'compass-local) => #t)
    (check (extension-flow-def-local? flow) => #t)
  ) ;let

  ;; 5. 验证 prompt injectors
  (let ((injectors (extension-registry-prompt-injectors-list api)))
    (check (pair? injectors) => #t)
  ) ;let

  ;; 6. 验证 augmenters
  (let ((augmenters (extension-registry-augmenters-list api)))
    (check (pair? augmenters) => #t)
  ) ;let
) ;let*

(check-report)
