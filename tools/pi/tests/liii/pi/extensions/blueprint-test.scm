(import (liii check)
  (liii path)
  (liii pi extension)
) ;import

(check-set-mode! 'report-failed)

(define (get-blueprint-path)
  (cond
    ((file-exists? "tools/pi/liii/pi/extensions/blueprint.scm")
     "tools/pi/liii/pi/extensions/blueprint.scm")
    ((file-exists? "liii/pi/extensions/blueprint.scm")
     "liii/pi/extensions/blueprint.scm")
    (else "../../liii/pi/extensions/blueprint.scm")
  ) ;cond
) ;define

;; 1. 加载 blueprint.scm 扩展
(let* ((api (make-extension-api))
       (res (pi-load-extension (get-blueprint-path) api)))
  (check (pair? res) => #t)
  (check (eq? (cdr (assq 'status res)) 'ok) => #t)

  ;; 2. 验证 flags
  (let ((flag-bp (find-extension-flag api "--blueprint"))
        (flag-status (find-extension-flag api "--plan-status"))
        (flag-next (find-extension-flag api "--plan-next"))
        (flag-verify (find-extension-flag api "--plan-verify"))
        (flag-abandon (find-extension-flag api "--plan-abandon"))
        (flag-approve (find-extension-flag api "--approve"))
        (flag-file (find-extension-flag api "--blueprint-file")))
    (check (extension-flag-def? flag-bp) => #t)
    (check (eq? (extension-flag-def-key flag-bp) 'blueprint) => #t)
    (check (extension-flag-def? flag-status) => #t)
    (check (eq? (extension-flag-def-key flag-status) 'plan-status) => #t)
    (check (extension-flag-def? flag-next) => #t)
    (check (eq? (extension-flag-def-key flag-next) 'plan-next) => #t)
    (check (extension-flag-def? flag-verify) => #t)
    (check (eq? (extension-flag-def-key flag-verify) 'plan-verify) => #t)
    (check (extension-flag-def? flag-abandon) => #t)
    (check (eq? (extension-flag-def-key flag-abandon) 'plan-abandon) => #t)
    (check (extension-flag-def? flag-approve) => #t)
    (check (eq? (extension-flag-def-key flag-approve) 'approve) => #t)
    (check (extension-flag-def? flag-file) => #t)
    (check (eq? (extension-flag-def-key flag-file) 'blueprint-file) => #t)
  ) ;let

  ;; 3. 验证 tools
  (check (extension-tool-def? (find-extension-tool api "blueprint_update")) => #t)
  (check (extension-tool-def? (find-extension-tool api "blueprint_status")) => #t)
  (check (extension-tool-def? (find-extension-tool api "blueprint_next")) => #t)

  ;; 4. 验证 flows
  (let ((flow-local (find-extension-flow api 'blueprint-local))
        (flow-session (find-extension-flow api 'blueprint-session)))
    (check (extension-flow-def? flow-local) => #t)
    (check (eq? (extension-flow-def-name flow-local) 'blueprint-local) => #t)
    (check (extension-flow-def-local? flow-local) => #t)
    (check (extension-flow-def? flow-session) => #t)
    (check (eq? (extension-flow-def-name flow-session) 'blueprint-session) => #t)
    (check (extension-flow-def-local? flow-session) => #f)
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
