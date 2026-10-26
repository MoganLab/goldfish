(import (liii check)
  (liii path)
  (liii pi code core extensions)
) ;import

(check-set-mode! 'report-failed)

(define (get-review-path)
  (cond
    ((file-exists? "tools/pi/extensions/code-review.scm")
     "tools/pi/extensions/code-review.scm")
    ((file-exists? "extensions/code-review.scm")
     "extensions/code-review.scm")
    (else "../../extensions/code-review.scm")
  ) ;cond
) ;define

;; 1. 加载 code-review.scm 扩展
(let* ((api (make-extension-api))
       (res (pi-load-extension (get-review-path) api)))
  (check (pair? res) => #t)
  (check (eq? (cdr (assq 'status res)) 'ok) => #t)

  ;; 2. 验证 flags
  (let ((flag-rev (find-extension-flag api "--code-review"))
        (flag-auto (find-extension-flag api "--auto-review"))
        (flag-staged (find-extension-flag api "--staged"))
        (flag-ref (find-extension-flag api "--ref")))
    (check (extension-flag-def? flag-rev) => #t)
    (check (extension-flag-def? flag-auto) => #t)
    (check (extension-flag-def? flag-staged) => #t)
    (check (extension-flag-def? flag-ref) => #t)
    (check (extension-flag-def-takes-arg? flag-ref) => #t)
  ) ;let

  ;; 3. 验证 commands
  (check (extension-command-def? (find-extension-command api "/review")) => #t)
  (check (extension-command-def? (find-extension-command api "review")) => #t)
  (check (extension-command-def? (find-extension-command api "/code-review")) => #t)
  (check (extension-command-def? (find-extension-command api "code-review")) => #t)

  ;; 4. 验证 flows
  (let ((flow (find-extension-flow api 'code-review)))
    (check (extension-flow-def? flow) => #t)
    (check (eq? (extension-flow-def-name flow) 'code-review) => #t)
  ) ;let

  ;; 5. 验证 prompt-injectors
  (check (pair? (extension-registry-prompt-injectors-list api)) => #t)

  ;; 6. 验证 validators
  (let ((validators (extension-registry-validators-list api)))
    (check (pair? validators) => #t)
    (let ((v (car validators)))
      ;; 正常合法参数通过
      (check (v '((code-review . #t) (staged . #t))) => (if #f #f))
      ;; --code-review 与 --auto-review 冲突
      (check-catch 'pi-arg-error
        (v '((code-review . #t) (auto-review . #t))))
      ;; --code-review 与 --simplify 冲突
      (check-catch 'pi-arg-error
        (v '((code-review . #t) (simplify . #t))))
      ;; --staged 必须搭配 --code-review 或 --simplify
      (check-catch 'pi-arg-error
        (v '((staged . #t) (code-review . #f) (simplify . #f))))
    ) ;let
  ) ;let
) ;let*

(check-report)
