;; simplify-extension
;; 验证内置 simplify 扩展脚本的加载、flags、commands 与 flows 注册。
;;
;; 语法
;; ----
;; (pi-load-extension path api)
;;
;; 参数
;; ----
;; path : string?
;; 扩展脚本路径。
;;
;; api : extension-api?
;; 扩展宿主 API 实例。
;;
;; 返回值
;; ----
;; list?
;; 扩展加载结果字典。

(import (liii check) (liii path) (liii pi code core extensions))

(check-set-mode! 'report-failed)

(define (get-simplify-path)
  (cond ((file-exists? "tools/pi/extensions/simplify.scm")
         "tools/pi/extensions/simplify.scm"
        ) ;
        ((file-exists? "extensions/simplify.scm") "extensions/simplify.scm")
        (else "../../extensions/simplify.scm")
  ) ;cond
) ;define

;; 1. 加载 simplify.scm 扩展
(let* ((api (make-extension-api)) (res (pi-load-extension (get-simplify-path) api)))
  (check (pair? res) => #t)
  (check (eq? (cdr (assq 'status res)) 'ok) => #t)

  ;; 2. 验证 flags
  (let ((flag-simp (find-extension-flag api "--simplify"))
        (flag-apply (find-extension-flag api "--apply"))
       ) ;
    (check (extension-flag-def? flag-simp) => #t)
    (check (eq? (extension-flag-def-key flag-simp) 'simplify) => #t)
    (check (extension-flag-def? flag-apply) => #t)
    (check (eq? (extension-flag-def-key flag-apply) 'apply) => #t)
  ) ;let

  ;; 3. 验证 commands
  (let ((cmd-slash (find-extension-command api "/simplify"))
        (cmd-plain (find-extension-command api "simplify"))
       ) ;
    (check (extension-command-def? cmd-slash) => #t)
    (check (extension-command-def? cmd-plain) => #t)
  ) ;let

  ;; 4. 验证 flows
  (let ((flow (find-extension-flow api 'simplify)))
    (check (extension-flow-def? flow) => #t)
    (check (eq? (extension-flow-def-name flow) 'simplify) => #t)
  ) ;let

  ;; 5. 验证 validators
  (let ((validators (extension-registry-validators-list api)))
    (check (pair? validators) => #t)
    (let ((v (car validators)))
      ;; 正常合法参数通过
      (check (v '((simplify . #t) (apply . #t))) => (if #f #f))
      ;; --apply requires --simplify
      (check-catch 'pi-arg-error (v '((apply . #t) (simplify . #f))))
      ;; --simplify 与 --code-review 冲突
      (check-catch 'pi-arg-error (v '((simplify . #t) (code-review . #t))))
    ) ;let
  ) ;let
) ;let*

(check-report)
