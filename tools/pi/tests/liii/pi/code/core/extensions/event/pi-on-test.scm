;; pi-on!
;; 为指定事件类型注册扩展监听器。
;;
;; 语法
;; ----
;; (pi-on! target event-type handler)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 事件所属的扩展对象。
;;
;; event-type : (or symbol string)
;; 事件名称，字符串会被规范化为符号。
;;
;; handler : procedure
;; 监听器过程。
;;
;; 返回值
;; ----
;; boolean
;; 注册成功返回 #t。

(import (liii check) (liii pi code core extensions))

(check-set-mode! 'report-failed)

;; 1. pi-on! 注册事件监听器
(let* ((api (make-extension-api))
       (reg (extension-api-registry api))
       (h1 (lambda (p c) 1))
       (h2 (lambda (p c) 2))
      ) ;
  (pi-on! api 'session_start h1)
  (pi-on! api 'session_start h2)
  (let ((listeners (extension-registry-get-listeners reg 'session_start)))
    (check (length listeners) => 2)
    (check (eq? (car listeners) h1) => #t)
    (check (eq? (cadr listeners) h2) => #t)
  ) ;let
) ;let*

;; 2. pi-on! 传入 string 事件名称自动规范为 symbol
(let* ((api (make-extension-api))
       (reg (extension-api-registry api))
       (h (lambda (p c) 'started))
      ) ;
  (pi-on! api "agent_start" h)
  (let ((listeners (extension-registry-get-listeners reg 'agent_start)))
    (check (length listeners) => 1)
    (check (eq? (car listeners) h) => #t)
  ) ;let
) ;let*

;; 3. pi-on! 直接在 registry 和 context 上注册
(let* ((reg (make-extension-registry))
       (ctx (make-extension-context :registry reg))
       (h-reg (lambda (p c) 'reg))
       (h-ctx (lambda (p c) 'ctx))
      ) ;
  (pi-on! reg 'tool_result h-reg)
  (pi-on! ctx 'tool_result h-ctx)
  (let ((listeners (extension-registry-get-listeners reg 'tool_result)))
    (check (length listeners) => 2)
    (check (eq? (car listeners) h-reg) => #t)
    (check (eq? (cadr listeners) h-ctx) => #t)
  ) ;let
) ;let*

;; 4. 错误校验：handler 必须是 procedure，event 必须是 symbol 或 string
(let ((api (make-extension-api)))
  (check-catch 'type-error (pi-on! api 'session_start "not-a-procedure"))
  (check-catch 'type-error (pi-on! api 12345 (lambda (p c) #t)))
  (check-catch 'type-error (pi-on! "not-an-api" 'session_start (lambda (p c) #t)))
) ;let

(check-report)
