;; call-with-dynamic-scope
;; 在动态作用域中调用过程。
;;
;; 语法
;; ----
;; (call-with-dynamic-scope root thunk)
;;
;; 参数
;; ----
;; root : any?
;; 要加入动态作用域的根资源。
;;
;; thunk : procedure
;; 无参过程，在动态作用域内执行。
;;
;; 返回值
;; ----
;; any?
;; thunk 的返回值。
;;
;; 注意
;; ----
;; 动态作用域是栈式的：
;; 过程执行前压入 root，执行后恢复上一层作用域，
;; 即使过程抛出异常也会恢复。
;;
;; 作用域内容影响 $dynamicAnchor 与 $dynamicRef 的解析。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema model) (liii json schema reference))

(check-set-mode! 'report-failed)

(define validator
  (make-validator '(("$dynamicAnchor" . "node") ("const" . 1)) '())
) ;define

(define local-root '(("$dynamicAnchor" . "node") ("const" . 1)))

(define scoped-root '(("$dynamicAnchor" . "node") ("const" . 2)))

;; 过程返回值原样传出。

(check (call-with-dynamic-scope local-root (lambda () 'value)) => 'value)

;; 作用域内的动态锚点解析优先使用作用域中的资源。

(check (call-with-dynamic-scope scoped-root
         (lambda () (resolve-dynamic-reference validator local-root "#node"))
       ) ;call-with-dynamic-scope
  =>
  '(("$dynamicAnchor" . "node") ("const" . 2))
) ;check

;; 作用域在过程结束后被恢复。

(check (resolve-dynamic-reference validator local-root "#node")
  =>
  '(("$dynamicAnchor" . "node") ("const" . 1))
) ;check

;; 嵌套调用按栈式恢复。

(check
  (call-with-dynamic-scope local-root
    (lambda ()
      (call-with-dynamic-scope scoped-root
        (lambda () (resolve-dynamic-reference validator local-root "#node"))
      ) ;call-with-dynamic-scope
    ) ;lambda
  ) ;call-with-dynamic-scope
  =>
  '(("$dynamicAnchor" . "node") ("const" . 2))
) ;check

(check (resolve-dynamic-reference validator local-root "#node")
  =>
  '(("$dynamicAnchor" . "node") ("const" . 1))
) ;check

(check-report)
