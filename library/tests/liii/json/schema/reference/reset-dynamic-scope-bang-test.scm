;; reset-dynamic-scope!
;; 清空当前动态作用域。
;;
;; 语法
;; ----
;; (reset-dynamic-scope!)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; unspecified
;; 返回值没有意义。
;;
;; 注意
;; ----
;; 动态作用域是模块级的全局状态，
;; 校验过程中出现异常时可能残留，
;; 复位后动态锚点重新只在当前资源中查找。
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

;; 作用域内动态锚点解析到作用域中的资源。

(check (call-with-dynamic-scope scoped-root
         (lambda () (resolve-dynamic-reference validator local-root "#node"))
       ) ;call-with-dynamic-scope
  =>
  '(("$dynamicAnchor" . "node") ("const" . 2))
) ;check

;; 复位后作用域被清空。

(reset-dynamic-scope!)

(check (resolve-dynamic-reference validator local-root "#node")
  =>
  '(("$dynamicAnchor" . "node") ("const" . 1))
) ;check

;; 复位可以重复调用。

(reset-dynamic-scope!)
(reset-dynamic-scope!)

(check (resolve-dynamic-reference validator local-root "#node")
  =>
  '(("$dynamicAnchor" . "node") ("const" . 1))
) ;check

(check-report)
