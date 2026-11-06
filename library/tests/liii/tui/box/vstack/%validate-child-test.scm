;; %validate-child
;; 校验并规范化单个子组件，字符串自动提升为 make-text 组件。
;;
;; 语法
;; ----
;; (%validate-child child caller)
;;
;; 参数
;; ----
;; child : any
;; 待校验的子组件或字符串。
;;
;; caller : string?
;; 调用方标识。
;;
;; 返回值
;; ----
;; component?
;; 规范化后的子组件实例。

(import (scheme base) (liii check) (liii tui component) (liii tui box vstack))

(check-set-mode! 'report-failed)

(let ((c (make-text "hello")))
  (check (eq? (%validate-child c "test") c) => #t)
) ;let

(let ((c (%validate-child "hello" "test")))
  (check-true (component? c))
) ;let

(check-catch 'type-error (%validate-child 123 "test"))

(check-report)
