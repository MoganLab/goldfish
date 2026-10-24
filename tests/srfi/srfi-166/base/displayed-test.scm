(import (liii check)
        (srfi srfi-166 base))

(check-set-mode! 'report-failed)

;; displayed
;; 按 display 语义输出对象的组合子。如果是字符串或字符，直接原样输出；如果是过程，作为组合子调用；其它对象按 written 输出。
;;
;; 语法
;; ----
;; (displayed x)
;;
;; 示例
;; ----
;; (show #f (displayed "hi")) => "hi"
;; (show #f (displayed #\a)) => "a"
;; (show #f (displayed 123)) => "123"
;;
;; 另见
;; ----
;; (liii format base) 同名再导出本函数，日常使用更推荐导入该库：
;;   (import (liii format base))
;;   (show #f (displayed "hi"))

(check (show #f (displayed "hi")) => "hi")
(check (show #f (displayed #\a)) => "a")
(check (show #f (displayed 123)) => "123")
(check (show #f (displayed '(1 2))) => "(1 2)")

(check-report)
