(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; with-input-from-string
;; 从字符串创建输入端口，并在过程体内将其作为当前输入端口。
;;
;; 语法
;; ----
;; (with-input-from-string str thunk)
;;
;; 参数
;; ----
;; str   - 输入字符串
;; thunk - 无参数过程
;;
;; 返回值
;; ------
;; thunk 的返回值。
;;
;; 说明
;; ----
;; with-input-from-string 是 S7 内置函数，但不在 R7RS 标准中定义。
;;
;; 示例
;; ----
;; (with-input-from-string "123" (lambda () (read)))       => 123
;; (with-input-from-string "1 2" (lambda () (list (read) (read)))) => '(1 2)
;; (with-input-from-string "#t" (lambda () (read)))        => #t


(check (with-input-from-string "123" (lambda () (read))) => 123)
(check (with-input-from-string "1 2" (lambda () (list (read) (read)))) => '(1 2))
(check (with-input-from-string "#t" (lambda () (read))) => #t)
(check (with-input-from-string "(+ 1 2)" (lambda () (read))) => '(+ 1 2))


(check-report)
