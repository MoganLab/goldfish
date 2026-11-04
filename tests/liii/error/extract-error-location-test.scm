;; extract-error-location
;; 从错误表达式或 owlet 错误上下文中提取源码文件路径与行号。
;;
;; 语法
;; ----
;; (extract-error-location info) -> pair or #f
;;
;; 参数
;; ----
;; info : list or any
;; catch 传递给错误处理器的参数。
;;
;; 返回值
;; ----
;; pair or #f
;; 成功时返回 (file-path . line-number)，无法提取时返回 #f。

(import (liii check)
  (liii error)
) ;import

(check-set-mode! 'report-failed)

;; 1. 空参数或非 pair 形式无法提取位置
(check (extract-error-location #f) => #f)
(check (extract-error-location '()) => #f)
(check (extract-error-location '("simple message")) => #f)

;; 2. 非代码表达式结构返回 #f
(check (extract-error-location '("msg ~S ~S" 'a 'b)) => #f)

(check-report)
