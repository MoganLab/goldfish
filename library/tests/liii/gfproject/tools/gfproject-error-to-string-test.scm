;; gfproject-error->string
;; 将工具加载或运行中的异常转换为可读字符串。
;;
;; 语法
;; ----
;; (gfproject-error->string info) -> string
;;
;; 参数
;; ----
;; info : any
;; catch 捕获到的错误参数。
;;
;; 返回值
;; ----
;; string
;; 格式化后的完整错误提示字符串。

(import (liii check)
  (liii string)
  (liii gfproject tools)
) ;import

(check-set-mode! 'report-failed)

;; 1. 标准格式化占位符替换
(check (gfproject-error->string '("value is ~S" 42)) => "value is 42")
(check (gfproject-error->string '("lib ~S missing" (no such lib))) => "lib (no such lib) missing")

;; 2. 非列表参数转换为字符串
(check (gfproject-error->string 42) => "42")
(check (gfproject-error->string "plain") => "plain")

;; 3. 非格式串列表原样打印
(check (gfproject-error->string '(a b)) => "(a b)")

(check-report)
