;; parse-extensions
;; 解析逗号分隔的文件后缀名列表，逐个规范化。
;;
;; 语法
;; ----
;; (parse-extensions raw)
;;
;; 参数
;; ----
;; raw : string? 逗号分隔的文件后缀名列表
;;
;; 返回值
;; ------
;; list? 规范化后的后缀名字符串列表

(import (liii check) (liii base) (liii goldfix cli))

(check-set-mode! 'report-failed)

;; 测试：单个后缀名
(check (parse-extensions "scm") => '(".scm"))

;; 测试：逗号分隔的多个后缀名
(check (parse-extensions "scm,sld") => '(".scm" ".sld"))

;; 测试：已带前导点的后缀名保持不变
(check (parse-extensions ".scm,.sld") => '(".scm" ".sld"))

(check-report)
