;; check-rule-suffixes
;; 查询指定规则在配置中声明的 suffix 文件后缀名列表（不含前导点号）。
;;
;; 语法
;; ----
;; (check-rule-suffixes cfg rule-name)
;;
;; 参数
;; ----
;; cfg : json-object? 或 #f
;; 检查配置对象。
;; rule-name : string?
;; 规则名称。
;;
;; 返回值
;; ----
;; list?
;; 规则适用的后缀列表；未配置时回退默认内置后缀列表 '("scm")。

(import (scheme base)
  (liii check)
  (liii json)
  (liii goldcheck config)
) ;import

(check-set-mode! 'report-failed)

;; 1. cfg 为 #f 或未配置 suffix 时返回默认 '("scm")
(check (check-rule-suffixes #f "no-crlf") => '("scm"))
(let ((cfg (string->json "{\"rules\": {\"no-crlf\": {\"enabled\": true}}}")))
  (check (check-rule-suffixes cfg "no-crlf") => '("scm"))
) ;let

;; 2. 读取自定义 suffix 列表
(let* ((cfg (string->json "{\"rules\": {\"no-crlf\": {\"suffix\": [\"scm\", \"json\", \"c\"]}}}"))
       (suffixes (check-rule-suffixes cfg "no-crlf"))
      ) ;let*
  (check suffixes => '("scm" "json" "c"))
) ;let*

(check-report)
