;; check-rule-paths
;; 查询指定规则在配置中声明的 path 目录白名单列表。
;;
;; 语法
;; ----
;; (check-rule-paths cfg rule-name)
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
;; 规则适用的目录白名单列表；未配置时返回空列表 '()（表示全仓生效）。

(import (scheme base)
  (liii check)
  (liii json)
  (liii goldcheck config)
) ;import

(check-set-mode! 'report-failed)

;; 1. cfg 为 #f 或未配置 path 时返回空列表 '()
(check (check-rule-paths #f "no-crlf") => '())
(let ((cfg (string->json "{\"rules\": {\"no-crlf\": {\"enabled\": true}}}")))
  (check (check-rule-paths cfg "no-crlf") => '())
) ;let

;; 2. 读取自定义 path 列表
(let* ((cfg (string->json "{\"rules\": {\"no-crlf\": {\"path\": [\"goldfish\", \"tests\"]}}}"))
       (paths (check-rule-paths cfg "no-crlf"))
      ) ;let*
  (check paths => '("goldfish" "tests"))
) ;let*

(check-report)
