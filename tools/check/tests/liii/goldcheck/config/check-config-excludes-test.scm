(import (scheme base) (liii check) (liii json) (liii goldcheck config))

;; check-config-excludes
;; 从检查配置中解析提取所有的 exclude 路径模式列表（支持纯字符串与带 reason 对象两种形态）。
;;
;; 语法
;; ----
;; (check-config-excludes cfg)
;;
;; 参数
;; ----
;; cfg : json-object 或 #f
;; 检查配置对象。
;;
;; 返回值
;; ------
;; list of string
;; 豁免路径或模式字符串列表。未配置 exclude 时返回空列表 '()。

(check-set-mode! 'report-failed)

;; 1. cfg 为 #f 或未配置 exclude 时返回空列表
(check (check-config-excludes #f) => '())
(check (check-config-excludes (string->json "{}")) => '())

;; 2. 混合字符串与携带 reason 对象的 exclude 列表解析
(let* ((json-text "{\"exclude\": [\"goldfish/srfi/\", {\"path\": \"goldfish/liii/go.scm\", \"reason\": \"协程核心\"}]}"
       ) ;json-text
       (cfg (string->json json-text))
       (ex (check-config-excludes cfg))
      ) ;
  (check (length ex) => 2)
  (check (car ex) => "goldfish/srfi/")
  (check (cadr ex) => "goldfish/liii/go.scm")
) ;let*

(check-report)
