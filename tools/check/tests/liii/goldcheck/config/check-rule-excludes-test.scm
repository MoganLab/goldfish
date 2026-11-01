;; check-rule-excludes
;; 从检查配置中解析指定规则的 exclude 路径模式列表（支持纯字符串与带 reason 对象两种形态）。
;;
;; 语法
;; ----
;; (check-rule-excludes cfg rule-name)
;;
;; 参数
;; ----
;; cfg : json-object 或 #f
;; 检查配置对象。
;; rule-name : string
;; 规则名称，如 "max-file-lines"、"max-tree-depth"。
;;
;; 返回值
;; ----
;; list of string
;; 该规则豁免的路径或模式字符串列表。未配置 excludes 或该规则无豁免时返回空列表 '()。

(import (scheme base) (liii check) (liii json) (liii goldcheck config))

(check-set-mode! 'report-failed)

;; 1. cfg 为 #f、空对象或未配置 excludes 时返回空列表
(check (check-rule-excludes #f "max-file-lines") => '())
(check (check-rule-excludes (string->json "{}") "max-file-lines") => '())
(check (check-rule-excludes (string->json "{\"excludes\": {}}") "max-file-lines")
  =>
  '()
) ;check

;; 2. 指定规则未配置豁免时返回空列表
(let ((cfg (string->json "{\"excludes\": {\"max-tree-depth\": [\"goldfish/srfi/\"]}}")
      ) ;cfg
     ) ;
  (check (check-rule-excludes cfg "max-file-lines") => '())
  (check (check-rule-excludes cfg "max-tree-depth") => '("goldfish/srfi/"))
) ;let

;; 3. 混合字符串与携带 reason 对象的 exclude 列表解析
(let* ((json-text "{\"excludes\": {\"max-file-lines\": [\"goldfish/srfi/\", {\"path\": \"goldfish/liii/go.scm\", \"reason\": \"协程核心\"}]}}"
       ) ;json-text
       (cfg (string->json json-text))
       (ex (check-rule-excludes cfg "max-file-lines"))
      ) ;
  (check (length ex) => 2)
  (check (car ex) => "goldfish/srfi/")
  (check (cadr ex) => "goldfish/liii/go.scm")
) ;let*

;; 4. excludes 为非对象或该规则的豁免为非数组时返回空列表
(check (check-rule-excludes (string->json "{\"excludes\": \"not-an-object\"}")
         "max-file-lines"
       ) ;check-rule-excludes
  =>
  '()
) ;check
(let ((cfg (string->json "{\"excludes\": {\"max-file-lines\": \"not-an-array\"}}")))
  (check (check-rule-excludes cfg "max-file-lines") => '())
) ;let

(check-report)
