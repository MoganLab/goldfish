;; decode-entity-references-in-string
;; 解码字符串中的全部 HTML 字符实体引用。
;;
;; 语法
;; ----
;; (decode-entity-references-in-string str)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;;
;; 返回值
;; ----
;; string
;; 返回解码后的字符串。

(import (scheme base))
(import (liii check))
(import (liii cmark inline entity))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check (decode-entity-references-in-string "Tom &amp; Jerry &#35;")
  =>
  "Tom & Jerry #"
) ;check

(check-report)
