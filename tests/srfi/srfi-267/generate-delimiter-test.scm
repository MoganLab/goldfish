;; generate-delimiter
;; 为字符串生成可用的 delimiter。
;;
;; 语法
;; ----
;; (generate-delimiter str)
;;
;; 参数
;; ----
;; str : string?
;; 待表示的字符串内容。
;; 返回值
;; ----
;; string
;; 返回可用于表示 str 的 delimiter。
;; 说明
;; ----
;; 生成的 delimiter 由若干个连字符组成，长度随需要递增。
;; 错误处理
;; ----
;; type-error 当参数不是字符串时抛出。

(import (liii check) (scheme file) (srfi srfi-267))


(check-set-mode! 'report-failed)


(let* ((payload "\"-\" \"--\" \"---\" \"----\"")
       (delimiter (generate-delimiter payload))
      ) ;
  (check (string? delimiter) => #t)
  (check (can-delimit? payload delimiter) => #t)
) ;let*

(let* ((payload "#\"\"a\"\" and \"quotes\"") (delimiter (generate-delimiter payload)))
  (check (string? delimiter) => #t)
  (check (can-delimit? payload delimiter) => #t)
) ;let*

;; 普通字符串使用空 delimiter 即可
(check (generate-delimiter "abc") => "")

;; 错误处理
(check-catch 'type-error (generate-delimiter 1))

(check-report)
