;; %repl-s7-version-text
;; 返回 S7 Scheme 版本说明文本。
;;
;; 语法
;; ----
;; (%repl-s7-version-text)
;;
;; 参数
;; ----
;; 无
;; 该函数不接受参数。
;;
;; 返回值
;; ----
;; string
;; 版本说明文本。

(import (scheme base) (liii check) (liii string) (liii goldrepl render))

(check-set-mode! 'report-failed)

(let ((text (%repl-s7-version-text)))
  ;; 返回的是字符串
  (check (string? text) => #t)
  ;; 文本中包含 S7 Scheme 版本说明或者 REPL 引擎说明
  (check (or (string-contains text "S7 Scheme")
           (string-contains text "Interactive REPL")
         ) ;or
    =>
    #t
  ) ;check
) ;let

(check-report)
