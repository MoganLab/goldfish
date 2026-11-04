;; display-json-help
;; 输出 gf json 命令的帮助文本。
;;
;; 语法
;; ----
;; (display-json-help port)
;;
;; 参数
;; ----
;; port : output-port?
;; 帮助文本的输出端口。
;;
;; 返回值
;; -----
;; unspecified
;;
;; 说明
;; ----
;; 帮助文本包含用法、选项和位置参数三部分说明。

(set! *load-path* (cons "tools/json" *load-path*))

(import (scheme base) (scheme write) (liii check) (liii string)
  (liii goldjson args))

(check-set-mode! 'report-failed)

(let ((out (open-output-string)))
  (display-json-help out)
  (let ((text (get-output-string out)))
    (check (string-contains? text "Usage: gf json") => #t)
    (check (string-contains? text "Options:") => #t)
    (check (string-contains? text "--indent") => #t)
    (check (string-contains? text "--tab") => #t)
    (check (string-contains? text "--compact") => #t)
    (check (string-contains? text "--sort-keys") => #t)
    (check (string-contains? text "--ensure-ascii") => #t)
    (check (string-contains? text "--json-lines") => #t)
    (check (string-contains? text "--validate") => #t)
    (check (string-contains? text "--help") => #t)
    (check (string-contains? text "Arguments:") => #t)
    (check (string-contains? text "INFILE") => #t)
    (check (string-contains? text "OUTFILE") => #t)
  ) ;let
) ;let

(check-report)
