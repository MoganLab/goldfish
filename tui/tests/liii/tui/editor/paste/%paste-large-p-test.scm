;; %paste-large?
;; 判定粘贴内容是否属于大段文本（超过 10 行或超过 1000 字符）。
;;
;; 语法
;; ----
;; (%paste-large? str)
;;
;; 参数
;; ----
;; str : string?
;; 待判定的文本。
;;
;; 返回值
;; ----
;; boolean?
;; 超过阈值返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii tui editor paste))

(check-set-mode! 'report-failed)

(check-false (%paste-large? "1234567890"))
(check-false (%paste-large? (make-string 1000 #\a)))
(check-true (%paste-large? (make-string 1001 #\a)))
(check-false (%paste-large? (string-append "a" (make-string 9 #\newline) "b")))
(check-true (%paste-large? (string-append "a" (make-string 10 #\newline) "b")))

(check-report)
