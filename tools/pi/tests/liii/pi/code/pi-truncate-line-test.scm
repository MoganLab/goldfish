(import (liii check) (liii pi code tools))

(check-set-mode! 'report-failed)

;; pi-truncate-line
;; 针对单行长文本进行最大长度截断。
(let-values (((line truncated?) (pi-truncate-line "hello world")))
  (check line => "hello world")
  (check truncated? => #f))

(let* ((long-line (make-string 600 #\a)))
  (let-values (((line truncated?) (pi-truncate-line long-line)))
    (check (string-length line) => 500)
    (check truncated? => #t)))

(check-report)
