;; %grep-empty-result
;; 没有匹配结果时的返回文本

;; 语法
;; ----
;; (%grep-empty-result ...)

;; 参数
;; ----
;; 见各断言场景

;; 返回值
;; ------
;; 见各断言场景

(import (liii check) (liii string) (liii pi code core tools grep format))

(check-set-mode! 'report-failed)

;; 1. 没有文件被跳过时仍然是 "No matches found"
(check (%grep-empty-result '()) => "No matches found")

;; 2. 有文件读不出来时不能谎报 "No matches found"（Issue #1189）
(let ((out (%grep-empty-result '("a/b.txt"))))
  (check-false (string=? out "No matches found"))
  (check-true (string-contains? out "skipped"))
  (check-true (string-contains? out "a/b.txt"))
) ;let

(check-report)
