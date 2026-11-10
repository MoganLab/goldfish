;; hostname?
;; 判断字符串是否为合法的主机名（hostname）。
;;
;; 语法
;; ----
;; (hostname? value)
;;
;; 参数
;; ----
;; value : string?
;; 待判断的字符串。
;;
;; 返回值
;; ----
;; boolean
;; 合法主机名返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 总长 1~253，各段以点分隔、长 1~63，段内仅允许字母、数字
;; 与连字符，且首尾必须为字母或数字；带 xn-- 前缀的段须
;; 全部由 punycode 字母表（字母与数字）组成。

(import (liii check) (liii json schema format-network))

(check-set-mode! 'report-failed)

(check (hostname? "example.com") => #t)
(check (hostname? "localhost") => #t)
(check (hostname? "ex-ample.com") => #t)
(check (hostname? "a1.example.com") => #t)
(check (hostname? "EX-ample.com") => #t)

;; 段首尾不能是连字符、不能有空段
(check (hostname? "-bad.com") => #f)
(check (hostname? "bad-.com") => #f)
(check (hostname? "a..b") => #f)
(check (hostname? "a_b.com") => #f)
(check (hostname? "example.com.") => #f)
(check (hostname? "") => #f)

;; 段长上限 63
(check (hostname? (list->string (make-list 63 #\a))) => #t)
(check (hostname? (list->string (make-list 64 #\a))) => #f)

;; punycode 段：前缀后仅允许字母数字
(check (hostname? "xn--bcherkva.de") => #t)
(check (hostname? "xn--bcher_kva.de") => #f)

(check-report)
