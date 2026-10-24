(import (liii check)
        (srfi srfi-166 base))

(check-set-mode! 'report-failed)

;; space-to
;; 空白与制表定位组合子。
;;
;; 语法
;; ----
;; (space-to col)
;; (tab-to [tab-width])
;; nl
;; fl
;; nothing
;;
;; 示例
;; ----
;; (show #f "a" (space-to 5) "b") => "a    b"
;; (show #f "abc" (tab-to 5) "def") => "abc  def"

(check (show #f "a" (space-to 5) "b") => "a    b")
(check (show #f "a" (space-to 0) "b") => "ab")
(check (show #f "abc" (tab-to) "def") => "abc     def")
(check (show #f "abc" (tab-to 5) "def") => "abc  def")
(check (show #f "abc" (tab-to 3) "def") => "abcdef")
(check (show #f "abc" nl "def" nl) => "abc\ndef\n")
(check (show #f "abc" fl "def" nl fl) => "abc\ndef\n")
(check (show #f "a" nothing "b") => "ab")

(check-report)
