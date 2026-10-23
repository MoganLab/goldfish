(import (liii check))
(import (liii goldfmt format reader) (liii goldfmt format atom))

(check-set-mode! 'report-failed)

;; format-reader-datum-at
;; 将 datum 按 reader 风格格式化为文本，从指定缩进列开始计量。
;;
;; 语法
;; ----
;; (format-reader-datum-at datum indent)
;;
;; 参数
;; ----
;; datum : any?
;; indent : integer? 起始缩进列
;;
;; 返回值
;; ------
;; string?

;; 测试：短 datum 内联不受缩进影响
(check (format-reader-datum-at '(+ 1 2) 0) => "(+ 1 2)")

;; 测试：缩进导致长 list 换行
(let ((text (format-reader-datum-at '(list aaaa bbbb cccc dddd eeee ffff gggg hhhh iiii jjjj kkkk aaaa bbbb cccc dddd eeee ffff gggg hhhh) 10)))
  (check (string-contains-newline? text) => #t)
) ;let

;; 测试：quote 在缩进下仍带 '
(check (format-reader-datum-at (read (open-input-string "'(a b)")) 5) => "'(a b)")

(check-report)
