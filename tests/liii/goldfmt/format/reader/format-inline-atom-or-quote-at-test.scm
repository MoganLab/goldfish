(import (liii check))
(import (liii goldfmt format reader) (liii goldfmt format atom))

(check-set-mode! 'report-failed)

;; format-inline-atom-or-quote-at
;; 将 atom 或 quote 形式的值内联格式化，从指定缩进列开始计量。
;;
;; 语法
;; ----
;; (format-inline-atom-or-quote-at value indent)
;;
;; 参数
;; ----
;; value : any?
;; indent : integer?
;;
;; 返回值
;; ------
;; string?

;; 测试：短值不受缩进影响
(check (format-inline-atom-or-quote-at 'abc 10) => "abc")

;; 测试：quote 保持 '
(check (format-inline-atom-or-quote-at (read (open-input-string "'(a b)")) 3) => "'(a b)")

;; 测试：缩进导致长 list 换行
(let ((text (format-inline-atom-or-quote-at '(aaaa bbbb cccc dddd eeee ffff gggg hhhh iiii jjjj kkkk aaaa bbbb cccc dddd eeee ffff gggg hhhh) 10)))
  (check (string-contains-newline? text) => #t)
) ;let

(check-report)
