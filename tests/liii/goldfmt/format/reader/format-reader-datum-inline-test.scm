(import (liii check))
(import (liii string) (liii goldfmt format reader) (liii goldfmt format atom))

(check-set-mode! 'report-failed)

;; format-reader-datum-inline
;; 将 datum 强制内联格式化（不换行），超出行宽也不折行。
;;
;; 语法
;; ----
;; (format-reader-datum-inline datum)
;;
;; 参数
;; ----
;; datum : any?
;;
;; 返回值
;; ------
;; string?

;; 测试：普通 list 内联
(check (format-reader-datum-inline '(+ 1 2)) => "(+ 1 2)")

;; 测试：超长 list 也不产生换行
(let ((long-datum
        (apply list (map symbol->string (make-list 50 'abcdef)))))
  (check (string-contains? (format-reader-datum-inline long-datum) "\n") => #f)
) ;let

;; 测试：quote 内联
(check (format-reader-datum-inline (read (open-input-string "'(a b)"))) => "'(a b)")

;; 测试：vector 内联
(check (format-reader-datum-inline #(1 2 3)) => "#(1 2 3)")

(check-report)
