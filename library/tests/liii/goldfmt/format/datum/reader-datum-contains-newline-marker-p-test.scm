;; reader-datum-contains-newline-marker?
;; 判断 datum 中是否包含换行或注释标记。
;;
;; 语法
;; ----
;; (reader-datum-contains-newline-marker? datum)
;;
;; 参数
;; ----
;; datum : any?
;; 待检查的 datum。
;;
;; 返回值
;; ------
;; boolean?
;; 返回 #t 表示 datum 中包含换行或注释标记（内部 nonce 标记形式）。

(import (liii check) (liii goldfmt format datum) (liii goldfmt marker))

(check-set-mode! 'report-failed)

(check (reader-datum-contains-newline-marker? (list %newline-marker-symbol 1))
  =>
  #t
) ;check
(check (reader-datum-contains-newline-marker? (list 'a (list %newline-marker-symbol 2)))
  =>
  #t
) ;check
(check (reader-datum-contains-newline-marker? '(a b)) => #f)
(check (reader-datum-contains-newline-marker? (vector (list %newline-marker-symbol 1)))
  =>
  #t
) ;check
(check (reader-datum-contains-newline-marker? (vector 'a 'b)) => #f)
;; 用户写的 (*newline* ...) / (*comment* ...) 不算内部标记
(check (reader-datum-contains-newline-marker? (list '*newline* 1)) => #f)
(check (reader-datum-contains-newline-marker? (list 'a (list '*comment* "x")))
  =>
  #f
) ;check

(check-report)
