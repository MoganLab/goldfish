;; jsonc-next-str-state
;; 计算扫描 JSONC 字符串时的下一字符串与转义状态。
;;
;; 语法
;; ----
;; (jsonc-next-str-state c escaped) -> boolean boolean
;;
;; 参数
;; ----
;; c : char; escaped : boolean
;; 当前字符以及上一字符是否触发转义。
;;
;; 返回值
;; ----
;; boolean boolean
;; 分别表示下一字符是否仍在字符串内、是否处于转义状态。
;;

(import (scheme base) (liii check) (liii pi code cli windows-terminal jsonc))

(check-set-mode! 'report-failed)

(define (state c escaped)
  (let-values (((in-str esc) (jsonc-next-str-state c escaped)))
    (list in-str esc)
  ) ;let-values
) ;define

(check (state #\a #f) => '(#t #f))
(check (state #\\ #f) => '(#t #t))
(check (state #\" #f) => '(#f #f))
(check (state #\" #t) => '(#t #f))

(check-report)
