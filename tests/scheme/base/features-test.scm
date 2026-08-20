(import (liii check) (scheme base))
(check-set-mode! 'report-failed)

;; features
;; 返回当前实现可用的特性列表。
;;
;; 语法
;; ----
;; (features)
;;
;; 返回值
;; ----
;; list

(let ((fs (features)))
  (check (list? fs) => #t)
  ;; R7RS 强制要求 r7rs 特性存在
  (check (not (null? (member 'r7rs fs))) => #t)
) ;let

;; 与 cond-expand 的特性一致
(check (not (null? (member 'r7rs (features)))) => #t)

(check-report)
