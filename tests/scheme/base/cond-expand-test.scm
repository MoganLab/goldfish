(import (liii check) (scheme base))
(check-set-mode! 'report-failed)

;; cond-expand
;; R7RS 条件展开。
;;
;; 语法
;; ----
;; (cond-expand (feature-requirement body ...) ...)
;;
;; 说明
;; ----
;; feature-requirement 可以是特性符号、and/or/not 组合或 (library ...)。

;; r7rs 特性总是满足
(check (cond-expand (r7rs 42) (else 0)) => 42)

;; and/or/not 组合
(check (cond-expand ((and r7rs r7rs) 1) (else 0)) => 1)
(check (cond-expand ((or bogus-feature r7rs) 2) (else 0)) => 2)
(check (cond-expand ((not bogus-feature) 3) (else 0)) => 3)

;; else 兜底
(check (cond-expand ((bogus-feature) 0) (else 99)) => 99)

;; 用于定义
(cond-expand (r7rs (define ce-defined 5)))
(check ce-defined => 5)

(check-report)
