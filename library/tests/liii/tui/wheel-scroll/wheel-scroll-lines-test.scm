;; wheel-scroll-lines
;; 查询滚轮加速器当前的滚动行数模式配置。
;;
;; 语法
;; ----
;; (wheel-scroll-lines acc)
;;
;; 参数
;; ----
;; acc : wheel-scroll-accelerator?
;; 滚轮加速器实例。
;;
;; 返回值
;; ----
;; symbol? 或 integer?
;; 当前配置的滚动模式，为 'auto 或正整数。

(import (liii check) (liii tui wheel-scroll))

(check-set-mode! 'report-failed)

(let ((acc-auto (make-wheel-scroll-accelerator))
      (acc-fixed (make-wheel-scroll-accelerator :lines 4))
     ) ;
  (check (wheel-scroll-lines acc-auto) => 'auto)
  (check (wheel-scroll-lines acc-fixed) => 4)
  (check-catch 'type-error (wheel-scroll-lines "not-an-acc"))
) ;let

(check-report)
