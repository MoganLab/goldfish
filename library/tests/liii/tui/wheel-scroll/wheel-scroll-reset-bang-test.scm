;; wheel-scroll-reset!
;; 重置加速器的内部手势状态与时间戳。
;;
;; 语法
;; ----
;; (wheel-scroll-reset! acc)
;;
;; 参数
;; ----
;; acc : wheel-scroll-accelerator?
;; 待重置的滚轮加速器实例。
;;
;; 返回值
;; ----
;; #t
;; 重置成功始终返回 #t。

(import (liii check) (liii tui wheel-scroll))

(check-set-mode! 'report-failed)

(let ((acc (make-wheel-scroll-accelerator)))
  (check (wheel-scroll-next! acc -1 1000) => 1)
  (check (wheel-scroll-next! acc -1 1020) => 5)
  (check (wheel-scroll-reset! acc) => #t)
  ;; 重置后下一次滚动视为新手势，返回 1 行
  (check (wheel-scroll-next! acc -1 1030) => 1)
  (check-catch 'type-error (wheel-scroll-reset! "not-an-acc"))
) ;let

(check-report)
