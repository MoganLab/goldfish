;; wheel-scroll-set-lines!
;; 设置滚轮加速器的滚动行数配置模式。
;;
;; 语法
;; ----
;; (wheel-scroll-set-lines! acc lines)
;;
;; 参数
;; ----
;; acc : wheel-scroll-accelerator?
;; 滚轮加速器实例。
;;
;; lines : symbol? 或 integer?
;; 滚动模式。可为 'auto 或正整数。
;;
;; 返回值
;; ----
;; symbol? 或 integer?
;; 设置成功后返回传入的 lines。

(import (liii check) (liii tui wheel-scroll))

(check-set-mode! 'report-failed)

(let ((acc (make-wheel-scroll-accelerator)))
  (check (wheel-scroll-set-lines! acc 3) => 3)
  (check (wheel-scroll-lines acc) => 3)
  (check (wheel-scroll-set-lines! acc 'auto) => 'auto)
  (check (wheel-scroll-lines acc) => 'auto)
  (check-catch 'type-error (wheel-scroll-set-lines! acc 0))
  (check-catch 'type-error (wheel-scroll-set-lines! acc -1))
  (check-catch 'type-error (wheel-scroll-set-lines! acc "3"))
  (check-catch 'type-error (wheel-scroll-set-lines! "not-an-acc" 3))
) ;let

(check-report)
