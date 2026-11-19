;; wheel-scroll-next!
;; 接收一次滚轮滚动事件，更新内部加速状态并计算本次应滚动的行数。
;;
;; 语法
;; ----
;; (wheel-scroll-next! acc direction time-ms)
;;
;; 参数
;; ----
;; acc : wheel-scroll-accelerator?
;; 滚轮加速器实例。
;;
;; direction : integer?
;; 滚动方向，正数表示向上，负数表示向下。
;;
;; time-ms : number?
;; 滚动事件发生的时间戳（毫秒）。
;;
;; 返回值
;; ----
;; integer?
;; 本次事件计算得到的滚动行数（正整数）。

(import (liii check) (liii tui wheel-scroll))

(check-set-mode! 'report-failed)

;; 1. 孤立滚轮事件：每次滚动 1 行
(let ((acc (make-wheel-scroll-accelerator)))
  (check (wheel-scroll-next! acc -1 1000) => 1)
  ;; 间隔超过手势阈值（200ms）视为新手势，仍滚动 1 行
  (check (wheel-scroll-next! acc -1 5000) => 1)
) ;let

;; 2. 快速连续滚动：按事件间隔加速（最多 6 行）
(let ((acc (make-wheel-scroll-accelerator)))
  (check (wheel-scroll-next! acc -1 1000) => 1)
  ;; 20ms 间隔：约 100/20 = 5 行
  (check (wheel-scroll-next! acc -1 1020) => 5)
  ;; 持续 20ms 间隔：平均间隔稳定在 20ms，每次 5 行
  (check (wheel-scroll-next! acc -1 1040) => 5)
) ;let

;; 3. 同一物理刻度的突发事件（< 5ms）不加速
(let ((acc (make-wheel-scroll-accelerator)))
  (check (wheel-scroll-next! acc -1 1000) => 1)
  (check (wheel-scroll-next! acc -1 1002) => 1)
  (check (wheel-scroll-next! acc -1 1004) => 1)
) ;let

;; 4. 方向切换视为新手势
(let ((acc (make-wheel-scroll-accelerator)))
  (check (wheel-scroll-next! acc -1 1000) => 1)
  (check (wheel-scroll-next! acc 1 1010) => 1)
) ;let

;; 5. 固定行数模式：始终按固定行数滚动
(let ((acc (make-wheel-scroll-accelerator :lines 3)))
  (check (wheel-scroll-next! acc -1 1000) => 3)
  (check (wheel-scroll-next! acc -1 1010) => 3)
  (check (wheel-scroll-next! acc 1 1020) => 3)
) ;let

;; 6. 浮点时间戳与异常参数检查
(let ((acc (make-wheel-scroll-accelerator)))
  (check (integer? (wheel-scroll-next! acc -1 1000.5)) => #t)
  (check-catch 'type-error (wheel-scroll-next! 123 -1 1000))
  (check-catch 'type-error (wheel-scroll-next! acc "x" 1000))
) ;let

(check-report)
