(import (liii check) (liii tui wheel-scroll))

(check-set-mode! 'report-failed)

;; make-wheel-scroll-accelerator 与属性访问
(let ((acc (make-wheel-scroll-accelerator)))
  (check (wheel-scroll-accelerator? acc) => #t)
  (check (wheel-scroll-lines acc) => 'auto)
  (check-catch 'type-error (make-wheel-scroll-accelerator :lines 0))
  (check-catch 'type-error (make-wheel-scroll-accelerator :lines "x"))
  (check-catch 'type-error (make-wheel-scroll-accelerator :bogus 1))
) ;let

;; 孤立滚轮事件：每次滚动 1 行
(let ((acc (make-wheel-scroll-accelerator)))
  (check (wheel-scroll-next! acc -1 1000) => 1)
  ;; 间隔超过手势阈值（200ms）视为新手势，仍滚动 1 行
  (check (wheel-scroll-next! acc -1 5000) => 1)
) ;let

;; 快速连续滚动：按事件间隔加速（最多 6 行）
(let ((acc (make-wheel-scroll-accelerator)))
  (check (wheel-scroll-next! acc -1 1000) => 1)
  ;; 20ms 间隔：约 100/20 = 5 行
  (check (wheel-scroll-next! acc -1 1020) => 5)
  ;; 持续 20ms 间隔：平均间隔稳定在 20ms，每次 5 行
  (check (wheel-scroll-next! acc -1 1040) => 5)
) ;let

;; 同一物理刻度的突发事件（< 5ms）不加速
(let ((acc (make-wheel-scroll-accelerator)))
  (check (wheel-scroll-next! acc -1 1000) => 1)
  (check (wheel-scroll-next! acc -1 1002) => 1)
  (check (wheel-scroll-next! acc -1 1004) => 1)
) ;let

;; 方向切换视为新手势
(let ((acc (make-wheel-scroll-accelerator)))
  (check (wheel-scroll-next! acc -1 1000) => 1)
  (check (wheel-scroll-next! acc 1 1010) => 1)
) ;let

;; 固定行数模式：始终按固定行数滚动
(let ((acc (make-wheel-scroll-accelerator :lines 3)))
  (check (wheel-scroll-lines acc) => 3)
  (check (wheel-scroll-next! acc -1 1000) => 3)
  (check (wheel-scroll-next! acc -1 1010) => 3)
  (check (wheel-scroll-next! acc 1 1020) => 3)
) ;let

;; 切换模式与复位
(let ((acc (make-wheel-scroll-accelerator)))
  (check (wheel-scroll-next! acc -1 1000) => 1)
  (check (wheel-scroll-set-lines! acc 2) => 2)
  (check (wheel-scroll-next! acc -1 1010) => 2)
  ;; 切回自动模式并复位手势状态：下一个事件按新手势处理
  (check (wheel-scroll-set-lines! acc 'auto) => 'auto)
  (check (wheel-scroll-reset! acc) => #t)
  (check (wheel-scroll-next! acc -1 1020) => 1)
) ;let

;; 类型检查
(let ((acc (make-wheel-scroll-accelerator)))
  (check-catch 'type-error (wheel-scroll-next! 123 -1 1000))
  (check-catch 'type-error (wheel-scroll-next! acc "x" 1000))
  (check-catch 'type-error (wheel-scroll-lines "not-an-acc"))
) ;let

;; 浮点时间戳参与加速计算时，也必须返回精确整数
(let* ((acc (make-wheel-scroll-accelerator))
       (first (wheel-scroll-next! acc 1 1000.0))
       (accelerated (wheel-scroll-next! acc 1 1010.0))
      ) ;
  (check first => 1)
  (check (and (exact? accelerated) (= accelerated 6)) => #t)
) ;let*

;; 加速路径返回的行数必须始终是精确整数（否则滚动会因 delta 非整数而报错）
(let ((bad '()))
  (for-each
    (lambda (g1)
      (for-each
        (lambda (g2)
          (let* ((acc (make-wheel-scroll-accelerator))
                 (r1 (wheel-scroll-next! acc 1 1000))
                 (r2 (wheel-scroll-next! acc 1 (+ 1000 g1)))
                 (r3 (wheel-scroll-next! acc 1 (+ 1000 g1 g2)))
                ) ;
            (for-each
              (lambda (r)
                (unless (and (integer? r) (exact? r))
                  (set! bad (cons (list g1 g2 r) bad))
                ) ;unless
              ) ;lambda
              (list r1 r2 r3)
            ) ;for-each
          ) ;let*
        ) ;lambda
        (list 1 2 3 5 7 10 13 17 20 25 30 40 50 60 80 100 150 199 200 201)
      ) ;for-each
    ) ;lambda
    (list 1 2 3 5 7 10 13 17 20 25 30 40 50 60 80 100 150 199 200 201)
  ) ;for-each
  (check (null? bad) => #t)
) ;let

(check-report)
