(import (liii check)
  (liii error)
  (liii string)
  (liii tui text-width)
  (liii pi modes tui message)
  (liii pi modes tui viewport)
) ;import

(check-set-mode! 'report-failed)

;; 1.0 滚动偏移与视口窗口计算
;; 当总行数 <= 视口高度时，不论 offset 是多少，窗口都显示全部
(let-values (((start end) (compute-viewport-window 5 10 0)))
  (check start => 0)
  (check end => 5)
) ;let-values

;; 当总行数 (20) > 视口高度 (10) 时：
;; offset = 0（贴底），显示 [10, 20)
(let-values (((start end) (compute-viewport-window 20 10 0)))
  (check start => 10)
  (check end => 20)
) ;let-values

;; offset = 5（向上滚动 5 行），显示 [5, 15)
(let-values (((start end) (compute-viewport-window 20 10 5)))
  (check start => 5)
  (check end => 15)
) ;let-values

;; offset = 10（向上滚动到顶），显示 [0, 10)
(let-values (((start end) (compute-viewport-window 20 10 10)))
  (check start => 0)
  (check end => 10)
) ;let-values

;; offset 超过最大允许偏移（最大为 20 - 10 = 10），自动 clamp 到 10
(let-values (((start end) (compute-viewport-window 20 10 99)))
  (check start => 0)
  (check end => 10)
) ;let-values

;; 最大可滚动偏移计算
(check (compute-max-scroll-offset 5 10) => 0)
(check (compute-max-scroll-offset 20 10) => 10)
(check (compute-max-scroll-offset 0 10) => 0)

;; 2.0 视口渲染与行截取（render-messages-viewport）
(let* ((msgs (list
               (make-pi-tui-message 'user "User msg 1")
               (make-pi-tui-message 'assistant "Assistant msg 1")
               (make-pi-tui-message 'user "User msg 2")
             ) ;list
       ) ;msgs
       ;; 视口宽 40，高 10，贴底 offset 0
       (rendered (render-messages-viewport msgs 40 10 0))
      ) ;
  (check-true (list? rendered))
  (check (length rendered) => 3)
) ;let

;; 当消息折行后总行数超过视口高度时的截取
(let* ((long-text "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15")
       (msgs (list (make-pi-tui-message 'system long-text)))
       ;; 总行数 15，视口高 5
       ;; 贴底 (offset 0)：截取最后 5 行
       (v-bottom (render-messages-viewport msgs 40 5 0))
       ;; 滚到顶 (offset 10)：截取最前 5 行
       (v-top (render-messages-viewport msgs 40 5 10))
      ) ;
  (check (length v-bottom) => 5)
  (check (length v-top) => 5)
  ;; v-top 第一行应该包含第 1 行的内容
  (check-true (string-contains (car v-top) "1"))
  ;; v-bottom 最后一行应该包含第 15 行的内容
  (check-true (string-contains (list-ref v-bottom 4) "15"))
) ;let

;; 3.0 视口滚动指示器与状态标识
(check (viewport-scroll-indicator 20 10 0) => "") ; 贴底且处于底端无需顶标或为底部
(check (viewport-scroll-indicator 20 10 10) => "[Top]")
(check (viewport-scroll-indicator 20 10 5) => "[50%]")

(check-report)
