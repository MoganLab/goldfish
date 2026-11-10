;; pi-app-working-tick-partial-render-test
;; Working 动画节拍的轻量渲染路径：busy 后续拍不再重算整帧，
;; 只基于上一次整帧渲染的结果替换 Working 边框行。
;;
;; 背景：pi-app-working-tick! 在 busy 时每 80ms 整帧重绘，单帧成本随历史
;; 增长（长会话 ~16ms），导致空闲会话 CPU 稳定 ~20%。优化为：首拍整帧、
;; 后续拍仅刷新 Working 边框行（diff 输出至多一行）。
;;
;; 语义约定：
;;   - busy 首拍：完整整帧渲染（布局可能变化，需要完整重算）
;;   - busy 后续拍（80ms 节拍）：仅替换 Working 边框行，其余行保持
;;   - idle：不渲染并复位 spinner（原语义）
;;   - 尚无缓存帧时（如手工 set-busy! 后未渲染）：回退整帧渲染
;;   - 选择器激活时：回退整帧渲染（编辑器行被选择器替换，行定位不适用）
;;
;; 语法
;; ----
;; (pi-app-working-tick-partial-render-test)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; any
;; 测试用例无返回值；执行结果由 (check-report) 汇总输出。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii time)
  (liii list)
  (liii date-time current-time)
  (liii unicode)
  (liii string)
  (liii pi agent queue)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive render)
  (liii pi code modes interactive thinking)
  (liii pi code modes interactive thinking-selector)
  (liii pi code modes interactive working)
) ;import

(check-set-mode! 'report)

;; 性能测量取多轮最小值：测试运行器默认并行执行多个用例，
;; 单轮测量会被其它用例的 CPU 争用放大，最小值更接近真实单跑成本

(define (best-of-3 measure)
  (let loop
    ((i 0) (best #f))
    (if (< i 3)
      (let ((v (measure)))
        (loop (+ i 1) (if (or (not best) (< v best)) v best))
      ) ;let
      best
    ) ;if
  ) ;let
) ;define

;; 1. 语义正确性：busy 首拍后强制到期，帧内容与「每拍整帧」一致
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (a m) #t)))
       (screen (pi-app-screen app))
       (q (pi-app-queue app))
      ) ;
  (pi-app-render! app)
  (pi-queue-set-busy! q)
  ;; 首拍：整帧渲染（⠋ 帧）
  (check (pi-app-working-tick! app) => #t)
  (let ((first-frame (map strip-ansi (alt-screen-lines screen))))
    (check-true (string-contains? (list-ref first-frame 35) "⠋ Working"))
    ;; 强制节拍到期（不手工 advance）：tick 应推进帧 + 刷新
    (working-last-tick-set! (pi-app-working app) (- (current-monotonic-millis) 80))
    (check (pi-app-working-tick! app) => #t)
    (let ((second-frame (map strip-ansi (alt-screen-lines screen))))
      ;; 整帧内容应与「每拍整帧」一致：只有边框行变化
      (check (length second-frame) => 40)
      (check-true (string-contains? (list-ref second-frame 35) "⠙ Working"))
      ;; 其余行与首帧相同
      (let loop
        ((i 0))
        (when (< i 40)
          (when (not (= i 35))
            (check (list-ref second-frame i) => (list-ref first-frame i))
          ) ;when
          (loop (+ i 1))
        ) ;when
      ) ;let
    ) ;let
  ) ;let
) ;let*

;; 2. 性能：busy 后续拍（真实 80ms 节拍）不整帧重算
;;    绝对耗时随机器性能与 CI 并行负载浮动数倍（Windows runner 上曾超本机 3 倍），
;;    因此护栏改为相对判据：与同一进程实测的整帧成本相比必须明显更便宜
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
      ) ;
  (pi-app-render! app)
  (pi-queue-set-busy! q)
  ;; 首拍（整帧）
  (pi-app-working-tick! app)
  ;; 后续 199 拍：每拍都强制到期并推进帧，模拟真实 80ms 节拍
  (let ((n 199))
    (let* ((measure-ticks
             (lambda ()
               (let ((c0 (thread-time-ns)))
                 (let loop
                   ((i 0))
                   (when (< i n)
                     (working-last-tick-set! (pi-app-working app) (- (current-monotonic-millis) 80))
                     (pi-app-working-tick! app)
                     (loop (+ i 1))
                   ) ;when
                 ) ;let
                 (quotient (- (thread-time-ns) c0) (* n 1000))
               ) ;let
             ) ;lambda
           ) ;measure-ticks
           ;; 对照组：同一 app 的整帧渲染成本（即优化前的每拍路径）
           (measure-frames
             (lambda ()
               (let ((c0 (thread-time-ns)))
                 (let loop
                   ((i 0))
                   (when (< i n)
                     (pi-app-render! app)
                     (loop (+ i 1))
                   ) ;when
                 ) ;let
                 (quotient (- (thread-time-ns) c0) (* n 1000))
               ) ;let
             ) ;lambda
           ) ;measure-frames
           (t0 (current-monotonic-millis))
           (tick-a (best-of-3 measure-ticks))
           (frame-us (best-of-3 measure-frames))
           ;; 交错复测：整帧测量之后再测一次轻量拍，抵消机器整体快慢漂移
           (tick-b (best-of-3 measure-ticks))
           (tick-us (min tick-a tick-b))
           (wall (- (current-monotonic-millis) t0))
          ) ;
      (format #t
        "~a 拍轻量 tick：墙钟 ~ams，轻量 ~aμs/拍（同轮整帧 ~aμs/帧）~%"
        n wall tick-us frame-us
      ) ;format
      ;; 护栏 1：轻量拍须明显便宜于整帧（< 80%）；
      ;; 若退化为每拍整帧重算，比值回到 ~1.0，本断言即失败
      (check-true (< (* tick-us 5) (* frame-us 4)))
      ;; 护栏 2：绝对上限只拦数量级灾难（如拍内出现阻塞 IO），
      ;; 取 80ms 节拍的 1/4，对机器差异保持宽容
      (check-true (< tick-us 20000))
    ) ;let*
  ) ;let
) ;let*

;; 3. idle 语义保持：tick 返回 #f，不渲染，spinner 复位
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
      ) ;
  (pi-app-render! app)
  (pi-queue-set-busy! q)
  (pi-app-working-tick! app)
  (pi-queue-finish-task! q)
  (check (pi-app-working-tick! app) => #f)
  (check (working-frame-index (pi-app-working app)) => 0)
  (check (working-last-tick (pi-app-working app)) => #f)
) ;let*

;; 4. 兜底：busy 但从未整帧渲染过（无缓存帧）→ 回退整帧，不产生错误
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
      ) ;
  ;; 不调用 pi-app-render!，直接 busy + tick
  (pi-queue-set-busy! q)
  (check (pi-app-working-tick! app) => #t)
  (check-true
    (string-contains?
      (list-ref (map strip-ansi (alt-screen-lines (pi-app-screen app))) 35)
      "⠋ Working"
    ) ;string-contains?
  ) ;check-true
) ;let*

;; 5. 兜底：选择器激活时回退整帧（行定位不适用）
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
      ) ;
  (pi-app-render! app)
  (pi-queue-set-busy! q)
  (pi-app-working-tick! app)
  ;; 激活真实 thinking 选择器（frame-lines 会按选择器布局渲染）
  (%pi-app-active-selector-set! app
    (make-thinking-selector "off"
      pi-thinking-levels
      "off"
      (lambda (lvl) #t)
      (lambda () #t)
      (lambda (lvl) #t)
    ) ;make-thinking-selector
  ) ;%pi-app-active-selector-set!
  ;; 强制到期
  (working-last-tick-set! (pi-app-working app) (- (current-monotonic-millis) 80))
  (check (pi-app-working-tick! app) => #t)
) ;let*

(check-report)
