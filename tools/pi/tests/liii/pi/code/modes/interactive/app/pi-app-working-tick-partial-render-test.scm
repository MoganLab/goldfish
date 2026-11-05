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
        (liii pi code modes interactive working))

(check-set-mode! 'report)

;; 1. 语义正确性：busy 首拍后强制到期，帧内容与「每拍整帧」一致
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (a m) #t)))
       (screen (pi-app-screen app))
       (q (pi-app-queue app)))
  (pi-app-render! app)
  (pi-queue-set-busy! q)
  ;; 首拍：整帧渲染（⠋ 帧）
  (check (pi-app-working-tick! app) => #t)
  (let ((first-frame (map strip-ansi (alt-screen-lines screen))))
    (check-true (string-contains? (list-ref first-frame 35) "⠋ Working"))
    ;; 强制节拍到期（不手工 advance）：tick 应推进帧 + 刷新
    (working-last-tick-set! (pi-app-working app)
      (- (current-monotonic-millis) 80))
    (check (pi-app-working-tick! app) => #t)
    (let ((second-frame (map strip-ansi (alt-screen-lines screen))))
      ;; 整帧内容应与「每拍整帧」一致：只有边框行变化
      (check (length second-frame) => 40)
      (check-true (string-contains? (list-ref second-frame 35) "⠙ Working"))
      ;; 其余行与首帧相同
      (let loop ((i 0))
        (when (< i 40)
          (when (not (= i 35))
            (check (list-ref second-frame i) => (list-ref first-frame i)))
          (loop (+ i 1)))))))

;; 2. 性能：busy 200 拍连续 tick（真实 80ms 节拍）期间不整帧重算
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (a m) #t)))
       (q (pi-app-queue app)))
  (pi-app-render! app)
  (pi-queue-set-busy! q)
  ;; 首拍（整帧）
  (pi-app-working-tick! app)
  ;; 后续 199 拍：每拍都强制到期并推进帧，模拟真实 80ms 节拍
  (let ((t0 (current-monotonic-millis))
        (c0 (thread-time-ns)))
    (let loop ((i 0))
      (when (< i 199)
        (working-last-tick-set! (pi-app-working app)
          (- (current-monotonic-millis) 80))
        (pi-app-working-tick! app)
        (loop (+ i 1))))
    (let* ((cpu-ms (quotient (- (thread-time-ns) c0) 1000000))
           (wall (- (current-monotonic-millis) t0))
           (per-tick-us (quotient (- (thread-time-ns) c0) (* 199 1000))))
      (format #t "199 拍轻量 tick：CPU ~ams（墙钟 ~ams），平均 ~aμs/拍~%"
        cpu-ms wall per-tick-us)
      ;; 护栏：轻量 tick 平均应 < 1500μs/拍（原路径 ~3800μs，优化后 ~1100μs；
      ;; 后续 P1 优化 alt-screen-render! 的调用开销后可进一步收紧）
      (check-true (< per-tick-us 1500)))))

;; 3. idle 语义保持：tick 返回 #f，不渲染，spinner 复位
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (a m) #t)))
       (q (pi-app-queue app)))
  (pi-app-render! app)
  (pi-queue-set-busy! q)
  (pi-app-working-tick! app)
  (pi-queue-finish-task! q)
  (check (pi-app-working-tick! app) => #f)
  (check (working-frame-index (pi-app-working app)) => 0)
  (check (working-last-tick (pi-app-working app)) => #f))

;; 4. 兜底：busy 但从未整帧渲染过（无缓存帧）→ 回退整帧，不产生错误
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (a m) #t)))
       (q (pi-app-queue app)))
  ;; 不调用 pi-app-render!，直接 busy + tick
  (pi-queue-set-busy! q)
  (check (pi-app-working-tick! app) => #t)
  (check-true
    (string-contains? (list-ref (map strip-ansi (alt-screen-lines (pi-app-screen app))) 35)
      "⠋ Working")))

;; 5. 兜底：选择器激活时回退整帧（行定位不适用）
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (a m) #t)))
       (q (pi-app-queue app)))
  (pi-app-render! app)
  (pi-queue-set-busy! q)
  (pi-app-working-tick! app)
  ;; 激活真实 thinking 选择器（frame-lines 会按选择器布局渲染）
  (%pi-app-active-selector-set! app
    (make-thinking-selector "off" pi-thinking-levels "off"
      (lambda (lvl) #t) (lambda () #t) (lambda (lvl) #t)))
  ;; 强制到期
  (working-last-tick-set! (pi-app-working app)
    (- (current-monotonic-millis) 80))
  (check (pi-app-working-tick! app) => #t))

(check-report)
