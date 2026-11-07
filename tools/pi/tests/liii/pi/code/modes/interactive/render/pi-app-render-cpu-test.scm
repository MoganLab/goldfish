;; pi-app-render-cpu-test
;; 量化 busy 轮询下整帧渲染的 CPU 开销：
;; 模拟主循环在 busy 状态下 10ms 一拍、每拍收到流式 assistant_text 增量
;; 触发 pi-app-render! 的真实负载，测量 N 拍的墙钟时间与线程 CPU 时间。
;;
;; 背景：gf pi TUI busy 时主循环以 10ms 间隔轮询，流式事件到达即整帧重绘。
;; 本测试用虚拟终端隔离真实 IO，测量纯渲染管线的单帧成本。
;;
;; 语法
;; ----
;; (pi-app-render-cpu-test)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; any
;; 测试用例无返回值；耗时数据打印到标准输出，断言只做性能回退护栏。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii time)
  (liii go)
  (liii list)
  (liii date-time current-time)
  (liii string)
  (liii pi code modes interactive)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
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

;; 1. 单帧渲染成本：空闲 app（无消息）连渲 200 帧
(let* ((vt (make-virtual-terminal :width 120 :height 40))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  (pi-app-render! app)
  (let* ((t0 (current-monotonic-millis))
         (n 200)
         (per-frame-us
           (best-of-3
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
           ) ;best-of-3
         ) ;per-frame-us
         (wall (- (current-monotonic-millis) t0))
        ) ;
    (format #t "空闲 app：~a 帧，墙钟 ~ams，单帧 ~aμs~%" n wall
      per-frame-us
    ) ;format
    ;; 单帧渲染应在毫秒量级以下（性能回归护栏：>5ms/帧即视为异常）
    (check-true (< per-frame-us 5000))
  ) ;let*
) ;let*

;; 2. 流式场景：模拟 300 个 assistant 文本增量事件，每次事件后整帧渲染
;;    （对齐真实 busy 路径：每个流式 chunk → handle-event → render）
(let* ((vt (make-virtual-terminal :width 120 :height 40))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  ;; 预填 6 条历史消息卡片（含长短文本），更接近真实会话
  (pi-app-add-user-message! app "请帮我分析这个函数的性能问题")
  (pi-app-add-assistant-text! app
    (string-join
      (map (lambda (i)
             (string-append "第 "
               (number->string i)
               " 行：这里模拟一段中等长度的助手回复文本，包含中英文混合 content 与若干符号 ⠋ ✓。"
             ) ;string-append
           ) ;lambda
        (iota 30)
      ) ;map
      "\n"
    ) ;string-join
  ) ;pi-app-add-assistant-text!
  (pi-app-add-user-message! app "继续深入分析")
  (pi-app-add-assistant-text! app
    (string-join
      (map (lambda (i)
             (string-append "Reply line "
               (number->string i)
               ": some english text mixed with 中文 for width calculation benchmark."
             ) ;string-append
           ) ;lambda
        (iota 30)
      ) ;map
      "\n"
    ) ;string-join
  ) ;pi-app-add-assistant-text!
  (pi-app-render! app)
  (let* ((t0 (current-monotonic-millis))
         (n 300)
         (per-frame-us
           (best-of-3
             (lambda ()
               (let ((c0 (thread-time-ns)))
                 (let loop
                   ((i 0))
                   (when (< i n)
                     ;; 模拟流式增量：每次追加一小段文本再整帧渲染
                     (pi-app-add-assistant-text! app (string-append "chunk-" (number->string i) " "))
                     (pi-app-render! app)
                     (loop (+ i 1))
                   ) ;when
                 ) ;let
                 (quotient (- (thread-time-ns) c0) (* n 1000))
               ) ;let
             ) ;lambda
           ) ;best-of-3
         ) ;per-frame-us
         (wall (- (current-monotonic-millis) t0))
        ) ;
    (format #t "流式 app：~a 帧增量渲染，墙钟 ~ams，单帧 ~aμs~%"
      n wall per-frame-us
    ) ;format
    ;; 耗时仅作诊断：文本随每轮测量增长，且 CI 并行负载会放大绝对耗时。
    ;; 本测试针对 Working 动画的 CPU 回归护栏见第 5 节。
  ) ;let*
) ;let*

;; 3. 空闲主循环成本：模拟 1000 拍 (poll-events + frame-lines + 无变化渲染)
(let* ((vt (make-virtual-terminal :width 120 :height 40))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  (pi-app-render! app)
  (let* ((t0 (current-monotonic-millis))
         (n 1000)
         (per-tick-us
           (best-of-3
             (lambda ()
               (let ((c0 (thread-time-ns)))
                 (let loop
                   ((i 0))
                   (when (< i n)
                     (pi-app-poll-events! app)
                     (pi-app-render! app)
                     (loop (+ i 1))
                   ) ;when
                 ) ;let
                 (quotient (- (thread-time-ns) c0) (* n 1000))
               ) ;let
             ) ;lambda
           ) ;best-of-3
         ) ;per-tick-us
         (wall (- (current-monotonic-millis) t0))
        ) ;
    (format #t
      "空闲轮询一拍（poll+render 无变化）：~aμs（1000 拍墙钟 ~ams）~%"
      per-tick-us wall
    ) ;format
    ;; 空闲一拍应远小于 10ms 休眠本身
    (check-true (< per-tick-us 10000))
  ) ;let*
) ;let*

;; 4. 分段定位：帧计算 vs 屏幕差异提交 vs 终端尺寸查询
(let* ((vt (make-virtual-terminal :width 120 :height 40))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  (pi-app-render! app)
  (let* ((n 500)
         ;; 4a. terminal-size 单独成本（每帧被调 2~3 次）
         (c0 (thread-time-ns))
         (t (%pi-app-term app))
        ) ;
    (let loop
      ((i 0))
      (when (< i n)
        (terminal-size t)
        (terminal-size t)
        (terminal-size t)
        (loop (+ i 1))
      ) ;when
    ) ;let
    (let ((size-us (quotient (- (thread-time-ns) c0) (* n 1000))))
      (format #t "terminal-size ×3：~aμs/帧~%" size-us)
    ) ;let
    ;; 4b. frame-lines（布局与卡片渲染）单独成本
    (let ((frame-us
            (best-of-3
              (lambda ()
                (let ((c1 (thread-time-ns)))
                  (let loop
                    ((i 0))
                    (when (< i n)
                      (pi-app-frame-lines app)
                      (loop (+ i 1))
                    ) ;when
                  ) ;let
                  (quotient (- (thread-time-ns) c1) (* n 1000))
                ) ;let
              ) ;lambda
            ) ;best-of-3
          ) ;frame-us
         ) ;
      (format #t "pi-app-frame-lines：~aμs/帧~%" frame-us)
      ;; 4c. alt-screen-render!（diff + 提交）单独成本，用固定 frame
      (let*-values (((frame-lines _) (pi-app-frame-lines app)))
        (let* ((screen (pi-app-screen app))
               (alt-us
                 (best-of-3
                   (lambda ()
                     (let ((c2 (thread-time-ns)))
                       (let loop
                         ((i 0))
                         (when (< i n)
                           (alt-screen-render! screen frame-lines)
                           (loop (+ i 1))
                         ) ;when
                       ) ;let
                       (quotient (- (thread-time-ns) c2) (* n 1000))
                     ) ;let
                   ) ;lambda
                 ) ;best-of-3
               ) ;alt-us
              ) ;
          (format #t "alt-screen-render!（无变化帧）：~aμs/帧~%" alt-us)
          ;; 护栏：无变化帧的屏幕提交应 < 2.5ms；帧计算应 < 4ms
          ;; （绝对值随机器而异，取多轮最小值并留出余量，
          ;;   护栏用于拦截数量级回退而非精确计时）
          (check-true (< alt-us 2500))
          (check-true (< frame-us 4000))
        ) ;let*
      ) ;let*-values
    ) ;let
  ) ;let*
) ;let*

;; 5. 真实 busy 主循环节拍：复刻 run.scm 的 ((not k) busy 分支），
;;    连续跑 ~3 秒的 (poll-events + working-tick + sleep 10ms)，
;;    测主循环在无按键、无事件（agent 网络等待）时的自身 CPU 占比
(let* ((vt (make-virtual-terminal :width 120 :height 40))
       (app (make-pi-app :terminal vt :handler (lambda (a m) (sleep 30))))
      ) ;
  (pi-app-submit! app "busy 任务")
  (let* ((c0 (thread-time-ns))
         (t0 (current-monotonic-millis))
         ;; 主循环体副本：poll-events + working-tick + busy 10ms 休眠
         (loop! (lambda () (pi-app-poll-events! app) (pi-app-working-tick! app) (sleep 0.01))
         ) ;loop!
        ) ;
    (let spin
      ()
      (if (< (- (current-monotonic-millis) t0) 3000) (begin (loop!) (spin)) #f)
    ) ;let
    (let* ((c1 (thread-time-ns))
           (cpu-ms (quotient (- c1 c0) 1000000))
           (wall-ms (- (current-monotonic-millis) t0))
           (pct
             (inexact->exact
               (round (* 100 (/ cpu-ms (max 1 wall-ms))))
             ) ;inexact->exact
           ) ;pct
          ) ;
      (format #t
        "busy 主循环节拍（无事件空转 3s）：墙钟 ~ams CPU ~ams ≈ ~a%~%"
        wall-ms cpu-ms pct
      ) ;format
      ;; 护栏：busy 无事件时主循环自身应 < 8%（spinner 12.5fps 的全帧重绘现状约 5~7%）
      (check-true (< pct 8))
    ) ;let*
  ) ;let*
) ;let*

(check-report)
