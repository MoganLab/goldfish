(import (scheme base)
  (liii check)
  (liii date-time)
  (liii string)
  (liii unicode)
  (liii os)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

;; 渲染性能基准：测量逐键输入（模拟打字）与长文本场景下
;; pi-app-handle-input! + pi-app-render! 的每帧耗时。
;; 该测试输出耗时数据，断言只做宽松上限保护（防性能回退），
;; 具体数值供人工分析。

(define (%ms start)
  (exact (round (- (current-monotonic-millis) start)))
) ;define

;; 结果写到系统临时目录，避免在仓库留下垃圾文件
(define (%perf-out name)
  (string-append (os-temp-dir) (string (os-sep)) name)
) ;define

;; 场景一：短文本逐键输入（模拟打字 40 个 ASCII 字符）
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (app msg) #t)))
       (ed (pi-app-editor app))
      ) ;
  (let ((start (current-monotonic-millis)))
    (let loop
      ((i 0))
      (when (< i 40)
        (pi-app-handle-input! app #\a)
        (pi-app-render! app)
        (loop (+ i 1))
      ) ;when
    ) ;let
    (let ((cost (%ms start)))
      (call-with-output-file (%perf-out "render-perf-ascii.txt")
        (lambda (port)
          (display (string-append "typing 40 ascii chars: "
            (number->string cost) " ms total, "
            (number->string (exact (inexact (/ cost 40)))) " ms/key") port))
      )
      ;; 宽松上限：40 键总耗时应远低于 400ms（每键 10ms 内）
      (check-true (< cost 400))
    ) ;let
  ) ;let
) ;let*

;; 场景二：长中文文本行（宽字符 + 长行折行压力）
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (app msg) #t)))
       (ed (pi-app-editor app))
       ;; 100 个中文字符 ≈ 200 显示列，折成约 3 个视觉行
       (long-line (string-append (utf8-make-string 100 #\汉)))
      ) ;
  (editor-set-text! ed long-line)
  (editor-set-cursor! ed 0 100)
  (let ((start (current-monotonic-millis)))
    (let loop
      ((i 0))
      (when (< i 10)
        (pi-app-handle-input! app #\a)
        (pi-app-render! app)
        (loop (+ i 1))
      ) ;when
    ) ;let
    (let ((cost (%ms start)))
      (call-with-output-file (%perf-out "render-perf-cjk.txt")
        (lambda (port)
          (display (string-append "typing into 100-char CJK line (10 keys): "
            (number->string cost) " ms total, "
            (number->string (exact (inexact (/ cost 10)))) " ms/key") port))
      )
      ;; 宽松上限
      (check-true (< cost 500))
    ) ;let
  ) ;let
) ;let*

;; 场景三：长对话记录下输入（视口内容多，frame 计算压力大）
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (app msg) #t)))
       (ed (pi-app-editor app))
      ) ;
  ;; 灌入 50 条长消息卡片
  (let loop
    ((i 0))
    (when (< i 50)
      (pi-app-add-assistant-text! app
        (string-append "消息 " (number->string i) " " (utf8-make-string 200 #\汉))
      ) ;pi-app-add-assistant-text!
      (loop (+ i 1))
    ) ;when
  ) ;let
  (let ((start (current-monotonic-millis)))
    (let loop
      ((i 0))
      (when (< i 10)
        (pi-app-handle-input! app #\a)
        (pi-app-render! app)
        (loop (+ i 1))
      ) ;when
    ) ;let
    (let ((cost (%ms start)))
      (call-with-output-file (%perf-out "render-perf-history.txt")
        (lambda (port)
          (display (string-append "typing with 50 long messages loaded (10 keys): "
            (number->string cost) " ms total, "
            (number->string (exact (inexact (/ cost 10)))) " ms/key") port))
      )
      ;; 宽松上限
      (check-true (< cost 1000))
    ) ;let
  ) ;let
) ;let*

(check-report)
