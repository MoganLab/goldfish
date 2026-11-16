;; pi-app-frame-lines
;; 计算应用当前帧需要渲染的完整行列表以及终端光标位置。
;;
;; 语法
;; ----
;; (pi-app-frame-lines app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; (values list pair)
;; 返回两个值：当前帧的文本行列表，以及光标坐标 (cons x y)。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii tui)
  (liii pi agent queue)
  (liii pi code modes interactive app)
  (liii pi code modes interactive app-queue)
) ;import

(check-set-mode! 'report-failed)

;; 1. 24 行终端下帧总行数为 24，光标坐标合法
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
      ) ;
  (let-values (((lines cur) (pi-app-frame-lines app)))
    (check (length lines) => 24)
    (check-true (pair? cur))
    (check-true (integer? (car cur)))
    (check-true (integer? (cdr cur)))
  ) ;let-values
) ;let*

;; 2. 排队超长多行后台日志时，24 行终端下帧总行数仍为 24，不遮挡对话区与输入区
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
       (q (pi-app-queue app))
       (huge-log (string-join (map (lambda (i) (string-append "backend log line " (number->string i))) (iota 200)) "\n"))
      ) ;
  (pi-queue-set-busy! q)
  (pi-app-queue-steering! app huge-log)
  (let-values (((lines cur) (pi-app-frame-lines app)))
    (let ((plain (map strip-ansi lines)))
      (check (length lines) => 24)
      (check-true (< (cdr cur) 24))
      (check-true (any (lambda (l) (string-contains? l "Working")) plain))
      (check-true (any (lambda (l) (string-contains? l "more lines")) plain))
    ) ;let
  ) ;let-values
) ;let*

;; 3. 排队大量独立消息时，24 行终端下帧总行数仍为 24，不遮挡对话区与输入区
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
       (q (pi-app-queue app))
      ) ;
  (pi-queue-set-busy! q)
  (for-each (lambda (i) (pi-app-queue-steering! app (string-append "queued item " (number->string i))))
    (iota 30)
  ) ;for-each
  (let-values (((lines cur) (pi-app-frame-lines app)))
    (let ((plain (map strip-ansi lines)))
      (check (length lines) => 24)
      (check-true (< (cdr cur) 24))
      (check-true (any (lambda (l) (string-contains? l "Working")) plain))
      (check-true (any (lambda (l) (string-contains? l "more lines")) plain))
    ) ;let
  ) ;let-values
) ;let*

(check-report)
