;; %pi-app-working-border-row
;; 在缓存帧中定位 Working 边框行（编辑器上边框，busy 时承载 spinner）。
;;
;; 行号推导（确定性，无内容猜测）：dock 布局 = 待发送区 + 编辑器 + 菜单 + 页脚，
;; 边框行是编辑器区域的首行，其行号 = 帧长 - dock 长 + 待发送行数。
;;
;; 语法
;; ----
;; (%pi-app-working-border-row app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ----
;; exact-integer? 或 #f
;; 边框行在帧中的行号；缓存无效时为 #f（调用方应回退整帧渲染）。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii unicode)
  (liii string)
  (liii pi agent queue)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive render)
) ;import

(check-set-mode! 'report-failed)

;; 1. 尚无整帧渲染缓存：返回 #f
(let ((app (make-pi-app
             :terminal (make-virtual-terminal :width 80 :height 24)
             :handler  (lambda (a m) #t)
           ) ;make-pi-app
      ) ;app
     ) ;
  (check (%pi-app-working-border-row app) => #f)
) ;let

;; 2. 整帧渲染后：行号落在帧内，且指向编辑器上边框（busy 时为 Working 行）
(let* ((vt (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
      ) ;
  (pi-app-render! app)
  (pi-queue-set-busy! q)
  (pi-app-render! app)
  (let* ((row (%pi-app-working-border-row app))
         (lines (map strip-ansi (alt-screen-lines (pi-app-screen app))))
        ) ;
    (check-true (exact-integer? row))
    (check-true (>= row 0))
    (check-true (< row (length lines)))
    (check-true (string-contains? (list-ref lines row) "Working"))
  ) ;let*
) ;let*

;; 3. 有排队消息时：边框行随待发送区下移，仍指向 Working 行
(let* ((vt (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
      ) ;
  (pi-queue-enqueue! q "排队消息")
  (pi-queue-set-busy! q)
  (pi-app-render! app)
  (let* ((row (%pi-app-working-border-row app))
         (lines (map strip-ansi (alt-screen-lines (pi-app-screen app))))
        ) ;
    (check-true (exact-integer? row))
    (check-true (string-contains? (list-ref lines row) "Working"))
  ) ;let*
) ;let*

(check-report)
