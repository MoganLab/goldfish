;; %pi-app-working-border-row
;; 在缓存帧中定位 Working 边框行（编辑器上边框，busy 时承载 spinner）。
;;
;; 直接读取最近一次整帧布局保存的真实行号，并校验缓存与行号有效；
;; 确保包含多行或多条排队消息时，行号始终准确对齐 Working 分割线。
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
  (liii pi code modes interactive app-queue)
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

;; 4. 多条多行 steering 排队：边框行正确定位在全部排队消息与提示行之后
(let* ((vt (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
      ) ;
  (pi-queue-set-busy! q)
  ;; 插入一条 3 行的 steering 消息与一条单行 steering 消息
  (pi-app-queue-steering! app "第一行插话\n第二行插话\n第三行插话")
  (pi-app-queue-steering! app "第二条单行插话")
  (pi-app-render! app)
  (let* ((row (%pi-app-working-border-row app))
         (lines (map strip-ansi (alt-screen-lines (pi-app-screen app))))
        ) ;
    (check-true (exact-integer? row))
    (check-true (>= row 0))
    (check-true (< row (length lines)))
    (check-true (string-contains? (list-ref lines row) "Working"))
    ;; 确认该行上方紧邻的是撤回提示行或排队卡片，而不是被 Working 误覆盖
    (let ((hint-line (list-ref lines (- row 1))))
      (check-true (or (string-contains? hint-line "Option+Up")
                    (string-contains? hint-line "Alt+Up")
                  ) ;or
      ) ;check-true
    ) ;let
    (check-true (string-contains? (list-ref lines (- row 2)) "第二条单行插话")
    ) ;check-true
    (check-true (string-contains? (list-ref lines (- row 3)) "第三行插话"))
  ) ;let*
) ;let*

(check-report)
