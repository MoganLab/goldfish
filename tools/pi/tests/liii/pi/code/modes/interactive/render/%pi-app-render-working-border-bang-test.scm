;; %pi-app-render-working-border!
;; 轻量 tick 渲染：仅替换缓存帧中的 Working 边框行，不重算整帧布局。
;;
;; busy 期间布局不变（每拍仅 spinner 帧变化），逐行 eq? 命中备用屏快速路径，
;; 避免每拍重算整帧（长会话整帧重算成本可达 ~16ms）。
;;
;; 语法
;; ----
;; (%pi-app-render-working-border! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ----
;; boolean?
;; 已按轻量路径渲染返回 #t；不可用（无缓存帧或选择器激活）返回 #f，
;; 此时调用方应回退整帧渲染。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii unicode)
  (liii string)
  (liii list)
  (liii pi agent queue)
  (liii pi code modes interactive app)
  (liii pi code modes interactive app-queue)
  (liii pi code modes interactive state)
  (liii pi code modes interactive render)
  (liii pi code modes interactive working)
  (liii pi code modes interactive thinking)
  (liii pi code modes interactive thinking-selector)
) ;import

(check-set-mode! 'report-failed)

;; 1. 尚无整帧渲染缓存：返回 #f（调用方回退整帧渲染）
(let ((app (make-pi-app
             :terminal (make-virtual-terminal :width 80 :height 24)
             :handler  (lambda (a m) #t)
           ) ;make-pi-app
      ) ;app
     ) ;
  (check (%pi-app-render-working-border! app) => #f)
) ;let

;; 2. 渲染后：仅替换 Working 边框行，其余行保持不变
(let* ((vt (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
      ) ;
  (pi-app-render! app)
  (pi-queue-set-busy! q)
  (pi-app-render! app)
  (let* ((row (%pi-app-working-border-row app))
         (before (map strip-ansi (alt-screen-lines (pi-app-screen app))))
        ) ;
    ;; 推进 spinner 帧，使边框行内容发生变化
    (working-advance! (pi-app-working app))
    (check (%pi-app-render-working-border! app) => #t)
    (let ((after (map strip-ansi (alt-screen-lines (pi-app-screen app)))))
      (check-true (string-contains? (list-ref after row) "⠙ Working"))
      (let loop
        ((i 0))
        (when (< i (length after))
          (when (not (= i row))
            (check (list-ref after i) => (list-ref before i))
          ) ;when
          (loop (+ i 1))
        ) ;when
      ) ;let
    ) ;let
  ) ;let*
) ;let*

;; 3. 选择器激活时回退整帧渲染：返回 #f
(let* ((vt (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
      ) ;
  (pi-app-render! app)
  (pi-queue-set-busy! q)
  (pi-app-render! app)
  (%pi-app-active-selector-set! app
    (make-thinking-selector "off"
      pi-thinking-levels
      "off"
      (lambda (lvl) #t)
      (lambda () #t)
      (lambda (lvl) #t)
    ) ;make-thinking-selector
  ) ;%pi-app-active-selector-set!
  (check (%pi-app-render-working-border! app) => #f)
) ;let*

;; 4. 多条多行 steering 排队时轻量刷新：只更新唯一的 Working 行，不覆盖排队消息
(let* ((count-working
         (lambda (lines)
           (let loop
             ((ls lines) (c 0))
             (cond ((null? ls) c)
                   ((string-contains? (car ls) "Working") (loop (cdr ls) (+ c 1)))
                   (else (loop (cdr ls) c))
             ) ;cond
           ) ;let
         ) ;lambda
       ) ;count-working
       (vt (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
      ) ;
  (pi-queue-set-busy! q)
  (pi-app-queue-steering! app
    "多行插话第一行\n多行插话第二行\n多行插话第三行"
  ) ;pi-app-queue-steering!
  (pi-app-queue-steering! app "后续单行插话")
  (pi-app-render! app)
  (let* ((row (%pi-app-working-border-row app))
         (before (map strip-ansi (alt-screen-lines (pi-app-screen app))))
        ) ;
    ;; 整帧渲染后确认 Working 恰好只有一条
    (check (count-working before) => 1)
    ;; 推进 spinner 帧，使边框行内容发生变化
    (working-advance! (pi-app-working app))
    (check (%pi-app-render-working-border! app) => #t)
    (let ((after (map strip-ansi (alt-screen-lines (pi-app-screen app)))))
      ;; 1. Working 出现次数恰为一次
      (check (count-working after) => 1)
      ;; 2. 该行即为输入框上边框（row），且内容更新为下一帧 spinner
      (check-true (string-contains? (list-ref after row) "⠙ Working"))
      ;; 3. 排队消息逐行不变且完整保留
      (check-true (string-contains? (list-ref after (- row 5)) "多行插话第一行")
      ) ;check-true
      (check-true (string-contains? (list-ref after (- row 4)) "多行插话第二行")
      ) ;check-true
      (check-true (string-contains? (list-ref after (- row 3)) "多行插话第三行")
      ) ;check-true
      (check-true (string-contains? (list-ref after (- row 2)) "后续单行插话"))
      (let ((hint-line (list-ref after (- row 1))))
        (check-true (or (string-contains? hint-line "Option+Up")
                      (string-contains? hint-line "Alt+Up")
                    ) ;or
        ) ;check-true
      ) ;let
      ;; 4. 除 spinner 所在行外其余行与刷新前完全一致
      (let loop
        ((i 0))
        (when (< i (length after))
          (when (not (= i row))
            (check (list-ref after i) => (list-ref before i))
          ) ;when
          (loop (+ i 1))
        ) ;when
      ) ;let
    ) ;let
  ) ;let*
) ;let*

(check-report)
