(import (scheme base)
  (liii check)
  (liii unicode)
  (liii string)
  (liii tui)
  (liii pi agent queue)
  (liii pi code modes interactive working)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report)

;; 1. Working 指示器帧序列与循环推进
(let ((ind (make-working-indicator)))
  (check-true (working-indicator? ind))
  ;; 初始帧为盲文点阵第一帧 ⠋
  (check (working-frame ind) => "⠋")
  ;; 推进一帧为 ⠙
  (working-advance! ind)
  (check (working-frame ind) => "⠙")
  ;; 再推进 9 帧共 10 帧，回到 ⠋
  (for-each (lambda (_) (working-advance! ind)) '(1 2 3 4 5 6 7 8 9))
  (check (working-frame ind) => "⠋")
) ;let

;; 2. working-border-line 渲染格式：── ⠋ Working ──… 填满整行，颜色由调用方注入
(let ((ind (make-working-indicator)))
  (let* ((line (working-border-line ind 100 "\x1b;[38;2;151;118;229m"))
         (plain (strip-ansi line))
         (dashes (utf8-make-string (- 100 13) #\─))
        ) ;
    ;; 纯文本格式：── + 空格 + spinner + 空格 + Working + 空格 + 剩余横线
    (check plain => (string-append "── ⠋ Working " dashes))
    ;; 可见宽度恰为终端宽度
    (check (display-width plain) => 100)
    ;; 使用调用方注入的主题色（spinner/文字/边框线同色）
    (check-true (string-contains? line "\x1b;[38;2;151;118;229m"))
    ;; 复位色尾码
    (check-true (string-contains? line "\x1b;[0m"))
  ) ;let*
  ;; 帧推进后行内容随之变化
  (working-advance! ind)
  (check-true (string-contains? (strip-ansi (working-border-line ind 100 "\x1b;[38;2;151;118;229m")) "⠙"))
) ;let

;; 3. busy 时编辑器上边框嵌入 Working 动画，idle 时恢复普通边框
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (app msg) #t)))
       (screen (pi-app-screen app))
       (q (pi-app-queue app))
      ) ;
  ;; idle 首屏：上边框为纯横线，无 Working 字样
  (pi-app-render! app)
  (let ((plain (map strip-ansi (screen-lines screen))))
    (check (length plain) => 11)
    (check (list-ref plain 6) => (utf8-make-string 100 #\─))
  ) ;let

  ;; 置为 busy：上边框替换为 Working 边框行，总行数不变
  (pi-queue-set-busy! q)
  (pi-app-render! app)
  (let ((plain (map strip-ansi (screen-lines screen))))
    (check (length plain) => 11)
    (check-true (string-contains? (list-ref plain 6) "⠋ Working"))
    (check (display-width (list-ref plain 6)) => 100)
    ;; 光标仍定位在输入行
    (check (terminal-cursor vterm) => '(0 7))
  ) ;let

  ;; 帧推进后 spinner 变化
  (working-advance! (pi-app-working app))
  (pi-app-render! app)
  (check-true (string-contains?
    (list-ref (map strip-ansi (screen-lines screen)) 6) "⠙ Working"))

  ;; 任务完成恢复 idle：上边框回到纯横线
  (pi-queue-finish-task! q)
  (pi-app-render! app)
  (let ((plain (map strip-ansi (screen-lines screen))))
    (check (length plain) => 11)
    (check (list-ref plain 6) => (utf8-make-string 100 #\─))
  ) ;let
) ;let*

;; 4. working tick 节拍：idle 时不渲染并复位；busy 时首拍立即渲染
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (app msg) #t)))
       (screen (pi-app-screen app))
       (q (pi-app-queue app))
      ) ;
  ;; idle：tick 返回 #f 且不产生渲染副作用
  (check (pi-app-working-tick! app) => #f)

  ;; busy：首拍立即渲染（从 ⠋ 起始）
  (pi-queue-set-busy! q)
  (check (pi-app-working-tick! app) => #t)
  (check-true (string-contains?
    (list-ref (map strip-ansi (screen-lines screen)) 6) "⠋ Working"))

  ;; 80ms 内重复 tick：不推进帧不渲染
  (check (pi-app-working-tick! app) => #f)
  (check-true (string-contains?
    (list-ref (map strip-ansi (screen-lines screen)) 6) "⠋ Working"))

  ;; idle 复位：再次 busy 首拍仍从 ⠋ 开始
  (pi-queue-finish-task! q)
  (pi-app-working-tick! app)
  (working-advance! (pi-app-working app))
  (pi-queue-set-busy! q)
  (check (pi-app-working-tick! app) => #t)
  (check-true (string-contains?
    (list-ref (map strip-ansi (screen-lines screen)) 6) "⠋ Working"))
) ;let*

(check-report)
