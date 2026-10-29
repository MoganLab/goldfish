(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report)

;; 退出 TUI 后打印回主屏的对话记录（fullscreenExitOutput=transcript）：
;;   pi-app-transcript-lines 返回本次会话的完整消息历史（不含页眉与输入区）。
;; 回归点：曾把消息列表（container-children 的结果）直接交给 component-render，
;; 触发 type-error 并被静默 catch，退出后终端里看不到任何历史。

;; 1. 用户消息与助手回复都在历史里
(let* ((vt (make-virtual-terminal :width 100 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a msg) #t)))
      ) ;
  (pi-app-add-user-message! app "hello transcript")
  (pi-app-add-assistant-text! app "hi there")
  (let ((plain (map strip-ansi (pi-app-transcript-lines app))))
    (check-true (pair? plain))
    (check-true (string-contains? (string-join plain "\n") "hello transcript"))
    (check-true (string-contains? (string-join plain "\n") "hi there"))
  ) ;let
) ;let*

;; 2. 历史按消息顺序排列（用户消息在助手回复之前）
(define (index-of haystack needle)
  (let ((n (string-length needle)))
    (let loop
      ((i 0))
      (cond
       ((> (+ i n) (string-length haystack)) #f)
       ((string-contains? (substring haystack i (+ i n)) needle) i)
       (else (loop (+ i 1)))
      ) ;cond
    ) ;let
  ) ;let
) ;define
(let* ((vt (make-virtual-terminal :width 100 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a msg) #t)))
      ) ;
  (pi-app-add-user-message! app "first-msg")
  (pi-app-add-assistant-text! app "second-msg")
  (let* ((plain (map strip-ansi (pi-app-transcript-lines app)))
         (joined (string-join plain "\n"))
         (i-first (index-of joined "first-msg"))
         (i-second (index-of joined "second-msg"))
        ) ;
    (check-true (and (number? i-first) (number? i-second)))
    (check-true (< i-first i-second))
  ) ;let*
) ;let*

;; 3. 没有消息时历史为空（不返回页眉横幅）
(let* ((vt (make-virtual-terminal :width 100 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a msg) #t)))
      ) ;
  (check (pi-app-transcript-lines app) => '())
) ;let*

;; 4. 历史不受视口滚动位置影响：滚回顶部后仍包含全部消息
(let* ((vt (make-virtual-terminal :width 100 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a msg) #t)))
      ) ;
  (let loop
    ((i 0))
    (when (< i 10)
      (pi-app-add-user-message! app (string-append "msg-" (number->string i)))
      (loop (+ i 1))
    ) ;when
  ) ;let
  (pi-app-scroll-top! app)
  (let ((plain (map strip-ansi (pi-app-transcript-lines app))))
    (check-true (string-contains? (string-join plain "\n") "msg-0"))
    (check-true (string-contains? (string-join plain "\n") "msg-9"))
  ) ;let
) ;let*

(check-report)
