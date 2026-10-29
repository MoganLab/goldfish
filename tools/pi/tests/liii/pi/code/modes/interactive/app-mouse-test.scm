(import (scheme base)
        (liii check)
        (liii list)
        (liii tui)
        (liii tui mouse)
        (liii pi code modes interactive)
        (liii pi code modes interactive app)
        (liii pi code modes interactive views)
        (liii pi code modes interactive bash))

(check-set-mode! 'report-failed)

;; 辅助：构造带虚拟终端的 app（handler 恒成功，bash-handler 固定输出）
(define (%make-test-app . opts)
  (let* ((bash-fn (if (null? opts) #f (car opts)))
         (term (make-virtual-terminal :width 80 :height 40))
        ) ;
    (make-pi-app :terminal term
      :handler (lambda (a m) #t)
      :bash-handler (or bash-fn (lambda (cmd) (values "out1\nout2" 0)))
    ) ;make-pi-app
  ) ;let*
) ;define

;; 辅助：第 n 张（0-based）工具/bash 卡片的展开状态
(define (%card-expanded? chat idx)
  (let ((cards (append (filter tool-card-component? chat)
                 (filter bash-card-component? chat)
               ) ;append
         ) ;cards
       ) ;
    (if (tool-card-component? (list-ref cards idx))
      (tool-card-expanded? (list-ref cards idx))
      (bash-card-expanded? (list-ref cards idx))
    ) ;if
  ) ;let
) ;define

;; 1. click 命中工具卡片 → 该卡片 expanded? 取反
(let* ((app (%make-test-app)))
  (pi-app-add-tool-result! app "read" '(("path" . "a.scm")) 'success "l1\nl2\nl3")
  (pi-app-render! app)
  (let ((chat (pi-app-chat app)))
    (check (length (filter tool-card-component? chat)) => 1)
    (check (%card-expanded? chat 0) => #f)
    ;; 扫描前 12 个 doc 行，找到能翻转状态的点击位置（卡片内容行）
    (define hit-y #f)
    (for-each
      (lambda (y)
        (when (not hit-y)
          (pi-app-handle-mouse! app (make-mouse-event 'click 'left 5 y))
          (when (%card-expanded? chat 0) (set! hit-y y))
        ) ;when
      ) ;lambda
      (iota 12)
    ) ;for-each
    (check-true (and hit-y (< hit-y 12)))
    ;; 同位置再次点击：折叠回去
    (pi-app-handle-mouse! app (make-mouse-event 'click 'left 5 hit-y))
    (check (%card-expanded? chat 0) => #f)
  ) ;let
) ;let*

;; 2. 单卡片粒度：只翻转被点击的卡片，另一张不变
(let* ((app (%make-test-app)))
  (pi-app-add-tool-result! app "read" '(("path" . "a.scm")) 'success "l1")
  (pi-app-add-tool-result! app "grep" '(("pattern" . "x")) 'success "g1\ng2")
  (pi-app-render! app)
  (let ((chat (pi-app-chat app)))
    (check (length (filter tool-card-component? chat)) => 2)
    ;; 扫描找到只翻转第一张、不碰第二张的点击位置
    (define hit-y #f)
    (for-each
      (lambda (y)
        (when (not hit-y)
          (pi-app-handle-mouse! app (make-mouse-event 'click 'left 5 y))
          (when (and (%card-expanded? chat 0) (not (%card-expanded? chat 1)))
            (set! hit-y y)
          ) ;when
        ) ;when
      ) ;lambda
      (iota 16)
    ) ;for-each
    (check-true (and hit-y (< hit-y 16)))
    ;; 点击区域外（远超内容）不改变状态
    (pi-app-handle-mouse! app (make-mouse-event 'click 'left 5 hit-y))  ; 折回
    (check (%card-expanded? chat 0) => #f)
    (pi-app-handle-mouse! app (make-mouse-event 'click 'left 5 39))     ; 末行之外
    (check (%card-expanded? chat 0) => #f)
    (check (%card-expanded? chat 1) => #f)
  ) ;let
) ;let*

;; 3. pending 卡片不响应点击
(let* ((app (%make-test-app)))
  (pi-app-add-tool-result! app "read" '(("path" . "a.scm")) 'pending)
  (pi-app-render! app)
  (let ((chat (pi-app-chat app)))
    (for-each
      (lambda (y) (pi-app-handle-mouse! app (make-mouse-event 'click 'left 5 y)))
      (iota 12)
    ) ;for-each
    (check (%card-expanded? chat 0) => #f)
  ) ;let
) ;let*

;; 4. 非左键 / 滚轮 / press / release 不触发
(let* ((app (%make-test-app)))
  (pi-app-add-tool-result! app "read" '(("path" . "a.scm")) 'success "l1")
  (pi-app-render! app)
  (let ((chat (pi-app-chat app)))
    (for-each
      (lambda (ev)
        (pi-app-handle-mouse! app ev)
        (check (%card-expanded? chat 0) => #f)
      ) ;lambda
      (list (make-mouse-event 'click 'middle 5 4)
        (make-mouse-event 'click 'right 5 4)
        (make-mouse-event 'wheel-up 'none 5 4)
        (make-mouse-event 'press 'left 5 4)
        (make-mouse-event 'release 'left 5 4)
      ) ;list
    ) ;for-each
  ) ;let
) ;let*

;; 5. bash 卡片同样支持点击切换（pending 的 bash 卡片不切换）
(let* ((app (%make-test-app)))
  (pi-app-run-bash! app "echo hi")
  (pi-app-render! app)
  (let ((chat (pi-app-chat app)))
    (check (length (filter bash-card-component? chat)) => 1)
    (define hit-y #f)
    (for-each
      (lambda (y)
        (when (not hit-y)
          (pi-app-handle-mouse! app (make-mouse-event 'click 'left 5 y))
          (when (%card-expanded? chat 0) (set! hit-y y))
        ) ;when
      ) ;lambda
      (iota 16)
    ) ;for-each
    (check-true (and hit-y (< hit-y 16)))
  ) ;let
) ;let*

(check-report)
