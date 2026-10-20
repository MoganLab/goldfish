(import (liii check)
  (liii error)
  (liii string)
  (liii tui text-width)
  (liii pi modes tui thinking)
  (liii pi modes tui state)
  (liii pi modes tui viewport)
  (liii pi modes tui editor)
  (liii pi modes tui loop dispatch)
) ;import

(check-set-mode! 'report-failed)

;; 1.0 基础构造与默认值
(let ((card (make-pi-tui-thinking-card)))
  (check-true (pi-tui-thinking-card? card))
  (check (thinking-card-content card) => "")
  (check (thinking-card-token-count card) => 0)
  (check (thinking-card-collapsed? card) => #t)
  (check (thinking-card-completed? card) => #f)
  (check (thinking-card-scroll-offset card) => 0)
) ;let

;; 带参数构造
(let ((card (make-pi-tui-thinking-card
              :content "pondering the meaning of life"
              :token-count 42
              :collapsed? #f
              :completed? #t
              :scroll-offset 2
            ) ;make-pi-tui-thinking-card
      ) ;card
     ) ;
  (check (thinking-card-content card) => "pondering the meaning of life")
  (check (thinking-card-token-count card) => 42)
  (check (thinking-card-collapsed? card) => #f)
  (check (thinking-card-completed? card) => #t)
  (check (thinking-card-scroll-offset card) => 2)
) ;let

;; 非法参数校验
(check-catch 'type-error (make-pi-tui-thinking-card :content 123))
(check-catch 'type-error (make-pi-tui-thinking-card :token-count -5))
(check-catch 'type-error (make-pi-tui-thinking-card :collapsed? "yes"))
(check-catch 'type-error (make-pi-tui-thinking-card :completed? 1))
(check-catch 'type-error (make-pi-tui-thinking-card :scroll-offset -1))

;; 2.0 状态修改与切换
;; In-place toggle
(let ((card (make-pi-tui-thinking-card :collapsed? #t)))
  (check (thinking-card-collapsed? (thinking-card-toggle! card)) => #f)
  (check (thinking-card-collapsed? (thinking-card-toggle! card)) => #t)
) ;let

;; Pure functional toggle
(let* ((c1 (make-pi-tui-thinking-card :collapsed? #t))
       (c2 (thinking-card-with-toggle c1))
      ) ;
  (check (thinking-card-collapsed? c1) => #t)
  (check (thinking-card-collapsed? c2) => #f)
) ;let

;; 内容追加与完成态更新
(let ((card (make-pi-tui-thinking-card :content "Part 1. ")))
  (thinking-card-append-content! card "Part 2.")
  (check (thinking-card-content card) => "Part 1. Part 2.")
  (thinking-card-set-completed! card #t)
  (check (thinking-card-completed? card) => #t)
  (thinking-card-set-token-count! card 120)
  (check (thinking-card-token-count card) => 120)
  (thinking-card-set-scroll-offset! card 3)
  (check (thinking-card-scroll-offset card) => 3)
) ;let

;; 3.0 折叠态单行摘要渲染
;; 进行中：> [Thinking] 正在深度思考 (N tokens)...
(let* ((card (make-pi-tui-thinking-card :token-count 55 :completed? #f))
       (lines (render-thinking-collapsed card 80))
      ) ;
  (check (length lines) => 1)
  (let ((plain (strip-ansi (car lines))))
    (check plain => "> [Thinking] 正在深度思考 (55 tokens)...")
    (check-true (<= (display-width plain) 80))
  ) ;let
) ;let

;; 完成后：> [Thinking] 推理已完成 (按 Tab 展开)
(let* ((card (make-pi-tui-thinking-card :completed? #t))
       (lines (render-thinking-collapsed card 80))
      ) ;
  (check (length lines) => 1)
  (let ((plain (strip-ansi (car lines))))
    (check plain => "> [Thinking] 推理已完成 (按 Tab 展开)")
  ) ;let
) ;let

;; 超窄屏截断
(let* ((card (make-pi-tui-thinking-card :completed? #t))
       (lines (render-thinking-collapsed card 10))
      ) ;
  (check (length lines) => 1)
  (check-true (<= (display-width (strip-ansi (car lines))) 10))
) ;let

;; 4.0 展开态 Box 边框包裹与排版
(let* ((card (make-pi-tui-thinking-card
               :content "First line of thought\nSecond line of thought"
               :collapsed? #f
             ) ;make-pi-tui-thinking-card
       ) ;card
       (lines (render-thinking-expanded card 40))
      ) ;
  ;; 展开态至少有顶边框、内容行、底边框
  (check-true (>= (length lines) 3))
  ;; 首行应是顶框
  (check-true (string-contains (car lines) "Thinking"))
  ;; 末行应是底框
  (check-true (string-contains (car (reverse lines)) "┘"))
  ;; 每一行宽度都不超过 40
  (for-each (lambda (l)
              (check-true (<= (display-width (strip-ansi l)) 40))
            ) ;lambda
            lines
  ) ;for-each
) ;let

;; 展开态高度受限与滚动
(let* ((card (make-pi-tui-thinking-card
               :content "line 1\nline 2\nline 3\nline 4\nline 5\nline 6"
               :scroll-offset 0
             ) ;make-pi-tui-thinking-card
       ) ;card
       (lines (render-thinking-expanded card 40 5))
      ) ;
  ;; 限制最大高度为 5：顶边 + 3行内容 + 底边 = 5行
  (check (length lines) => 5)
) ;let

;; 统一调度渲染器 render-thinking-card
(let ((card (make-pi-tui-thinking-card :collapsed? #t)))
  (check (length (render-thinking-card card 50)) => 1)
  (thinking-card-toggle! card)
  (check-true (>= (length (render-thinking-card card 50)) 3))
) ;let

;; 5.0 State 集成与查找切换
(let* ((card (make-pi-tui-thinking-card :content "analyzing code"))
       (st (make-pi-tui-state
             :messages (list (cons 'user "hello") card (cons 'assistant "world"))
           ) ;make-pi-tui-state
       ) ;st
      ) ;
  (check-true (pi-tui-thinking-card? (state-find-active-thinking-card st)))
  (check (thinking-card-content (state-find-active-thinking-card st)) => "analyzing code")
  (check (thinking-card-collapsed? card) => #t)
  ;; 切换折叠状态并验证置脏
  (state-clear-dirty! st)
  (check (state-dirty? st) => #f)
  (check-true (pi-tui-thinking-card? (state-toggle-thinking-card! st)))
  (check (thinking-card-collapsed? card) => #f)
  (check (state-dirty? st) => #t)
  ;; 再次切换恢复折叠
  (state-toggle-thinking-card! st)
  (check (thinking-card-collapsed? card) => #t)
) ;let

;; 空消息或无 Thinking 卡片时的状态查找
(let ((st (make-pi-tui-state :messages (list (cons 'user "ping")))))
  (check (state-find-active-thinking-card st) => #f)
  (check (state-toggle-thinking-card! st) => #f)
) ;let

;; 6.0 视口渲染集成（包含 Thinking 卡片）
(let* ((card (make-pi-tui-thinking-card :content "thought 1\nthought 2" :collapsed? #t))
       (msgs (list (cons 'user "prompt") card))
       (lines-col (render-messages-viewport msgs 60 10 0))
      ) ;
  ;; 折叠态：user 消息 (1行) + 摘要 (1行) = 2行
  (check (length lines-col) => 2)
  ;; 展开卡片
  (thinking-card-toggle! card)
  (let ((lines-exp (render-messages-viewport msgs 60 10 0)))
    ;; 展开态：user 消息 (1行) + Box 边框卡片 (至少3行) >= 4行
    (check-true (>= (length lines-exp) 4))
  ) ;let
) ;let

;; 7.0 快捷键分发调度（Tab / Ctrl+O）
;; 输入框为空时按 Tab 触发折叠切换
(let* ((card (make-pi-tui-thinking-card :collapsed? #t))
       (st (make-pi-tui-state :messages (list card)))
      ) ;
  (state-clear-dirty! st)
  (%dispatch-tui-key st 'tab #f)
  (check (thinking-card-collapsed? card) => #f)
  (check (state-dirty? st) => #t)
  ;; 再次按 Tab 收起
  (%dispatch-tui-key st 'tab #f)
  (check (thinking-card-collapsed? card) => #t)
) ;let

;; 输入框非空时按 Tab 不触发折叠（保留给编辑器）
(let* ((card (make-pi-tui-thinking-card :collapsed? #t))
       (ed (make-pi-tui-editor :text "git status"))
       (st (make-pi-tui-state :messages (list card) :input-editor ed))
      ) ;
  (%dispatch-tui-key st 'tab #f)
  (check (thinking-card-collapsed? card) => #t)
) ;let

;; Ctrl+O 无论输入框是否有内容均能切换折叠
(let* ((card (make-pi-tui-thinking-card :collapsed? #t))
       (ed (make-pi-tui-editor :text "git commit"))
       (st (make-pi-tui-state :messages (list card) :input-editor ed))
      ) ;
  (%dispatch-tui-key st 'ctrl-o #f)
  (check (thinking-card-collapsed? card) => #f)
  ;; "\x0f" 同样生效
  (%dispatch-tui-key st "\x0f" #f)
  (check (thinking-card-collapsed? card) => #t)
) ;let

(check-report)
