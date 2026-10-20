(import (liii check)
  (liii error)
  (liii string)
  (liii tui text-width)
  (liii tui loader)
  (liii pi modes tui tool-card)
  (liii pi modes tui state)
  (liii pi modes tui viewport)
) ;import

(check-set-mode! 'report-failed)

;; 1.0 基础构造与默认值
(let ((card (make-pi-tui-tool-card :id "t1" :tool-name "bash")))
  (check-true (pi-tui-tool-card? card))
  (check (tool-card-id card) => "t1")
  (check (tool-card-tool-name card) => "bash")
  (check (tool-card-args-summary card) => "")
  (check (tool-card-status card) => 'running)
  (check (tool-card-elapsed card) => 0.0)
  (check (tool-card-result-summary card) => "")
  (check-true (spinner? (tool-card-spinner card)))
) ;let

;; 带参数构造
(let ((card (make-pi-tui-tool-card
              :id "t2"
              :tool-name "read"
              :args-summary "path: file.txt"
              :status 'success
              :elapsed 1.25
              :result-summary "10 lines read"
            ) ;make-pi-tui-tool-card
      ) ;card
     ) ;
  (check (tool-card-id card) => "t2")
  (check (tool-card-tool-name card) => "read")
  (check (tool-card-args-summary card) => "path: file.txt")
  (check (tool-card-status card) => 'success)
  (check (tool-card-elapsed card) => 1.25)
  (check (tool-card-result-summary card) => "10 lines read")
) ;let

;; 非法参数校验
(check-catch 'type-error (make-pi-tui-tool-card :id 123 :tool-name "bash"))
(check-catch 'type-error (make-pi-tui-tool-card :id "t" :tool-name 'bash))
(check-catch 'type-error (make-pi-tui-tool-card :id "t" :tool-name "b" :status 'unknown))
(check-catch 'type-error (make-pi-tui-tool-card :id "t" :tool-name "b" :elapsed "1s"))

;; 2.0 状态修改与 tick
(let ((card (make-pi-tui-tool-card :id "t3" :tool-name "bash" :args-summary "git status")))
  ;; tick 动画帧推进
  (let ((f0 (spinner-current-frame (tool-card-spinner card))))
    (tool-card-tick! card)
    (let ((f1 (spinner-current-frame (tool-card-spinner card))))
      (check-true (not (string=? f0 f1)))
    ) ;let
  ) ;let

  ;; 成功完成
  (tool-card-complete! card 0.45 "On branch main")
  (check (tool-card-status card) => 'success)
  (check (tool-card-elapsed card) => 0.45)
  (check (tool-card-result-summary card) => "On branch main")
  (check-true (not (eq? (spinner-status (tool-card-spinner card)) 'spinning)))
) ;let

;; 失败完成
(let ((card (make-pi-tui-tool-card :id "t4" :tool-name "bash" :args-summary "false")))
  (tool-card-fail! card 1.0 "Command exited with code 1")
  (check (tool-card-status card) => 'failed)
  (check (tool-card-elapsed card) => 1.0)
  (check (tool-card-result-summary card) => "Command exited with code 1")
) ;let

;; 3.0 格式化渲染
;; 3.1 运行态：[⠋] bash: git status
(let* ((card (make-pi-tui-tool-card :id "t5" :tool-name "bash" :args-summary "git status"))
       (lines (render-tool-card card 80))
      ) ;
  (check (length lines) => 1)
  (let ((plain (strip-ansi (car lines))))
    (check-true (string-contains plain "bash: git status"))
    ;; 包含括号与 spinner 符号
    (check-true (string-contains plain "["))
    (check-true (string-contains plain "]"))
  ) ;let
) ;let

;; 3.2 成功态：[✓] bash (0.4s) 与下方结果摘要
(let* ((card (make-pi-tui-tool-card
               :id "t6"
               :tool-name "bash"
               :args-summary "git status"
               :status 'success
               :elapsed 0.4
               :result-summary "clean"
             ) ;make-pi-tui-tool-card
       ) ;card
       (lines (render-tool-card card 80))
      ) ;
  (check (length lines) => 2)
  (let ((line1 (strip-ansi (car lines))) (line2 (strip-ansi (cadr lines))))
    (check-true (string-contains line1 "[✓] bash (0.4s)"))
    (check-true (string-contains line2 "  clean"))
  ) ;let
) ;let

;; 3.3 失败态：[✗] bash: failed (1.2s) 与下方错误信息
(let* ((card (make-pi-tui-tool-card
               :id "t7"
               :tool-name "bash"
               :args-summary "rm /foo"
               :status 'failed
               :elapsed 1.2
               :result-summary "permission denied"
             ) ;make-pi-tui-tool-card
       ) ;card
       (lines (render-tool-card card 80))
      ) ;
  (check (length lines) => 2)
  (let ((line1 (strip-ansi (car lines))) (line2 (strip-ansi (cadr lines))))
    (check-true (string-contains line1 "[✗] bash: failed (1.2s)"))
    (check-true (string-contains line2 "  permission denied"))
  ) ;let
) ;let

;; 3.4 无结果摘要时的渲染（仅 1 行）
(let* ((card (make-pi-tui-tool-card
               :id "t8"
               :tool-name "edit"
               :status 'success
               :elapsed 0.1
             ) ;make-pi-tui-tool-card
       ) ;card
       (lines (render-tool-card card 80))
      ) ;
  (check (length lines) => 1)
  (let ((line1 (strip-ansi (car lines))))
    (check-true (string-contains line1 "[✓] edit (0.1s)"))
  ) ;let
) ;let

;; 4.0 状态管理与事件流接线
;; 4.1 收到 tool_execution_start 追加卡片
(let* ((st (make-pi-tui-state :status 'running))
       (card (state-handle-tool-start! st "call_1" "bash" '((command . "ls -la"))))
      ) ;
  (check-true (pi-tui-tool-card? card))
  (check (tool-card-id card) => "call_1")
  (check (tool-card-tool-name card) => "bash")
  (check (tool-card-args-summary card) => "ls -la")
  (check (state-active-tool-card st) => card)
  ;; 自动存入 state-messages
  (check-true (pair? (member card (state-messages st))))
  (check-true (state-dirty? st))
) ;let

;; 4.2 RUNNING 节拍下 tick 推进
(let* ((st (make-pi-tui-state :status 'running))
       (card (state-handle-tool-start! st "call_2" "read" '(("path" . "foo.scm"))))
      ) ;
  (state-clear-dirty! st)
  (let ((f0 (spinner-current-frame (tool-card-spinner card))))
    (state-tick-active-tools! st)
    (let ((f1 (spinner-current-frame (tool-card-spinner card))))
      (check-true (not (string=? f0 f1)))
      (check-true (state-dirty? st))
    ) ;let
  ) ;let
) ;let

;; 4.3 收到 tool_execution_end 成功平滑替换
(let* ((st (make-pi-tui-state :status 'running))
       (card (state-handle-tool-start! st "call_3" "bash" '(("command" . "pwd"))))
      ) ;
  (state-clear-dirty! st)
  (state-handle-tool-end! st "call_3" "/workspace" #f)
  (check (tool-card-status card) => 'success)
  (check (tool-card-result-summary card) => "/workspace")
  (check (state-active-tool-card st) => #f)
  (check-true (state-dirty? st))
) ;let

;; 4.4 收到 tool_execution_end 失败平滑替换
(let* ((st (make-pi-tui-state :status 'running))
       (card (state-handle-tool-start! st "call_4" "bash" '(("command" . "exit 1"))))
      ) ;
  (state-clear-dirty! st)
  (state-handle-tool-end! st "call_4" "exit 1" #t)
  (check (tool-card-status card) => 'failed)
  (check (tool-card-result-summary card) => "exit 1")
  (check (state-active-tool-card st) => #f)
  (check-true (state-dirty? st))
) ;let

;; 5.0 Viewport 视口集成测试
(let* ((st (make-pi-tui-state))
       (c1 (make-pi-tui-tool-card :id "t_v1" :tool-name "bash" :args-summary "echo 1"))
      ) ;
  (state-append-message! st (cons 'user "run bash"))
  (state-append-message! st c1)
  (let ((vlines (render-messages-viewport (state-messages st) 80 10 0)))
    ;; 视口应正常渲染出工具卡片
    (check-true (pair? (member #t (map (lambda (l) (string-contains l "bash: echo 1")) vlines))))
  ) ;let
) ;let

(check-report)
