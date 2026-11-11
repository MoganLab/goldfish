;; pi-app-handle-resume-event!
;; 处理后台返回的会话恢复成功或失败事件并更新界面。
;;
;; 语法
;; ----
;; (pi-app-handle-resume-event! app ev)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; ev : list
;; 会话恢复事件（如 '(session_resume_error "reason")）。
;;
;; 返回值
;; ------
;; any
;; 执行界面提示更新与卡片追加的副作用。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii shutil)
  (liii string)
  (liii tui)
  (liii pi code core session-manager)
  (liii pi code core session-stats)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive footer)
) ;import

(check-set-mode! 'report-failed)

;; 聊天区卡片的纯文本
(define (chat-text app)
  (string-join
    (map (lambda (child) (string-join (map strip-ansi (component-render child 100)) "\n"))
      (pi-app-chat app)
    ) ;map
    "\n"
  ) ;string-join
) ;define

;; 1. 处理恢复失败事件并追加错误卡片
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
      ) ;
  (pi-app-handle-resume-event! app '(session_resume_error "file not found"))
  (check-true (pair? (pi-app-chat app)))
) ;let*

;; 2. 处理恢复成功事件：恢复 token 统计并更新 footer
(let* ((file (path->string (path-join (path-temp-dir) "pi-resume-stats-test.jsonl")))
       (vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
      ) ;
  (when (file-exists? file)
    (delete-file file)
  ) ;when
  (save-session-header! file "res-id" "2026-11-04T08:00:00.000Z" "/tmp")
  (save-session-message! file
    "{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"hi\"}],\"usage\":{\"input\":100,\"output\":20,\"cacheRead\":900,\"cacheWrite\":0}}"
    "msg-1" #f "2026-11-04T08:00:01.000Z"
  ) ;save-session-message!
  (pi-app-handle-resume-event! app (list 'session_resumed file "ResumedDemo" #f))
  (check (pi-session-token-stats-cache-read (pi-app-tokens app)) => 900)
  (check (pi-session-token-stats-input (pi-app-tokens app)) => 100)
  ;; 恢复的会话按最后一条 assistant usage 换算上下文占比（1020/1000000）
  (check (pi-footer-stats (%pi-app-footer app))
    =>
    "↑100 ↓20 R900 0.1%/1.0M (auto)"
  ) ;check
  (delete-file file)
) ;let*

;; 3. 恢复成功：以目标会话的历史替换当前聊天区，并保留一张状态提示卡片
(let* ((tmp-dir (path->string (path-join (path-temp-dir) "test-pi-resume-event-1323")))
       (file (path->string (path-join tmp-dir "session.jsonl")))
       (vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm :handler (lambda (a msg) #t)))
      ) ;
  (when (file-exists? tmp-dir)
    (rmtree tmp-dir)
  ) ;when
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (save-session-header! file "res-1323" "2026-11-04T08:00:00.000Z" tmp-dir)
  (save-session-message! file
    "{\"role\":\"user\",\"content\":\"目标会话的历史提问\"}" "m1" #f
    "2026-11-04T08:00:01.000Z"
  ) ;save-session-message!
  (save-session-message! file
    "{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"目标会话的历史回答\"}]}"
    "m2" "m1" "2026-11-04T08:00:02.000Z"
  ) ;save-session-message!
  ;; 旧会话的内容先在界面上
  (pi-app-add-user-message! app "旧会话的消息")
  (pi-app-handle-resume-event! app (list 'session_resumed file "ResumedDemo" #f))
  (let ((text (chat-text app)))
    ;; 旧内容被替换，不残留
    (check-false (string-contains? text "旧会话的消息"))
    ;; 目标会话的历史按顺序出现
    (check-true (string-contains? text "目标会话的历史提问"))
    (check-true (string-contains? text "目标会话的历史回答"))
    ;; 末尾保留一次 Resumed session 状态提示
    (check-true (string-contains? text "Resumed session"))
  ) ;let
  (rmtree tmp-dir)
) ;let*

;; 4. 恢复失败（目标文件不存在）：不清空当前历史，只追加错误卡片
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm :handler (lambda (a msg) #t)))
      ) ;
  (pi-app-add-user-message! app "原有会话的消息")
  (let ((before (length (pi-app-chat app))))
    (pi-app-handle-resume-event! app
      (list 'session_resumed (path->string (path-join (path-temp-dir) "missing-1323.jsonl")) #f)
    ) ;pi-app-handle-resume-event!
    (check (length (pi-app-chat app)) => (+ before 1))
    (check-true (string-contains? (chat-text app) "原有会话的消息"))
  ) ;let
) ;let*

(check-report)
