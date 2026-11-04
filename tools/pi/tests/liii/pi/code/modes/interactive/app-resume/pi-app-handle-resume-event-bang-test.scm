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
  (liii tui)
  (liii pi code core session-manager)
  (liii pi code core session-stats)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive footer)
) ;import

(check-set-mode! 'report-failed)

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
  (when (file-exists? file) (delete-file file))
  (save-session-header! file "res-id" "2026-11-04T08:00:00.000Z" "/tmp")
  (save-session-message! file
    "{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"hi\"}],\"usage\":{\"input\":100,\"output\":20,\"cacheRead\":900,\"cacheWrite\":0}}"
    "msg-1" #f "2026-11-04T08:00:01.000Z"
  ) ;save-session-message!
  (pi-app-handle-resume-event! app (list 'session_resumed file "ResumedDemo" #f))
  (check (pi-session-token-stats-cache-read (pi-app-tokens app)) => 900)
  (check (pi-session-token-stats-input (pi-app-tokens app)) => 100)
  (check (pi-footer-stats (%pi-app-footer app)) => "↑100 ↓20 R900 0.0%/1.0M (auto)")
  (delete-file file)
) ;let*

(check-report)
