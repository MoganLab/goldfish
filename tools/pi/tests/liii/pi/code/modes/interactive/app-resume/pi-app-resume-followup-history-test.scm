;; pi-app-resume-followup-history
;; 恢复历史会话后继续追问：历史只出现一次，新消息接在历史之后。
;;
;; 语法
;; ----
;; (pi-app-resume-followup-history ...)
;;
;; 参数
;; ----
;; 详见语法说明。
;;
;; 返回值
;; ----
;; any
;; 视具体调用结果而定。

(import (scheme base)
  (liii check)
  (liii path)
  (liii shutil)
  (liii string)
  (liii list)
  (liii tui)
  (liii pi code core session-manager)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

;; 聊天区卡片的纯文本（去掉 ANSI 颜色码）
(define (chat-text app)
  (string-join
    (map (lambda (child) (string-join (map strip-ansi (component-render child 100)) "\n"))
      (pi-app-chat app)
    ) ;map
    "\n"
  ) ;string-join
) ;define

;; 1. 恢复历史后发送新消息：历史只出现一次，新消息接在历史之后
(let* ((tmp-dir (path->string (path-join (path-temp-dir) "test-pi-resume-followup-1323")))
       (file-a (path->string (path-join tmp-dir "a.jsonl")))
       (vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (a msg) #t)))
      ) ;
  (when (file-exists? tmp-dir)
    (rmtree tmp-dir)
  ) ;when
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (save-session-header! file-a "sess-a" "2026-10-21T08:00:00.000Z" tmp-dir)
  (save-session-message! file-a
    "{\"role\":\"user\",\"content\":\"会话A的提问\"}" "a1" #f
    "2026-10-21T08:00:01.000Z"
  ) ;save-session-message!
  (pi-app-handle-resume-event! app (list 'session_resumed file-a "Alpha" #f))
  (let ((before (length (pi-app-chat app))))
    (check-true (> before 0))
    ;; 新消息接在历史之后：只新增一张用户卡片
    (pi-app-submit! app "新的追问")
    (check (length (pi-app-chat app)) => (+ before 1))
    (let ((text (chat-text app)))
      (check-true (string-contains? text "新的追问"))
      ;; 历史提问只出现一次（切分后为两段）
      (check (length (string-split text "会话A的提问")) => 2)
    ) ;let
  ) ;let
  (rmtree tmp-dir)
) ;let*

(check-report)
