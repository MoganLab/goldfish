;; pi-app-rebuild-history!
;; 用目标会话的历史替换交互式应用的聊天区（启动恢复与 /resume 切换共用）。
;;
;; 语法
;; ----
;; (pi-app-rebuild-history! app session-file)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; session-file : string
;; 目标会话的 JSONL 文件路径。
;;
;; 返回值
;; ------
;; boolean
;; #t 表示重建成功（含空历史）；#f 表示失败，当前界面保持不变。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii shutil)
  (liii string)
  (liii tui)
  (liii pi code core session-manager)
  (liii pi code modes interactive app)
  (liii pi code modes interactive app-history)
  (liii pi code modes interactive state)
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

;; 临时会话目录
(define %tmp-dir (path->string (path-join (path-temp-dir) "test-pi-app-history-1323")))

(define (make-session-file name)
  (let ((dir (path->string (path-join %tmp-dir name))))
    (path-mkdir dir :parents #t :exist-ok #t)
    (path->string (path-join dir "session.jsonl"))
  ) ;let
) ;define

;; 写入一个含用户消息、助手回复与工具调用的会话
(define (write-sample-session! file)
  (save-session-header! file "sess-1323" "2026-11-04T08:00:00.000Z" %tmp-dir)
  (save-session-message! file
    "{\"role\":\"user\",\"content\":\"历史提问暗号\"}" "m1" #f
    "2026-11-04T08:00:01.000Z"
  ) ;save-session-message!
  (save-session-message! file
    (string-append
      "{\"role\":\"assistant\",\"content\":["
      "{\"type\":\"thinking\",\"thinking\":\"历史推理\"},"
      "{\"type\":\"text\",\"text\":\"历史回答暗号\"},"
      "{\"type\":\"toolCall\",\"id\":\"call-1\",\"name\":\"mytool\",\"arguments\":{}}"
      "]}"
    ) ;string-append
    "m2" "m1" "2026-11-04T08:00:02.000Z"
  ) ;save-session-message!
  (save-session-message! file
    (string-append
      "{\"role\":\"toolResult\",\"toolCallId\":\"call-1\",\"toolName\":\"mytool\","
      "\"content\":[{\"type\":\"text\",\"text\":\"历史工具输出\"}],\"isError\":false}"
    ) ;string-append
    "m3" "m2" "2026-11-04T08:00:03.000Z"
  ) ;save-session-message!
  file
) ;define

(when (file-exists? %tmp-dir)
  (rmtree %tmp-dir)
) ;when
(path-mkdir %tmp-dir :parents #t :exist-ok #t)

;; 1. 重建成功：聊天区出现历史用户消息、助手回答、思考与工具卡片
(let* ((file (write-sample-session! (make-session-file "a")))
       (vt (make-virtual-terminal :width 100 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a msg) #t)))
       (ok? (pi-app-rebuild-history! app file))
       (text (chat-text app))
      ) ;
  (check-true ok?)
  (check-true (string-contains? text "历史提问暗号"))
  (check-true (string-contains? text "历史回答暗号"))
  (check-true (string-contains? text "历史推理"))
  (check-true (string-contains? text "历史工具输出"))
  ;; 工具卡片索引按 toolCallId 重建
  (check (length (%pi-app-tool-cards app)) => 1)
  (check (assoc "call-1" (%pi-app-tool-cards app)) => (assoc "call-1" (%pi-app-tool-cards app)))
) ;let*

;; 2. 重复重建幂等：不产生重复卡片
(let* ((file (write-sample-session! (make-session-file "b")))
       (vt (make-virtual-terminal :width 100 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a msg) #t)))
      ) ;
  (pi-app-rebuild-history! app file)
  (let ((n1 (length (pi-app-chat app))))
    (check-true (> n1 0))
    (pi-app-rebuild-history! app file)
    (check (length (pi-app-chat app)) => n1)
  ) ;let
) ;let*

;; 3. 重建替换旧内容：切换到另一个会话后旧卡片不再残留
(let* ((file-a (write-sample-session! (make-session-file "c")))
       (file-b (path->string (path-join %tmp-dir "b-only.jsonl")))
       (vt (make-virtual-terminal :width 100 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a msg) #t)))
      ) ;
  (save-session-header! file-b "sess-b" "2026-11-04T09:00:00.000Z" %tmp-dir)
  (save-session-message! file-b
    "{\"role\":\"user\",\"content\":\"另一个会话的提问\"}" "n1" #f
    "2026-11-04T09:00:01.000Z"
  ) ;save-session-message!
  (pi-app-rebuild-history! app file-a)
  (check-true (string-contains? (chat-text app) "历史提问暗号"))
  (pi-app-rebuild-history! app file-b)
  (check-false (string-contains? (chat-text app) "历史提问暗号"))
  (check-true (string-contains? (chat-text app) "另一个会话的提问"))
) ;let*

;; 4. 空会话（只有会话头）：界面变为空历史，不残留旧会话内容
(let* ((file (path->string (path-join %tmp-dir "empty.jsonl")))
       (vt (make-virtual-terminal :width 100 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a msg) #t)))
      ) ;
  (save-session-header! file "sess-empty" "2026-11-04T10:00:00.000Z" %tmp-dir)
  (pi-app-rebuild-history! app (write-sample-session! (make-session-file "d")))
  (check-true (pair? (pi-app-chat app)))
  (check-true (pi-app-rebuild-history! app file))
  (check (pi-app-chat app) => '())
) ;let*

;; 5. 目标文件不存在：返回 #f，当前界面保持不变
(let* ((file (write-sample-session! (make-session-file "e")))
       (vt (make-virtual-terminal :width 100 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a msg) #t)))
      ) ;
  (pi-app-rebuild-history! app file)
  (let ((before (length (pi-app-chat app))))
    (check-false (pi-app-rebuild-history! app (path->string (path-join %tmp-dir "nope.jsonl"))))
    (check (length (pi-app-chat app)) => before)
  ) ;let
) ;let*

;; 6. 重建后 thinking 引用与选区复位，工具卡片索引按新会话重建
(let* ((file (write-sample-session! (make-session-file "f")))
       (vt (make-virtual-terminal :width 100 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a msg) #t)))
      ) ;
  (%pi-app-thinking-card-set! app 'stale)
  (%pi-app-tool-cards-set! app '(("old" . stale)))
  (pi-app-rebuild-history! app file)
  (check (pi-app-thinking-card app) => #f)
  (check (assoc "old" (%pi-app-tool-cards app)) => #f)
  (check (assoc "call-1" (%pi-app-tool-cards app)) => (assoc "call-1" (%pi-app-tool-cards app)))
) ;let*

;; 7. 恢复的历史进入退出 transcript
(let* ((file (write-sample-session! (make-session-file "g")))
       (vt (make-virtual-terminal :width 100 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a msg) #t)))
      ) ;
  (pi-app-rebuild-history! app file)
  (let ((plain (string-join (map strip-ansi (pi-app-transcript-lines app)) "\n")))
    (check-true (string-contains? plain "历史提问暗号"))
    (check-true (string-contains? plain "历史回答暗号"))
  ) ;let
) ;let*

;; 8. 损坏条目不致崩溃：不改写原会话文件
(let* ((file (path->string (path-join %tmp-dir "broken.jsonl")))
       (vt (make-virtual-terminal :width 100 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a msg) #t)))
      ) ;
  (save-session-header! file "sess-broken" "2026-11-04T11:00:00.000Z" %tmp-dir)
  (save-session-message! file
    "{\"role\":\"user\",\"content\":\"损坏之前的消息\"}" "b1" #f
    "2026-11-04T11:00:01.000Z"
  ) ;save-session-message!
  (append-session-entry! file "{ this is not json")
  (save-session-message! file
    "{\"role\":\"user\",\"content\":\"损坏之后的消息\"}" "b2" "b1"
    "2026-11-04T11:00:02.000Z"
  ) ;save-session-message!
  (check-true (pi-app-rebuild-history! app file))
  (check-true (string-contains? (chat-text app) "损坏之前的消息"))
  ;; 原 JSONL 不被改写
  (check-true (string-contains? (string-join (path-read-lines file) "\n") "{ this is not json"))
) ;let*

(when (file-exists? %tmp-dir)
  (rmtree %tmp-dir)
) ;when

(check-report)
