;; pi-session-context-usage
;; 从会话文件恢复上下文占用：取最后一次压缩之后的最后一条 assistant usage
;; 换算占比；压缩之后尚无新响应时占比未知（显示 "?"，对齐上游 getContextUsage）
;;
;; 语法
;; ----
;; (pi-session-context-usage file-path window)
;;
;; 参数
;; ----
;; file-path : string | #f
;; 会话文件路径；#f 或文件不存在时按空会话处理。
;; window : number | #f
;; 上下文窗口 Token 数。
;;
;; 返回值
;; ----
;; pi-context-usage
;; 上下文占用 record。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii pi code core session-manager)
  (liii pi code core compaction session)
  (liii pi code core context-usage)
) ;import

(check-set-mode! 'report-failed)

(define SESSION-FILE
  (path->string (path-join (path-temp-dir) "pi-session-context-usage-test.jsonl"))
) ;define

(when (file-exists? SESSION-FILE)
  (delete-file SESSION-FILE)
) ;when

(save-session-header! SESSION-FILE "ctx-usage-id" "2026-10-23T08:00:00.000Z"
  "/tmp/project"
) ;save-session-header!
(save-session-message! SESSION-FILE "{\"role\":\"user\",\"content\":\"hello\"}"
  "msg-user" #f "2026-10-23T08:00:01.000Z"
) ;save-session-message!
(save-session-message! SESSION-FILE
  "{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"ok\"}],\"usage\":{\"input\":1000,\"output\":200,\"cacheRead\":800,\"cacheWrite\":100,\"totalTokens\":2100}}"
  "msg-assistant-1" "msg-user" "2026-10-23T08:00:02.000Z"
) ;save-session-message!

;; 1. 取最后一条 assistant usage 换算占比
(let* ((ctx (pi-session-context-usage SESSION-FILE 10000))
       (info (pi-format-context-usage ctx))
      ) ;
  (check-true (pi-context-usage-known? ctx))
  (check (pi-context-usage-window ctx) => 10000)
  (check (pi-context-usage-percent ctx) => 21.0)
  (check info => "21.0%/10k (auto)")
) ;let*

;; 2. 压缩之后尚无新响应：占比未知
(pi-append-compaction-entry! SESSION-FILE "summary" "msg-assistant-1" 2100 '()
) ;append-compaction-entry!
(let* ((ctx (pi-session-context-usage SESSION-FILE 10000)))
  (check-false (pi-context-usage-known? ctx))
  (check (pi-format-context-usage ctx) => "?/10k (auto)")
) ;let*

;; 3. 压缩之后出现新的 assistant usage：占比恢复
(save-session-message! SESSION-FILE
  "{\"role\":\"user\",\"content\":\"next\"}"
  "msg-user-2" "msg-assistant-1" "2026-10-23T08:00:03.000Z"
) ;save-session-message!
(save-session-message! SESSION-FILE
  "{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"ok\"}],\"usage\":{\"input\":500,\"output\":100,\"cacheRead\":0,\"cacheWrite\":0,\"totalTokens\":600}}"
  "msg-assistant-2" "msg-user-2" "2026-10-23T08:00:04.000Z"
) ;save-session-message!
(let* ((ctx (pi-session-context-usage SESSION-FILE 10000)))
  (check-true (pi-context-usage-known? ctx))
  (check (pi-context-usage-percent ctx) => 6.0)
) ;let*

;; 4. 空会话：占比为 0
(let* ((ctx (pi-session-context-usage #f 10000)))
  (check-true (pi-context-usage-known? ctx))
  (check (pi-context-usage-percent ctx) => 0.0)
) ;let*

(delete-file SESSION-FILE)

(check-report)
