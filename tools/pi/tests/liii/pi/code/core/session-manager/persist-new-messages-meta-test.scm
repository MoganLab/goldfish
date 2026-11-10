;; persist-new-messages! 元数据保留
;; 助手消息持久化为裸消息 alist 后，投影读回 provider / model / api 完整。
;;
;; 语法
;; ----
;; (persist-new-messages! file-path initial-messages final-messages)
;;
;; 参数
;; ----
;; file-path : (or string path)
;; 会话文件路径。
;;
;; initial-messages : list
;; 持久化前的消息列表，其长度用于计算新增消息的起点。
;;
;; final-messages : list
;; 完整消息列表，仅写入尚未持久化的后缀。
;;
;; 返回值
;; ----
;; 不返回
;; 副作用为向会话文件追加 JSONL 条目；助手消息条目的 message 字段
;; 为裸消息 alist（role 在第一层），无 record 序列化外壳。

(import (liii check)
  (liii path)
  (liii pi ai message)
  (liii pi code core session-manager)
  (liii pi code core session-projection)
  (liii pi code core compaction session)
) ;import

(check-set-mode! 'report-failed)

;; 助手消息持久化为裸消息 alist，读回时 role 在第一层且元数据完整
(let* ((tmp-dir (path->string (path-join (path-temp-dir) "test-pi-persist-meta")))
      ) ;
  (when (file-exists? tmp-dir) (path-rmdir tmp-dir))
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (let ((session-file (path->string (path-join tmp-dir "meta.jsonl"))))
    (save-session-header! session-file "sess-meta" "2026-11-10T08:00:00.000Z" tmp-dir)
    (persist-new-messages! session-file
      '()
      (list (pi-assistant-message (list (pi-text-block "hi"))
              :stop-reason "stop"
              :provider "google"
              :model "gemini-2.5-flash"
              :api "google-generative-ai"
            ) ;pi-assistant-message
      ) ;list
    ) ;persist-new-messages!
    (let ((history (read-session-history session-file)))
      (check (length history) => 1)
      (let ((msg (car history)))
        (check (cdr (assoc "role" msg)) => "assistant")
        (check (cdr (assoc "provider" msg)) => "google")
        (check (cdr (assoc "model" msg)) => "gemini-2.5-flash")
        (check (cdr (assoc "api" msg)) => "google-generative-ai")
      ) ;let
    ) ;let
    (delete-file session-file)
  ) ;let
  (path-rmdir tmp-dir)
) ;let*

;; 持久化后经投影读回：助手消息 provider / model / api 不丢失
;; （跨模型切换后 /resume 恢复会话的判定依据）
(let* ((tmp-dir (path->string (path-join (path-temp-dir) "test-pi-persist-meta-proj")))
      ) ;
  (when (file-exists? tmp-dir) (path-rmdir tmp-dir))
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (let ((session-file (path->string (path-join tmp-dir "meta-proj.jsonl"))))
    (save-session-header! session-file "sess-meta-proj" "2026-11-10T08:00:00.000Z" tmp-dir)
    (persist-new-messages! session-file
      '()
      (list (pi-assistant-message (list (pi-text-block "hi"))
              :stop-reason "stop"
              :provider "minimax-cn"
              :model "MiniMax-M2.7"
              :api "anthropic-messages"
            ) ;pi-assistant-message
      ) ;list
    ) ;persist-new-messages!
    (let-values (((msgs state diagnostics)
                  (pi-build-session-projection (pi-read-session-entries session-file))
                ) ;
               ) ;
      (check (length msgs) => 1)
      (let ((back (car msgs)))
        (check (pi-assistant-message-provider back) => "minimax-cn")
        (check (pi-assistant-message-model back) => "MiniMax-M2.7")
        (check (pi-assistant-message-api back) => "anthropic-messages")
      ) ;let
    ) ;let-values
    (delete-file session-file)
  ) ;let
  (path-rmdir tmp-dir)
) ;let*

(check-report)
