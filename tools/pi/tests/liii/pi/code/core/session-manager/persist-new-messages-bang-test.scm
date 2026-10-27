(import (liii check)
  (liii path)
  (liii json)
  (liii pi ai message)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

;; persist-new-messages!
;; 将会话中尚未持久化的消息追加到 JSONL 会话文件。
;;
;; 语法
;; ----
;; (persist-new-messages! session-file initial-messages final-messages [persisted-count])
;;
;; 参数
;; ----
;; session-file : string | path | #f
;; 目标会话文件；为 #f 时不执行写入。
;; initial-messages : list
;; 持久化前的消息列表，其长度用于计算新增消息的起点。
;; final-messages : list
;; 完整消息列表，仅写入尚未持久化的后缀。
;; persisted-count : integer | #f
;; 可选的已持久化消息数量，优先于 initial-messages 的长度。
;;
;; 返回值
;; ----
;; 未指定；消息按顺序写入，并通过 parentId 串联。
;;
;; 示例
;; ----
;; (persist-new-messages! "session.jsonl" '() (list user-msg assistant-msg))

;; 写入首轮 user/assistant 消息并恢复
(let* ((tmp-dir (path->string (path-join (path-temp-dir) "test-pi-session-persist-initial"))
       ) ;tmp-dir
       (session-file (path->string (path-join tmp-dir "initial-messages.jsonl")))
      ) ;
  (if (file-exists? tmp-dir) (path-rmdir tmp-dir))
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (save-session-header! session-file "sess-initial" "2026-10-21T08:00:00.000Z"
    tmp-dir
  ) ;save-session-header!
  (persist-new-messages! session-file
    '()
    (list (list (cons 'role "user") (cons 'content "first query"))
      (list (cons 'role "assistant") (cons 'content "first response"))
    ) ;list
  ) ;persist-new-messages!
  (let ((history (read-session-history session-file)))
    (check (length history) => 2)
    (check (cdr (assoc "content" (car history))) => "first query")
    (check (cdr (assoc "content" (cadr history))) => "first response")
    (check-true (string? (find-last-session-message-id session-file)))
  ) ;let
  (delete-file session-file)
  (path-rmdir tmp-dir)
) ;let*

;; 写入 record 并在恢复时正确归一化 content
(let* ((tmp-dir (path->string (path-join (path-temp-dir) "test-pi-session-persist-roundtrip"))
       ) ;tmp-dir
       (session-file (path->string (path-join tmp-dir "persist-test.jsonl")))
      ) ;
  (if (file-exists? tmp-dir) (path-rmdir tmp-dir))
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (save-session-header! session-file "sess-rt" "2026-10-21T08:00:00.000Z" tmp-dir)
  (let* ((u-msg (pi-user-message "run test"))
         (tr-bash (pi-tool-result-message "c_bash" "bash" (list (pi-text-block "SECRET-1071")) #f)
         ) ;tr-bash
         (tr-read (pi-tool-result-message "c_read" "read" (list (pi-text-block "FILE-1071")) #f)
         ) ;tr-read
        ) ;
    (persist-new-messages! session-file '() (list u-msg tr-bash tr-read))
    (let ((history (read-session-history session-file)))
      (check (length history) => 3)
      (let ((h-bash (cadr history)) (h-read (caddr history)))
        (check (cdr (assoc "role" h-bash)) => "toolResult")
        (check-true (list? (cdr (assoc "content" h-bash))))
        (check
          (cdr
            (assoc "text" (car (cdr (assoc "content" h-bash))))
          ) ;cdr
          =>
          "SECRET-1071"
        ) ;check
        (check (cdr (assoc "role" h-read)) => "toolResult")
        (check-true (list? (cdr (assoc "content" h-read))))
        (check
          (cdr
            (assoc "text" (car (cdr (assoc "content" h-read))))
          ) ;cdr
          =>
          "FILE-1071"
        ) ;check
      ) ;let
    ) ;let
  ) ;let*
  (delete-file session-file)
  (path-rmdir tmp-dir)
) ;let*

(check-report)
