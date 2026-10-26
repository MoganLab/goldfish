(import (scheme base)
        (scheme file)
        (liii check)
        (liii string)
        (liii list)
        (liii path)
        (liii pi ai message)
        (liii pi code session)
        (liii pi code compact session))

(check-set-mode! 'report-failed)

(define TARGET
  (path->string (path-join (path-temp-dir) "pi-bash-exec-session.jsonl"))
) ;define

(when (file-exists? TARGET) (delete-file TARGET))
(save-session-header! TARGET "test-sid" "2026-01-01T00:00:00.000Z" "/tmp")

;; ============================================================
;; 一、append-bash-execution!：写入 bashExecution 条目并保持
;; parentId 链接（对齐 pi 的 recordBashResult 持久化）
;; ============================================================

;; 1. 常规写入：返回条目 id，消息字段完整
(define first-id (append-bash-execution! TARGET "echo hi" "hi" 0))
(let ((mid first-id))
  (check-true (string? mid))
  (let* ((entries (pi-read-session-entries TARGET))
         (entry (cadr entries))
         (msg (cdr (assoc "message" entry)))
        ) ;
    (check (cdr (assoc "type" entry)) => "message")
    (check (cdr (assoc "id" entry)) => mid)
    ;; parentId 链接到上一条消息
    (check-true (string? (cdr (assoc "parentId" entry))))
    (check (cdr (assoc "role" msg)) => "bashExecution")
    (check (cdr (assoc "command" msg)) => "echo hi")
    (check (cdr (assoc "output" msg)) => "hi")
    (check (cdr (assoc "exitCode" msg)) => 0)
    ;; 未排除时不写 excludeFromContext 字段
    (check-false (assoc "excludeFromContext" msg))
  ) ;let*
) ;let

;; 2. 排除条目（!! 前缀）：写入 excludeFromContext，投影时被跳过
(let ((mid (append-bash-execution! TARGET "secret-cmd" "hidden" 0 #t)))
  (check-true (string? mid))
  (let* ((entries (pi-read-session-entries TARGET))
         (entry (caddr entries))
         (msg (cdr (assoc "message" entry)))
        ) ;
    (check (cdr (assoc "excludeFromContext" msg)) => 'true)
    ;; parentId 链接到上一条（第一条 bash 条目）
    (check (cdr (assoc "parentId" entry)) => first-id)
  ) ;let*
) ;let

;; 3. 投影：仅未排除的 bashExecution 转为 user 消息
(let* ((entries (pi-read-session-entries TARGET))
       (msgs (pi-project-session-messages entries))
      ) ;
  (check (length msgs) => 1)
  (check (pi-message-role (car msgs)) => "user")
  (check (pi-user-message-content (car msgs)) => "Ran `echo hi`\n```\nhi\n```")
) ;let*

;; 4. sfile 为 #f（--no-session）：静默无操作
(check-false (append-bash-execution! #f "cmd" "out" 0))

;; ============================================================
;; 二、长输出截断：复用 bash 工具的保尾 2000 行策略
;; ============================================================

(let* ((long-out (string-join (map number->string (iota 3000 1)) "\n"))
       (tmp (path->string (path-join (path-temp-dir) "pi-bash-exec-trunc.jsonl")))
      ) ;
  (when (file-exists? tmp) (delete-file tmp))
  (save-session-header! tmp "trunc-sid" "2026-01-01T00:00:00.000Z" "/tmp")
  (append-bash-execution! tmp "seq 1 3000" long-out 0)
  (let* ((entries (pi-read-session-entries tmp))
         (msg (cdr (assoc "message" (cadr entries))))
         (out (cdr (assoc "output" msg)))
        ) ;
    ;; 尾部 2000 行 + 截断提示
    (check-true (string-contains? out "[Showing lines 1001-3000 of 3000.]"))
    (check-true (string-starts? out "1001\n"))
    (check-true (string-contains? out "\n1002\n"))
    (check-true (string-contains? out "\n3000"))
    ;; 头部行已被截去（不存在恰好为 "1" 的独立行）
    (check-false (string-contains? out "\n1\n"))
  ) ;let*
) ;let*

(check-report)
