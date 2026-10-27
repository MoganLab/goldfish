(import (scheme base)
  (liii check)
  (liii path)
  (liii shutil)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

;; setup-initial-messages：构造首轮消息列表
;; --resume / --continue 时恢复历史消息并追加本轮用户输入

(define CWD "/home/da/git/goldfish")

(let* ((tmp-agent-dir (path->string (path-join (path-temp-dir) "test-setup-initial-msgs")))
       (cfg `((agent-dir . ,tmp-agent-dir)))
       (sdir (get-session-dir tmp-agent-dir CWD))
       (older (path->string (path-join sdir "2026-10-01T00-00-00-000Z_older.jsonl"))))
  (when (file-exists? tmp-agent-dir)
    (rmtree tmp-agent-dir))
  (path-mkdir sdir :parents #t :exist-ok #t)
  (save-session-header! older "older" "2026-10-01T00:00:00.000Z" CWD)
  (save-session-message! older "{\"role\":\"user\",\"content\":\"selected history\"}"
    "older-user" #f "2026-10-01T00:00:01.000Z")
  ;; --resume 选中文件：历史 + 本轮输入
  (let ((parsed `((no-session . #f) (continue . #f) (resume . #t) (session-file . ,older))))
    (let ((messages (setup-initial-messages parsed "next prompt" sdir)))
      (check (length messages) => 2)
      (check (cdr (assoc "content" (car messages))) => "selected history")))
  ;; 无恢复来源：仅本轮输入
  (let ((messages (setup-initial-messages '() "next prompt" sdir)))
    (check (length messages) => 1)
    (check (cdr (assoc "content" (car messages))) => "next prompt"))
  (rmtree tmp-agent-dir))

;; continue 恢复上一会话两条历史消息，并在末尾追加新输入
(let* ((tmp-agent-dir (path->string (path-join (path-temp-dir) "test-pi-session-continue-msgs")))
       (cfg `((agent-dir . ,tmp-agent-dir))))
  (when (file-exists? tmp-agent-dir)
    (rmtree tmp-agent-dir))
  (let-values (((sfile sdir)
                (setup-session-file '((no-session . #f) (continue . #f)) cfg CWD)))
    (let ((init-msgs (setup-initial-messages '((continue . #f)) "first query" sdir)))
      (check (length init-msgs) => 1)
      (check (cdr (assoc 'content (car init-msgs))) => "first query"))
    (save-session-message! sfile "{\"role\":\"user\",\"content\":\"first query\"}"
      "msg-1" #f "2026-10-17T08:00:01.000Z")
    (save-session-message! sfile "{\"role\":\"assistant\",\"content\":\"first response\"}"
      "msg-2" "msg-1" "2026-10-17T08:00:02.000Z")
    (let ((init-msgs (setup-initial-messages '((continue . #t)) "second query" sdir)))
      (check (length init-msgs) => 3)
      (check (cdr (assoc "content" (car init-msgs))) => "first query")
      (check (cdr (assoc "content" (cadr init-msgs))) => "first response")
      (check (cdr (assoc 'content (caddr init-msgs))) => "second query")))
  (rmtree tmp-agent-dir))

(check-report)
