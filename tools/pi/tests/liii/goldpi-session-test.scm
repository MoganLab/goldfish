(import (liii check)
        (liii goldpi-session)
        (liii path)
        (scheme file))

(check-set-mode! 'report-failed)

;; 1. 测试 encode-cwd-to-session-dirname
(check (encode-cwd-to-session-dirname "/home/da/git/goldfish")
       => "--home-da-git-goldfish--")
(check (encode-cwd-to-session-dirname "/var/log")
       => "--var-log--")

;; 2. 测试 get-session-dir 与 make-session-file-path
(let* ((agent-dir "/tmp/test-agent")
       (cwd "/home/da/git/goldfish")
       (sdir (get-session-dir agent-dir cwd))
       (file-path (make-session-file-path sdir "2026-10-17T09:29:29.706Z" "test-id-123")))
  (check sdir => "/tmp/test-agent/sessions/--home-da-git-goldfish--")
  (check file-path => "/tmp/test-agent/sessions/--home-da-git-goldfish--/2026-10-17T09-29-29-706Z_test-id-123.jsonl"))

;; 3. 测试 save 与 read 往返
(let* ((tmp-dir "/tmp/test-goldpi-session-fixture")
       (_ (if (file-exists? tmp-dir) (delete-file tmp-dir) #t))
       (session-file (path->string (path-join "/tmp" "session-test.jsonl"))))
  (if (file-exists? session-file) (delete-file session-file))
  (save-session-header! session-file "id-1" "2026-10-17T08:00:00.000Z" "/tmp")
  (save-session-message! session-file "{\"role\":\"user\",\"content\":\"hello\"}" "msg-1" #f "2026-10-17T08:00:01.000Z")
  (save-session-message! session-file "{\"role\":\"assistant\",\"content\":\"hi!\"}" "msg-2" "msg-1" "2026-10-17T08:00:02.000Z")
  
  (let ((msgs (read-session-history session-file)))
    (check (length msgs) => 2)
    (check (cdr (assoc "role" (car msgs))) => "user")
    (check (cdr (assoc "content" (car msgs))) => "hello")
    (check (cdr (assoc "role" (cadr msgs))) => "assistant")
    (check (cdr (assoc "content" (cadr msgs))) => "hi!"))
  (delete-file session-file))

;; 4. 测试 find-latest-session-file
(let* ((tmp-dir "/tmp/test-goldpi-latest-dir")
       (_ (if (file-exists? tmp-dir) (path-rmdir tmp-dir) #t))
       (_ (path-mkdir tmp-dir))
       (f1 (path->string (path-join tmp-dir "2026-10-01T00-00-00-000Z_id1.jsonl")))
       (f2 (path->string (path-join tmp-dir "2026-10-02T00-00-00-000Z_id2.jsonl")))
       (f3 (path->string (path-join tmp-dir "2026-09-30T00-00-00-000Z_id0.jsonl"))))
  (save-session-header! f1 "id1" "2026-10-01T00:00:00.000Z" "/tmp")
  (save-session-header! f2 "id2" "2026-10-02T00:00:00.000Z" "/tmp")
  (save-session-header! f3 "id0" "2026-09-30T00:00:00.000Z" "/tmp")
  (let ((latest (find-latest-session-file tmp-dir)))
    (check latest => f2))
  (delete-file f1)
  (delete-file f2)
  (delete-file f3)
  (path-rmdir tmp-dir))

(check-report)
