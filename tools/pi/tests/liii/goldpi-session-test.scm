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

;; 5. 测试 setup-session-file 与 setup-initial-messages
(let* ((tmp-agent-dir "/tmp/test-goldpi-session-agent")
       (cwd "/home/da/git/goldfish")
       (cfg `((agent-dir . ,tmp-agent-dir)))
       (sdir (get-session-dir tmp-agent-dir cwd)))
  (if (file-exists? tmp-agent-dir) (delete-file tmp-agent-dir))
  
  ;; 5.1 no-session: 不产生文件
  (let-values (((sfile dir) (setup-session-file '((no-session . #t)) cfg cwd)))
    (check sfile => #f)
    (check dir => #f))

  ;; 5.2 正常首次启动: 生成 session 文件与 header
  (let-values (((sfile dir) (setup-session-file '((no-session . #f) (continue . #f)) cfg cwd)))
    (check (string? sfile) => #t)
    (check (file-exists? sfile) => #t)
    ;; 验证写入一条 user 消息
    (let ((init-msgs (setup-initial-messages '((continue . #f)) "first query" dir)))
      (check (length init-msgs) => 1)
      (check (cdr (assoc 'content (car init-msgs))) => "first query")
      ;; persist 消息
      (persist-new-messages! sfile '() (list (car init-msgs)
                                             (list (cons 'role "assistant")
                                                   (cons 'content "first response"))))
      (let ((history (read-session-history sfile)))
        (check (length history) => 2)
        (check (cdr (assoc "content" (car history))) => "first query")
        (check (cdr (assoc "content" (cadr history))) => "first response"))

      ;; 5.3 continue: 恢复上一会话并在末尾追加新消息
      (let-values (((sfile2 dir2) (setup-session-file '((no-session . #f) (continue . #t)) cfg cwd)))
        (check sfile2 => sfile)
        (let ((init-msgs-2 (setup-initial-messages '((continue . #t)) "second query" dir2)))
          (check (length init-msgs-2) => 3)
          (check (cdr (assoc "content" (car init-msgs-2))) => "first query")
          (check (cdr (assoc "content" (cadr init-msgs-2))) => "first response")
          (check (cdr (assoc 'content (caddr init-msgs-2))) => "second query")))))
  (if (file-exists? sdir)
    (for-each delete-file (map (lambda (f) (if (string? f) f (path->string f))) (path-glob sdir "*")))))

(check-report)
