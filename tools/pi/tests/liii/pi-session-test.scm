(import (liii check)
        (liii pi)
        (liii pi session)
        (liii os)
        (liii path)
        (liii json)
        (liii string)
        (scheme time)
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
  (check (if (os-windows?) (string-replace sdir "\\" "/") sdir)
         => "/tmp/test-agent/sessions/--home-da-git-goldfish--")
  (check (if (os-windows?) (string-replace file-path "\\" "/") file-path)
         => "/tmp/test-agent/sessions/--home-da-git-goldfish--/2026-10-17T09-29-29-706Z_test-id-123.jsonl"))

;; 3. 测试 save 与 read 往返
(let* ((tmp-dir (path->string (path-join (path-temp-dir) "test-pi-session-fixture")))
       (_ (if (file-exists? tmp-dir) (delete-file tmp-dir) #t))
       (session-file (path->string (path-join (path-temp-dir) "session-test.jsonl"))))
  (if (file-exists? session-file) (delete-file session-file))
  (save-session-header! session-file "id-1" "2026-10-17T08:00:00.000Z" (path->string (path-temp-dir)))
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
(let* ((tmp-dir (path->string (path-join (path-temp-dir) "test-pi-latest-dir")))
       (_ (if (file-exists? tmp-dir) (path-rmdir tmp-dir) #t))
       (_ (path-mkdir tmp-dir))
       (f1 (path->string (path-join tmp-dir "2026-10-01T00-00-00-000Z_id1.jsonl")))
       (f2 (path->string (path-join tmp-dir "2026-10-02T00-00-00-000Z_id2.jsonl")))
       (f3 (path->string (path-join tmp-dir "2026-09-30T00-00-00-000Z_id0.jsonl"))))
  (save-session-header! f1 "id1" "2026-10-01T00:00:00.000Z" (path->string (path-temp-dir)))
  (save-session-header! f2 "id2" "2026-10-02T00:00:00.000Z" (path->string (path-temp-dir)))
  (save-session-header! f3 "id0" "2026-09-30T00:00:00.000Z" (path->string (path-temp-dir)))
  (let ((latest (find-latest-session-file tmp-dir)))
    (check latest => f2))
  (delete-file f1)
  (delete-file f2)
  (delete-file f3)
  (path-rmdir tmp-dir))

;; 5. 测试 setup-session-file 与 setup-initial-messages
(let* ((tmp-agent-dir (path->string (path-join (path-temp-dir) "test-pi-session-agent")))
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
    ;; 验证文件名中包含符合 RFC 9562 的 UUIDv7 会话 ID
    (let* ((fname (path-name (path sfile)))
           (sep-idx (string-index-right fname #\_))
           (dot-idx (string-index-right fname #\.))
           (file-sid (substring fname (+ sep-idx 1) dot-idx)))
      (check (string-length file-sid) => 36)
      (check (char=? (string-ref file-sid 8) #\-) => #t)
      (check (char=? (string-ref file-sid 13) #\-) => #t)
      (check (char=? (string-ref file-sid 18) #\-) => #t)
      (check (char=? (string-ref file-sid 23) #\-) => #t)
      (check (char=? (string-ref file-sid 14) #\7) => #t)
      (check-true (and (memv (string-ref file-sid 19) '(#\8 #\9 #\a #\b #\A #\B)) #t))
      (let* ((hex-ts (string-append (substring file-sid 0 8) (substring file-sid 9 13)))
             (ts-ms (string->number hex-ts 16))
             (uuid-sec (/ ts-ms 1000.0))
             (now-sec (current-second))
             (diff (abs (- uuid-sec now-sec))))
        (check-true (< diff 120.0))))

    ;; 验证 session header 中的 id 符合 RFC 9562 UUIDv7
    (let* ((lines (path-read-lines sfile))
           (header-line (car lines))
           (header-json (string->json header-line))
           (header-id (cdr (assoc "id" header-json))))
      (check (string-length header-id) => 36)
      (check (char=? (string-ref header-id 8) #\-) => #t)
      (check (char=? (string-ref header-id 13) #\-) => #t)
      (check (char=? (string-ref header-id 18) #\-) => #t)
      (check (char=? (string-ref header-id 23) #\-) => #t)
      (check (char=? (string-ref header-id 14) #\7) => #t)
      (check-true (and (memv (string-ref header-id 19) '(#\8 #\9 #\a #\b #\A #\B)) #t))
      (let* ((hex-ts (string-append (substring header-id 0 8) (substring header-id 9 13)))
             (ts-ms (string->number hex-ts 16))
             (uuid-sec (/ ts-ms 1000.0))
             (now-sec (current-second))
             (diff (abs (- uuid-sec now-sec))))
        (check-true (< diff 120.0))))

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
        (check (cdr (assoc "content" (cadr history))) => "first response")
        (check-true (string? (find-last-session-message-id sfile))))

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

;; 6. 测试反序列化包含 bash 和 read 工具结果的消息（从 JSON 恢复 vector content 归一化为 list）
(let* ((tmp-dir (path->string (path-join (path-temp-dir) "test-pi-session-tool-results")))
       (session-file (path->string (path-join tmp-dir "tool-results-test.jsonl"))))
  (if (file-exists? tmp-dir) (path-rmdir tmp-dir))
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (save-session-header! session-file "sess-1071" "2026-10-21T08:00:00.000Z" tmp-dir)
  ;; 写入包含 bash 和 read 的 toolResult JSON 记录（JSON 数组在解码后为 vector）
  (save-session-message! session-file
    "{\"role\":\"user\",\"content\":\"执行测试命令与读取文件\"}"
    "msg-u1" #f "2026-10-21T08:00:01.000Z")
  (save-session-message! session-file
    "{\"role\":\"assistant\",\"content\":[{\"type\":\"toolCall\",\"id\":\"call_bash_1\",\"name\":\"bash\",\"arguments\":{\"command\":\"echo SECRET-1071\"}},{\"type\":\"toolCall\",\"id\":\"call_read_1\",\"name\":\"read\",\"arguments\":{\"path\":\"secret.txt\"}}]}"
    "msg-a1" "msg-u1" "2026-10-21T08:00:02.000Z")
  (save-session-message! session-file
    "{\"role\":\"toolResult\",\"toolCallId\":\"call_bash_1\",\"toolName\":\"bash\",\"content\":[{\"type\":\"text\",\"text\":\"SECRET-1071\\n\"}],\"isError\":false}"
    "msg-tr1" "msg-a1" "2026-10-21T08:00:03.000Z")
  (save-session-message! session-file
    "{\"role\":\"toolResult\",\"toolCallId\":\"call_read_1\",\"toolName\":\"read\",\"content\":[{\"type\":\"text\",\"text\":\"FILE-CONTENT-1071\"}],\"isError\":false}"
    "msg-tr2" "msg-tr1" "2026-10-21T08:00:04.000Z")

  (let ((msgs (read-session-history session-file)))
    (check (length msgs) => 4)
    (let ((tr-bash (caddr msgs)) (tr-read (cadddr msgs)))
      ;; 验证 bash 工具结果已正确归一化为 list 且文本非空保留
      (check (cdr (assoc "role" tr-bash)) => "toolResult")
      (check (cdr (assoc "toolCallId" tr-bash)) => "call_bash_1")
      (check (cdr (assoc "toolName" tr-bash)) => "bash")
      (check-true (list? (cdr (assoc "content" tr-bash))))
      (let ((c-bash (cdr (assoc "content" tr-bash))))
        (check (= (length c-bash) 1) => #t)
        (check (cdr (assoc "type" (car c-bash))) => "text")
        (check (cdr (assoc "text" (car c-bash))) => "SECRET-1071\n"))
      ;; 验证 read 工具结果已正确归一化为 list 且文本非空保留
      (check (cdr (assoc "role" tr-read)) => "toolResult")
      (check (cdr (assoc "toolCallId" tr-read)) => "call_read_1")
      (check (cdr (assoc "toolName" tr-read)) => "read")
      (check-true (list? (cdr (assoc "content" tr-read))))
      (let ((c-read (cdr (assoc "content" tr-read))))
        (check (= (length c-read) 1) => #t)
        (check (cdr (assoc "type" (car c-read))) => "text")
        (check (cdr (assoc "text" (car c-read))) => "FILE-CONTENT-1071"))))

  (delete-file session-file)
  (path-rmdir tmp-dir))

;; 7. 测试 persist-new-messages! 写入 record 并在恢复时正确归一化 content
(let* ((tmp-dir (path->string (path-join (path-temp-dir) "test-pi-session-persist-roundtrip")))
       (session-file (path->string (path-join tmp-dir "persist-test.jsonl"))))
  (if (file-exists? tmp-dir) (path-rmdir tmp-dir))
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (save-session-header! session-file "sess-rt" "2026-10-21T08:00:00.000Z" tmp-dir)
  (let* ((u-msg (pi-user-message "run test"))
         (tr-bash (pi-tool-result-message "c_bash" "bash" (list (pi-text-block "SECRET-1071")) #f))
         (tr-read (pi-tool-result-message "c_read" "read" (list (pi-text-block "FILE-1071")) #f)))
    (persist-new-messages! session-file '() (list u-msg tr-bash tr-read))
    (let ((history (read-session-history session-file)))
      (check (length history) => 3)
      (let ((h-bash (cadr history)) (h-read (caddr history)))
        (check (cdr (assoc "role" h-bash)) => "toolResult")
        (check-true (list? (cdr (assoc "content" h-bash))))
        (check (cdr (assoc "text" (car (cdr (assoc "content" h-bash))))) => "SECRET-1071")
        (check (cdr (assoc "role" h-read)) => "toolResult")
        (check-true (list? (cdr (assoc "content" h-read))))
        (check (cdr (assoc "text" (car (cdr (assoc "content" h-read))))) => "FILE-1071"))))
  (delete-file session-file)
  (path-rmdir tmp-dir))

(check-report)
