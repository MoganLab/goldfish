(import (liii check)
  (liii path)
  (liii json)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

;; save 与 read 往返
(let ((session-file (path->string (path-join (path-temp-dir) "session-test.jsonl"))))
  (if (file-exists? session-file) (delete-file session-file))
  (save-session-header! session-file "id-1" "2026-10-17T08:00:00.000Z"
    (path->string (path-temp-dir)))
  (save-session-message! session-file "{\"role\":\"user\",\"content\":\"hello\"}"
    "msg-1" #f "2026-10-17T08:00:01.000Z")
  (save-session-message! session-file "{\"role\":\"assistant\",\"content\":\"hi!\"}"
    "msg-2" "msg-1" "2026-10-17T08:00:02.000Z")
  (let ((msgs (read-session-history session-file)))
    (check (length msgs) => 2)
    (check (cdr (assoc "role" (car msgs))) => "user")
    (check (cdr (assoc "content" (car msgs))) => "hello")
    (check (cdr (assoc "role" (cadr msgs))) => "assistant")
    (check (cdr (assoc "content" (cadr msgs))) => "hi!"))
  (delete-file session-file))

;; 反序列化包含 bash 和 read 工具结果的消息，vector content 归一化为 list
(let* ((tmp-dir (path->string (path-join (path-temp-dir) "test-pi-session-tool-results")))
       (session-file (path->string (path-join tmp-dir "tool-results-test.jsonl"))))
  (if (file-exists? tmp-dir) (path-rmdir tmp-dir))
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (save-session-header! session-file "sess-1071" "2026-10-21T08:00:00.000Z" tmp-dir)
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
      (check (cdr (assoc "role" tr-bash)) => "toolResult")
      (check (cdr (assoc "toolCallId" tr-bash)) => "call_bash_1")
      (check (cdr (assoc "toolName" tr-bash)) => "bash")
      (check-true (list? (cdr (assoc "content" tr-bash))))
      (let ((c-bash (cdr (assoc "content" tr-bash))))
        (check (= (length c-bash) 1) => #t)
        (check (cdr (assoc "type" (car c-bash))) => "text")
        (check (cdr (assoc "text" (car c-bash))) => "SECRET-1071\n"))
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

(check-report)
