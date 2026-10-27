(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii shutil)
  (liii pi code core session-manager)
  (liii pi code core session-name)
  (liii pi code core session-list)
) ;import

(check-set-mode! 'report-failed)

;; pi-read-session-info：从 JSONL 会话文件提取选择器摘要
;; （id/cwd/name/created/modified/message-count/first-message/all-messages-text），
;; 非 session 开头的文件返回 #f，损坏行被忽略

(define ROOT
  (path->string (path-join (path-temp-dir) "pi-read-session-info-test"))
) ;define
(define CURRENT (path->string (path-join ROOT "current")))
(define OLD-FILE (path->string (path-join CURRENT "z-old.jsonl")))
(define NEW-FILE (path->string (path-join CURRENT "a-new.jsonl")))

(when (file-exists? ROOT)
  (rmtree ROOT)
) ;when
(path-mkdir CURRENT :parents #t :exist-ok #t)

(save-session-header! OLD-FILE "old-id" "2026-10-20T08:00:00.000Z" "/work/current")
(save-session-message! OLD-FILE
  "{\"role\":\"user\",\"content\":\"old question\"}"
  "old-user"
  #f
  "2026-10-20T08:00:01.000Z"
) ;save-session-message!
(save-session-message! OLD-FILE
  "{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"old answer\"}]}"
  "old-assistant"
  "old-user"
  "2026-10-20T08:00:02.000Z"
) ;save-session-message!

(save-session-header! NEW-FILE "new-id" "2026-10-21T08:00:00.000Z" "/work/current")
(append-session-info! NEW-FILE "Named current session")
(save-session-message! NEW-FILE
  "{\"role\":\"user\",\"content\":\"new question\"}"
  "new-user"
  #f
  "2026-10-21T08:00:01.000Z"
) ;save-session-message!

;; 损坏行应被忽略，不能让整个读取失败
(let ((port (open-output-file (path->string (path-join CURRENT "broken.jsonl")))))
  (display "not json\n" port)
  (close-output-port port)
) ;let

;; 1. 多消息会话：文本投影、消息计数、modified 取最后一条消息时间戳
(let ((info (pi-read-session-info OLD-FILE)))
  (check (pi-session-info-path info) => OLD-FILE)
  (check (pi-session-info-id info) => "old-id")
  (check (pi-session-info-cwd info) => "/work/current")
  (check (pi-session-info-name info) => #f)
  (check (pi-session-info-first-message info) => "old question")
  (check (pi-session-info-all-messages-text info) => "old question old answer")
  (check (pi-session-info-message-count info) => 2)
  (check (pi-session-info-modified info) => "2026-10-20T08:00:02.000Z")
) ;let

;; 2. 命名会话：name 取自 session_info 条目
(let ((info (pi-read-session-info NEW-FILE)))
  (check (pi-session-info-name info) => "Named current session")
  (check (pi-session-info-modified info) => "2026-10-21T08:00:01.000Z")
) ;let

;; 3. 仅 header 的空会话：first-message 占位、modified 回退 header 时间戳
(let* ((empty-file (path->string (path-join CURRENT "empty.jsonl"))))
  (save-session-header! empty-file "empty-id" "2026-10-22T08:00:00.000Z" "/work/empty")
  (let ((info (pi-read-session-info empty-file)))
    (check (pi-session-info-first-message info) => "(no messages)")
    (check (pi-session-info-message-count info) => 0)
    (check (pi-session-info-modified info) => "2026-10-22T08:00:00.000Z")
  ) ;let
) ;let*

;; 4. 非 session 开头的 JSONL 与缺失文件都返回 #f
(check (pi-read-session-info (path->string (path-join CURRENT "broken.jsonl"))) => #f)
(check (pi-read-session-info (path->string (path-join CURRENT "missing.jsonl"))) => #f)

;; 5. 真实会话样本也应可读取（覆盖工具结果、多内容块等记录）
(let* ((relative "tests/fixtures/direct/sessions/--home-da-git-goldfish-tools-pi--")
       (fixture-dir (if (file-exists? relative) relative
                      (string-append "tools/pi/" relative)))
       (sample (path->string
                 (path-join fixture-dir
                   "2026-10-18T18-30-49-221Z_2F231627-07C6-4CC0-9360-B474C925C96A.jsonl"))))
  (when (file-exists? sample)
    (check-true (pi-session-info? (pi-read-session-info sample)))))

(rmtree ROOT)

(check-report)
