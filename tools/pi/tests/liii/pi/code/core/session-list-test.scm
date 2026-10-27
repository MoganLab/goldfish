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

(define ROOT
  (path->string (path-join (path-temp-dir) "pi-session-list-test"))
) ;define
(define CURRENT (path->string (path-join ROOT "current")))
(define OTHER (path->string (path-join ROOT "other")))
(define OLD-FILE (path->string (path-join CURRENT "z-old.jsonl")))
(define NEW-FILE (path->string (path-join CURRENT "a-new.jsonl")))
(define OTHER-FILE (path->string (path-join OTHER "other.jsonl")))

(when (file-exists? ROOT)
  (rmtree ROOT)
) ;when
(path-mkdir CURRENT :parents #t :exist-ok #t)
(path-mkdir OTHER :parents #t :exist-ok #t)

;; 文件名顺序与最近消息时间相反，列表必须按活动时间而非文件名排序。
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

(save-session-header! OTHER-FILE "other-id" "2026-10-22T08:00:00.000Z" "/work/other")

;; 非会话 JSONL 与损坏行都应被忽略，不能让整个选择器失败。
(let ((port (open-output-file (path->string (path-join CURRENT "broken.jsonl")))))
  (display "not json\n" port)
  (close-output-port port)
) ;let

(let ((info (pi-read-session-info OLD-FILE)))
  (check (cdr (assoc 'path info)) => OLD-FILE)
  (check (cdr (assoc 'id info)) => "old-id")
  (check (cdr (assoc 'cwd info)) => "/work/current")
  (check (cdr (assoc 'name info)) => #f)
  (check (cdr (assoc 'first-message info)) => "old question")
  (check (cdr (assoc 'all-messages-text info)) => "old question old answer")
  (check (cdr (assoc 'message-count info)) => 2)
  (check (cdr (assoc 'modified info)) => "2026-10-20T08:00:02.000Z")
) ;let

(let ((info (pi-read-session-info NEW-FILE)))
  (check (cdr (assoc 'name info)) => "Named current session")
  (check (cdr (assoc 'modified info)) => "2026-10-21T08:00:01.000Z")
) ;let

(let ((info (pi-read-session-info OTHER-FILE)))
  (check (cdr (assoc 'first-message info)) => "(no messages)")
  (check (cdr (assoc 'message-count info)) => 0)
  (check (cdr (assoc 'modified info)) => "2026-10-22T08:00:00.000Z")
) ;let

(let ((sessions (pi-list-session-infos CURRENT)))
  (check (map (lambda (s) (cdr (assoc 'path s))) sessions)
    => (list NEW-FILE OLD-FILE))
) ;let

;; 可选递归模式供 All scope 使用；默认只枚举当前目录。
(let ((sessions (pi-list-session-infos ROOT #t)))
  (check (map (lambda (s) (cdr (assoc 'path s))) sessions)
    => (list OTHER-FILE NEW-FILE OLD-FILE))
) ;let

(check (pi-list-session-infos (path->string (path-join ROOT "missing"))) => '())

;; 真实会话样本也应可读取（覆盖工具结果、多内容块等记录）。
(let* ((relative "tests/fixtures/direct/sessions/--home-da-git-goldfish-tools-pi--")
       (fixture-dir (if (file-exists? relative) relative
                      (string-append "tools/pi/" relative)))
       (sample (path->string
                 (path-join fixture-dir
                   "2026-10-18T18-30-49-221Z_2F231627-07C6-4CC0-9360-B474C925C96A.jsonl"))))
  (when (file-exists? sample)
    (check-true (pair? (pi-read-session-info sample)))))

(rmtree ROOT)

(check-report)
