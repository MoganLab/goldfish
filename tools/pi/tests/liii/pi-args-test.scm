(set! *load-path*
  (append (list "tools/pi" ".") *load-path*))

(import (liii check)
        (liii string)
        (liii pi)
        (liii pi-args))

(check-set-mode! 'report-failed)

;; 1. 基本选项解析
(let ((res (parse-pi-args '("-h"))))
  (check (pi-args-ref res 'help) => #t))

(let ((res (parse-pi-args '("--help"))))
  (check (pi-args-ref res 'help) => #t))

(let ((res (parse-pi-args '("-p" "hello"))))
  (check (pi-args-ref res 'print) => #t)
  (check (pi-args-ref res 'messages) => '("hello")))

(let ((res (parse-pi-args '("--print" "hello"))))
  (check (pi-args-ref res 'print) => #t))

(let ((res (parse-pi-args '("--mode" "json"))))
  (check (pi-args-ref res 'mode) => "json"))

(let ((res (parse-pi-args '("--mode" "text"))))
  (check (pi-args-ref res 'mode) => "text"))

(let ((res (parse-pi-args '("--model" "gemini-3.8-flash"))))
  (check (pi-args-ref res 'model) => "gemini-3.8-flash"))

(let ((res (parse-pi-args '("--provider" "google"))))
  (check (pi-args-ref res 'provider) => "google"))

(let ((res (parse-pi-args '("--proxy" "socks5h://127.0.0.1:1080"))))
  (check (pi-args-ref res 'proxy) => "socks5h://127.0.0.1:1080"))

(let ((res (parse-pi-args '("--tools" "read,bash,edit"))))
  (check (pi-args-ref res 'tools) => "read,bash,edit"))

(let ((res (parse-pi-args '("--max-turns" "10"))))
  (check (pi-args-ref res 'max-turns) => 10))

(let ((res (parse-pi-args '("--max-turns" "0"))))
  (check (pi-args-ref res 'max-turns) => 0))

(let ((res (parse-pi-args '("--timeout" "60"))))
  (check (pi-args-ref res 'timeout) => 60))

(let ((res (parse-pi-args '("--no-session"))))
  (check (pi-args-ref res 'no-session) => #t))

(let ((res (parse-pi-args '("--system-prompt" "system instructions"))))
  (check (pi-args-ref res 'system-prompt) => "system instructions"))

;; 2. 多次追加系统提示词
(let ((res (parse-pi-args '("--append-system-prompt" "prompt1"
                           "--append-system-prompt" "prompt2"))))
  (check (pi-args-ref res 'append-system-prompt) => '("prompt1" "prompt2")))

;; 3. 默认值
(let ((res (parse-pi-args '())))
  (check (pi-args-ref res 'help) => #f)
  (check (pi-args-ref res 'print) => #f)
  (check (pi-args-ref res 'mode) => #f)
  (check (pi-args-ref res 'model) => #f)
  (check (pi-args-ref res 'provider) => #f)
  (check (pi-args-ref res 'proxy) => #f)
  (check (pi-args-ref res 'tools) => #f)
  (check (pi-args-ref res 'max-turns) => 200)
  (check (pi-args-ref res 'timeout) => 120)
  (check (pi-args-ref res 'no-session) => #f)
  (check (pi-args-ref res 'system-prompt) => #f)
  (check (pi-args-ref res 'append-system-prompt) => '())
  (check (pi-args-ref res 'file-args) => '())
  (check (pi-args-ref res 'messages) => '()))

;; 4. @file 与 messages 区分
(let ((res (parse-pi-args '("@path/to/a.scm" "@b.txt" "first message" "second message"))))
  (check (pi-args-ref res 'file-args) => '("path/to/a.scm" "b.txt"))
  (check (pi-args-ref res 'messages) => '("first message" "second message")))

;; 5. -- 停止选项解析，支持 prompt 以 - 开头
(let ((res (parse-pi-args '("--model" "gemini-3.8-flash" "--" "-p" "--mode" "@doc.txt" "normal message"))))
  (check (pi-args-ref res 'model) => "gemini-3.8-flash")
  (check (pi-args-ref res 'print) => #f)
  (check (pi-args-ref res 'file-args) => '("doc.txt"))
  (check (pi-args-ref res 'messages) => '("-p" "--mode" "normal message")))

;; 5.1 --continue / -c 解析测试
(let ((res1 (parse-pi-args '("-c" "hello")))
      (res2 (parse-pi-args '("--continue" "world"))))
  (check (pi-args-ref res1 'continue) => #t)
  (check (pi-args-ref res1 'messages) => '("hello"))
  (check (pi-args-ref res2 'continue) => #t)
  (check (pi-args-ref res2 'messages) => '("world")))

;; 6. process-file-arguments 测试
(let* ((test-file (if (file-exists? "liii/pi-args.scm")
                      "liii/pi-args.scm"
                      "tools/pi/liii/pi-args.scm"))
       (block (process-file-arguments (list test-file))))
  (check-true (string? block))
  (check-true (> (string-length block) 0))
  (check-true (string-contains? block "<file name=\""))
  (check-true (string-contains? block "pi-args.scm"))
  (check-true (string-contains? block "</file>\n")))

;; 7. 空文件跳过测试
(let ((empty-result (process-file-arguments '())))
  (check empty-result => ""))

;; 7.1 文件不存在时报错测试（§2.1 / 缺陷6）
(check-catch 'file-error (process-file-arguments '("/nonexistent-file-xyz")))

;; 8. 结合 parse-pi-args 与 build-initial-prompt 进行完整 prompt 组装测试（覆盖 join 规则与 --）
(let* ((parsed (parse-pi-args '("--model" "gemini-3.8-flash" "--" "-h" "second message")))
       (messages (pi-args-ref parsed 'messages)))
  (let-values (((initial rest)
                (build-initial-prompt "stdin text\n"
                                     "<file name=\"/abs/path\">\nfile content\n</file>\n"
                                     messages)))
    ;; 验证 join 规则：无分隔符直接拼接
    (check initial => "stdin text\n<file name=\"/abs/path\">\nfile content\n</file>\n-h")
    (check rest => '("second message"))))

(check-report)
