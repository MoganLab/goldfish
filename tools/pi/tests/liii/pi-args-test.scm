(set! *load-path*
  (append (list "tools/pi" ".") *load-path*))

(import (liii check)
        (liii string)
        (liii pi code cli args) (liii pi code core prompt)
        (liii pi code cli args))

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

(let ((res (parse-pi-args '("--exclude-tools" "bash,write,edit"))))
  (check (pi-args-ref res 'exclude-tools) => "bash,write,edit"))

(let ((res (parse-pi-args '("--max-turns" "10"))))
  (check (pi-args-ref res 'max-turns) => 10))

(let ((res (parse-pi-args '("--max-turns" "0"))))
  (check (pi-args-ref res 'max-turns) => 0))

(let ((res (parse-pi-args '("--timeout" "60"))))
  (check (pi-args-ref res 'timeout) => 60))

(let ((res (parse-pi-args '("--timeout" "0"))))
  (check (pi-args-ref res 'timeout) => 0))

(let ((res (parse-pi-args '("--timeout" "-5"))))
  (check (pi-args-ref res 'timeout) => -5))

(let ((res (parse-pi-args '("--session-timeout" "1800"))))
  (check (pi-args-ref res 'session-timeout) => 1800))

(let ((res (parse-pi-args '("--session-timeout" "0"))))
  (check (pi-args-ref res 'session-timeout) => 0))

(let ((res (parse-pi-args '("--no-session"))))
  (check (pi-args-ref res 'no-session) => #t))

(let ((res (parse-pi-args '("--no-tools"))))
  (check (pi-args-ref res 'no-tools) => #t))

(let ((res (parse-pi-args '("--no-builtin-tools"))))
  (check (pi-args-ref res 'no-tools) => #t))

(let ((res (parse-pi-args '("--system-prompt" "system instructions"))))
  (check (pi-args-ref res 'system-prompt) => "system instructions"))

;; 1.1 --compact / --no-auto-compact
(let ((res (parse-pi-args '("--compact"))))
  (check (pi-args-ref res 'compact) => #t)
  (check (pi-args-ref res 'no-auto-compact) => #f))

(let ((res (parse-pi-args '("--no-auto-compact" "hello"))))
  (check (pi-args-ref res 'no-auto-compact) => #t)
  (check (pi-args-ref res 'compact) => #f)
  (check (pi-args-ref res 'messages) => '("hello")))

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
  (check (pi-args-ref res 'exclude-tools) => #f)
  (check (pi-args-ref res 'no-tools) => #f)
  (check (pi-args-ref res 'max-turns) => 200)
  (check (pi-args-ref res 'timeout) => #f)
  (check (pi-args-ref res 'session-timeout) => #f)
  (check (pi-args-ref res 'no-session) => #f)
  (check (pi-args-ref res 'compact) => #f)
  (check (pi-args-ref res 'no-auto-compact) => #f)
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

;; 5.2 --resume / -r 解析测试
(let ((res1 (parse-pi-args '("-r")))
      (res2 (parse-pi-args '("--resume"))))
  (check (pi-args-ref res1 'resume) => #t)
  (check (pi-args-ref res2 'resume) => #t))

(check (pi-args-ref (parse-pi-args '()) 'resume) => #f)

;; 5.3 --list-models 解析测试
(let ((res1 (parse-pi-args '("--list-models")))
      (res2 (parse-pi-args '("--list-models" "kimi")))
      (res3 (parse-pi-args '("--list-models" "--provider" "google")))
      (res4 (parse-pi-args '("--provider" "google" "--list-models")))
      (res5 (parse-pi-args '("--list-models" "@doc.txt"))))
  (check (pi-args-ref res1 'list-models) => #t)
  (check (pi-args-ref res2 'list-models) => "kimi")
  (check (pi-args-ref res3 'list-models) => #t)
  (check (pi-args-ref res3 'provider) => "google")
  (check (pi-args-ref res4 'list-models) => #t)
  (check (pi-args-ref res4 'provider) => "google")
  (check (pi-args-ref res5 'list-models) => #t)
  (check (pi-args-ref res5 'file-args) => '("doc.txt")))

;; 6. process-file-arguments 测试
(let* ((test-file (if (file-exists? "liii/pi/code/cli/args.scm")
                      "liii/pi/code/cli/args.scm"
                      "tools/pi/liii/pi/code/cli/args.scm"))
       (block (process-file-arguments (list test-file))))
  (check-true (string? block))
  (check-true (> (string-length block) 0))
  (check-true (string-contains? block "<file name=\""))
  (check-true (string-contains? block "args.scm"))
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

;; 9. --thinking 解析与合法值校验测试
(let ((res1 (parse-pi-args '("--thinking" "off")))
      (res2 (parse-pi-args '("--thinking" "minimal")))
      (res3 (parse-pi-args '("--thinking" "low")))
      (res4 (parse-pi-args '("--thinking" "medium")))
      (res5 (parse-pi-args '("--thinking" "high")))
      (res6 (parse-pi-args '("--thinking" "xhigh")))
      (res7 (parse-pi-args '("--thinking" "max"))))
  (check (pi-args-ref res1 'thinking-level) => "off")
  (check (pi-args-ref res2 'thinking-level) => "minimal")
  (check (pi-args-ref res3 'thinking-level) => "low")
  (check (pi-args-ref res4 'thinking-level) => "medium")
  (check (pi-args-ref res5 'thinking-level) => "high")
  (check (pi-args-ref res6 'thinking-level) => "xhigh")
  (check (pi-args-ref res7 'thinking-level) => "max"))

;; --thinking-level 已移除，应落入 unknown-flags
(let ((res (parse-pi-args '("--thinking-level" "high"))))
  (check (pi-args-ref res 'thinking-level) => #f)
  (check (member "--thinking-level" (pi-args-ref res 'unknown-flags '()))
    => '("--thinking-level")))

(check-catch 'pi-arg-error (parse-pi-args '("--thinking" "invalid")))

;; 9.1 --name / -n 解析、规范化与参数校验
(let ((res1 (parse-pi-args '("--name" "  Refactor auth module  ")))
      (res2 (parse-pi-args '("-n" "Fix\nWindows\r\nbuild"))))
  (check (pi-args-ref res1 'name) => "Refactor auth module")
  (check (pi-args-ref res2 'name) => "Fix Windows build"))

(check (pi-args-ref (parse-pi-args '()) 'name) => #f)
(check-catch 'pi-arg-error (parse-pi-args '("--name")))
(check-catch 'pi-arg-error (parse-pi-args '("--name" "   ")))

;; 10. --session-timeout 解析与默认值测试
(let ((res1 (parse-pi-args '("--session-timeout" "7000" "hello")))
      (res2 (parse-pi-args '("--session-timeout" "0" "world"))))
  (check (pi-args-ref res1 'session-timeout) => 7000)
  (check (pi-args-ref res1 'messages) => '("hello"))
  (check (pi-args-ref res2 'session-timeout) => 0)
  (check (pi-args-ref res2 'messages) => '("world")))

(check-report)
