(import (scheme process-context)
        (liii check)
        (liii string)
        (liii path)
        (liii sys)
        (liii subprocess))

(check-set-mode! 'report-failed)

(define GF_BIN (executable))

(define FIXTURE_DIR
  (cond ((file-exists? "tests/fixtures/default")
         (path->string (path-resolve (path "tests/fixtures/default"))))
        ((file-exists? "tools/pi/tests/fixtures/default")
         (path->string (path-resolve (path "tools/pi/tests/fixtures/default"))))
        (else (path->string (path-resolve (path "../../tools/pi/tests/fixtures/default"))))))

(define (read-all p . maybe-stream)
  (let ((stream (if (null? maybe-stream) 'stdout (car maybe-stream))))
    (let loop ((acc ""))
      (let ((c (process-read p :stream stream)))
        (if (eof-object? c) acc (loop (string-append acc c)))))))

(define (exec-gf-pi args)
  (let* ((cmd (cons GF_BIN (cons "pi" args)))
         (env (append (get-environment-variables)
                      (list (cons "PI_CODING_AGENT_DIR" FIXTURE_DIR))))
         (p (process-spawn cmd :env env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 30000)))
    (unless code
      (process-kill p)
      (process-wait p 2000))
    (let ((out (read-all p 'stdout))
          (err (read-all p 'stderr)))
      (process-close p)
      (list code (if (string? out) out "") (if (string? err) err "")))))

;; 1. 全局列出：无 --provider 时跨全部 provider 列出
(let ((res (exec-gf-pi '("--list-models"))))
  (check (car res) => 0)
  (let ((out (cadr res)))
    ;; 表头
    (check-true (string-contains? out "provider"))
    (check-true (string-contains? out "model"))
    (check-true (string-contains? out "context"))
    (check-true (string-contains? out "max-out"))
    ;; 跨 provider 全部出现
    (check-true (string-contains? out "anthropic"))
    (check-true (string-contains? out "deepseek"))
    (check-true (string-contains? out "glm"))
    (check-true (string-contains? out "google"))
    (check-true (string-contains? out "kimi-coding"))
    (check-true (string-contains? out "minimax"))
    (check-true (string-contains? out "minimax-cn"))
    (check-true (string-contains? out "zai-coding-cn"))))

;; 2. --provider 过滤能力
(let ((res (exec-gf-pi '("--provider" "zai-coding-cn" "--list-models"))))
  (check (car res) => 0)
  (let ((out (cadr res)))
    (check-true (string-contains? out "zai-coding-cn"))
    (check-false (string-contains? out "anthropic"))
    (check-false (string-contains? out "google"))))

;; 3. 搜索过滤 (跨 provider)
(let ((res (exec-gf-pi '("--list-models" "flash"))))
  (check (car res) => 0)
  (let ((out (cadr res)))
    (check-true (string-contains? out "deepseek-flash"))
    (check-true (string-contains? out "gemini-3.8-flash"))))

;; 4. 子序列模糊搜索 (g25f -> gemini-2.5-flash)
(let ((res (exec-gf-pi '("--list-models" "g25f"))))
  (check (car res) => 0)
  (let ((out (cadr res)))
    (check-true (string-contains? out "gemini-2.5-flash"))))

;; 5. 搜索无命中提示
(let ((res (exec-gf-pi '("--list-models" "non-existent-pattern-xyz-037"))))
  (check (car res) => 0)
  (let ((out (cadr res)))
    (check-true (string-contains? out "No models matching \"non-existent-pattern-xyz-037\""))))

;; 6. 未知 provider 明确报错退出码 1
(let ((res (exec-gf-pi '("--provider" "unknown-prov-xyz-037" "--list-models"))))
  (check (car res) => 1)
  (let ((err (caddr res)))
    (check-true (string-contains? err "unknown-prov-xyz-037"))))

(check-report)
