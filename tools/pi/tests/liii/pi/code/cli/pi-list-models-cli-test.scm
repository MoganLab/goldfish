(import (scheme process-context)
  (liii check)
  (liii list)
  (liii string)
  (liii path)
  (liii sys)
  (liii subprocess)
) ;import

(check-set-mode! 'report-failed)

(define GF_BIN (executable))

(define FIXTURE_DIR
  (cond
   ((file-exists? "tests/fixtures/default")
    (path->string (path-resolve (path "tests/fixtures/default")))
   ) ;
   ((file-exists? "tools/pi/tests/fixtures/default")
    (path->string (path-resolve (path "tools/pi/tests/fixtures/default")))
   ) ;
   (else (path->string (path-resolve (path "../../tools/pi/tests/fixtures/default")))
   ) ;else
  ) ;cond
) ;define

(define PARTIAL_FIXTURE_DIR
  (cond
   ((file-exists? "tests/fixtures/user-config/full")
    (path->string (path-resolve (path "tests/fixtures/user-config/full")))
   ) ;
   ((file-exists? "tools/pi/tests/fixtures/user-config/full")
    (path->string (path-resolve (path "tools/pi/tests/fixtures/user-config/full")))
   ) ;
   (else (path->string (path-resolve (path "../../tools/pi/tests/fixtures/user-config/full"))
         ) ;path->string
   ) ;else
  ) ;cond
) ;define

(define (read-all p . maybe-stream)
  (let ((stream (if (null? maybe-stream) 'stdout (car maybe-stream))))
    (let loop
      ((acc ""))
      (let ((c (process-read p :stream stream)))
        (if (eof-object? c) acc (loop (string-append acc c)))
      ) ;let
    ) ;let
  ) ;let
) ;define

(define (%merge-env base overrides)
  (let ((override-keys (map car overrides)))
    (append overrides
      (filter
        (lambda (pair) (not (member (car pair) override-keys)))
        base
      ) ;filter
    ) ;append
  ) ;let
) ;define

(define (%find-sub-pos str sub)
  (string-position sub str)
) ;define

(define (exec-gf-pi args . opt-env-extra)
  (let* ((extra-env (apply append opt-env-extra))
         (cmd (cons GF_BIN (cons "pi" args)))
         (base-env (append (list (cons "PI_CODING_AGENT_DIR" FIXTURE_DIR))
                     (get-environment-variables)
                   ) ;append
         ) ;base-env
         (env (%merge-env base-env extra-env))
         (p (process-spawn cmd :env env :stdout 'pipe :stderr 'pipe))
         (out (read-all p 'stdout))
         (err (read-all p 'stderr))
         (code (process-wait p 30000))
        ) ;
    (unless code
      (process-kill p)
      (process-wait p 2000)
    ) ;unless
    (process-close p)
    (list code (if (string? out) out "") (if (string? err) err ""))
  ) ;let*
) ;define

;; 1. 全局列出：无 --provider 时跨全部 provider 列出
(let ((res (exec-gf-pi '("--list-models"))))
  (check (car res) => 0)
  (let ((out (cadr res)))
    ;; 表头
    (check-true (string-contains? out "provider"))
    (check-true (string-contains? out "model"))
    (check-true (string-contains? out "context"))
    (check-true (string-contains? out "max-out"))
    ;; 跨 provider 全部出现（anthropic / glm provider 已移除）
    (check-true (string-contains? out "deepseek"))
    (check-true (string-contains? out "glm"))
    (check-true (string-contains? out "google"))
    (check-true (string-contains? out "kimi-coding"))
    (check-true (string-contains? out "minimax-cn"))
    (check-true (string-contains? out "zai-coding-cn"))
    (check-false (string-contains? out "anthropic"))
  ) ;let
) ;let

;; 2. --provider 过滤能力
(let ((res (exec-gf-pi '("--provider" "zai-coding-cn" "--list-models"))))
  (check (car res) => 0)
  (let ((out (cadr res)))
    (check-true (string-contains? out "zai-coding-cn"))
    (check-false (string-contains? out "anthropic"))
    (check-false (string-contains? out "google"))
  ) ;let
) ;let

;; 3. 搜索过滤 (跨 provider)
(let ((res (exec-gf-pi '("--list-models" "flash"))))
  (check (car res) => 0)
  (let ((out (cadr res)))
    (check-true (string-contains? out "deepseek-v4-flash"))
    (check-true (string-contains? out "gemini-3.8-flash"))
  ) ;let
) ;let

;; 4. 子序列模糊搜索 (g38f -> gemini-3.8-flash)
(let ((res (exec-gf-pi '("--list-models" "g38f"))))
  (check (car res) => 0)
  (let ((out (cadr res)))
    (check-true (string-contains? out "gemini-3.8-flash"))
  ) ;let
) ;let

;; 5. 搜索无命中提示
(let ((res (exec-gf-pi '("--list-models" "non-existent-pattern-xyz-037"))))
  (check (car res) => 0)
  (let ((out (cadr res)))
    (check-true (string-contains? out "No models matching \"non-existent-pattern-xyz-037\"")
    ) ;check-true
  ) ;let
) ;let

;; 6. 未知 provider 明确报错退出码 1
(let ((res (exec-gf-pi '("--provider" "unknown-prov-xyz-037" "--list-models"))))
  (check (car res) => 1)
  (let ((err (caddr res)))
    (check-true (string-contains? err "unknown-prov-xyz-037"))
  ) ;let
) ;let

;; 7. 子进程管道输出（非 tty）默认无色
(let ((res (exec-gf-pi '("--list-models")
             (list (cons "PI_CODING_AGENT_DIR" PARTIAL_FIXTURE_DIR))
           ) ;exec-gf-pi
      ) ;res
     ) ;
  (check (car res) => 0)
  (let ((out (cadr res)))
    (check-false (string-contains? out "\x1b;"))
    (check-true (string-contains? out "●"))
    (check-true (string-contains? out "×"))
  ) ;let
) ;let

;; 8. NO_COLOR=1：无转义序列，且 ●/× 标记保留
(let ((res (exec-gf-pi '("--list-models")
             (list (cons "NO_COLOR" "1") (cons "PI_CODING_AGENT_DIR" PARTIAL_FIXTURE_DIR))
           ) ;exec-gf-pi
      ) ;res
     ) ;
  (check (car res) => 0)
  (let ((out (cadr res)))
    (check-false (string-contains? out "\x1b;"))
    (check-true (string-contains? out "●"))
    (check-true (string-contains? out "×"))
  ) ;let
) ;let

;; 9. FORCE_COLOR=1：包含 SGR 32（绿色）与 SGR 2（暗显）转义序列
(let ((res (exec-gf-pi '("--list-models")
             (list (cons "FORCE_COLOR" "1") (cons "PI_CODING_AGENT_DIR" PARTIAL_FIXTURE_DIR))
           ) ;exec-gf-pi
      ) ;res
     ) ;
  (check (car res) => 0)
  (let ((out (cadr res)))
    (check-true (string-contains? out "\x1b;[32m"))
    (check-true (string-contains? out "\x1b;[2m"))
    ;; 已激活行（●）排在未激活行（×）之前（置顶）
    (let ((pos-dot (%find-sub-pos out "●")) (pos-cross (%find-sub-pos out "×")))
      (check-true (and pos-dot pos-cross (< pos-dot pos-cross)))
    ) ;let
  ) ;let
) ;let

;; 10. 部分白名单沙箱目录：白名单外 provider 出现且排在白名单内之后
(let ((res (exec-gf-pi '("--list-models")
             (list (cons "PI_CODING_AGENT_DIR" PARTIAL_FIXTURE_DIR))
           ) ;exec-gf-pi
      ) ;res
     ) ;
  (check (car res) => 0)
  (let ((out (cadr res)))
    ;; 白名单外 provider（minimax-cn、deepseek）依然出现在全量输出中
    (check-true (string-contains? out "minimax-cn"))
    (check-true (string-contains? out "deepseek"))
    ;; 白名单内 provider（google）排在白名单外 provider（minimax-cn）之前
    (let ((pos-google (%find-sub-pos out "google"))
          (pos-minimax (%find-sub-pos out "minimax-cn"))
         ) ;
      (check-true (and pos-google pos-minimax (< pos-google pos-minimax)))
    ) ;let
  ) ;let
) ;let

;; 11. 白名单外内置 provider 通过 --provider 罗列（全量未激活 ×，不报 Unknown provider）
(let ((res (exec-gf-pi '("--provider" "minimax-cn" "--list-models")
             (list (cons "PI_CODING_AGENT_DIR" PARTIAL_FIXTURE_DIR))
           ) ;exec-gf-pi
      ) ;res
     ) ;
  (check (car res) => 0)
  (let ((out (cadr res)))
    (check-true (string-contains? out "minimax-cn"))
    (check-true (string-contains? out "×"))
    (check-false (string-contains? out "●"))
  ) ;let
) ;let


(check-report)
