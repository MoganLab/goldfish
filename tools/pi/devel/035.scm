(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (liii check)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii pi bash)
  (liii pi auth)
) ;import

(check-set-mode! 'report-failed)

;; [035] KIMI_API_KEY env 兜底修复 验收脚本

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(display "=== 验收点 1: pi-resolve-provider-key 函数级优先级验证 (env > auth.json) ===\n"
) ;display
;; 1.1 kimi-coding: KIMI_API_KEY 优先于 auth.json
(let ((auth '(("kimi-coding" . "auth-key-kimi")))
      (env '(("KIMI_API_KEY" . "env-key-kimi")))
     ) ;
  (check (pi-resolve-provider-key "kimi-coding" auth env) => "env-key-kimi")
  (check (pi-resolve-provider-key 'kimi-coding auth env) => "env-key-kimi")
  ;; 环境变量为空或空白时回退至 auth.json
  (check (pi-resolve-provider-key "kimi-coding" auth '(("KIMI_API_KEY" . "")))
    =>
    "auth-key-kimi"
  ) ;check
  (check (pi-resolve-provider-key "kimi-coding" auth '(("KIMI_API_KEY" . "   ")))
    =>
    "auth-key-kimi"
  ) ;check
  (check (pi-resolve-provider-key "kimi-coding" auth '()) => "auth-key-kimi")
) ;let

;; 1.2 google: GEMINI_API_KEY 优先于 auth.json
(let ((auth '(("google" . "auth-key-gemini")))
      (env '(("GEMINI_API_KEY" . "env-key-gemini")))
     ) ;
  (check (pi-resolve-provider-key "google" auth env) => "env-key-gemini")
  (check (pi-resolve-provider-key 'google auth env) => "env-key-gemini")
  ;; 环境变量为空或空白时回退至 auth.json
  (check (pi-resolve-provider-key "google" auth '(("GEMINI_API_KEY" . "")))
    =>
    "auth-key-gemini"
  ) ;check
  (check (pi-resolve-provider-key "google" auth '()) => "auth-key-gemini")
) ;let

;; 1.3 anthropic: ANTHROPIC_API_KEY / ANTHROPIC_AUTH_TOKEN 优先于 auth.json
(let ((auth '(("anthropic" . "auth-key-ant")))
      (env-api '(("ANTHROPIC_API_KEY" . "env-api-ant")))
      (env-tok '(("ANTHROPIC_AUTH_TOKEN" . "env-tok-ant")))
     ) ;
  (check (pi-resolve-provider-key "anthropic" auth env-api) => "env-api-ant")
  (check (pi-resolve-provider-key "anthropic" auth env-tok) => "env-tok-ant")
  (check (pi-resolve-provider-key "anthropic" auth '()) => "auth-key-ant")
) ;let

;; 1.4 glm: ANTHROPIC_AUTH_TOKEN / GLM_API_KEY / ANTHROPIC_API_KEY 优先于 auth.json
(let ((auth '(("glm" . "auth-key-glm")))
      (env-glm '(("GLM_API_KEY" . "env-key-glm")))
      (env-ant '(("ANTHROPIC_AUTH_TOKEN" . "env-tok-glm")))
     ) ;
  (check (pi-resolve-provider-key "glm" auth env-glm) => "env-key-glm")
  (check (pi-resolve-provider-key "glm" auth env-ant) => "env-tok-glm")
  (check (pi-resolve-provider-key "glm" auth '()) => "auth-key-glm")
) ;let

;; 1.5 deepseek: DEEPSEEK_API_KEY 优先于 auth.json
(let ((auth '(("deepseek" . "auth-key-ds")))
      (env-ds '(("DEEPSEEK_API_KEY" . "env-key-ds") ("HOME" . "/non-existent")))
     ) ;
  (check (pi-resolve-provider-key "deepseek" auth env-ds) => "env-key-ds")
  (check (pi-resolve-provider-key "deepseek" auth '(("HOME" . "/non-existent")))
    =>
    "auth-key-ds"
  ) ;check
) ;let

(display "=== 验收点 2: 单元测试全量通过 ===\n")
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi/pi-resolve-provider-key-test.scm"
         30
       ) ;%run-shell
  =>
  0
) ;check

(display "=== 验收点 3: 真实冒烟（差距清单 #10 解决验证） ===\n"
) ;display
;; 3.1 当设置 KIMI_API_KEY=sk-bogus 时，应优先携带环境变量中的假 key，API 返回 401（未授权）而非 403/429
(let* ((tmp-out "tmp-035-smoke-bogus.txt")
       (ret (%run-shell (string-append "env KIMI_API_KEY=sk-bogus bin/gf pi --provider kimi-coding --model k3 -p test > "
                          tmp-out
                          " 2>&1"
                        ) ;string-append
              30
            ) ;%run-shell
       ) ;ret
       (out (if (file-exists? tmp-out) (path-read-text tmp-out) ""))
      ) ;
  (check ret => 1)
  (check-true (string-contains? out "401"))
  (check-false (string-contains? out "403"))
  (when (file-exists? tmp-out)
    (delete-file tmp-out)
  ) ;when
) ;let*

;; 3.2 当未设置 KIMI_API_KEY 时，回退读取 auth.json 中的 key（当前测试环境下返回 429/403 限额，而非 401）
(let* ((tmp-out "tmp-035-smoke-auth.txt")
       (ret (%run-shell (string-append "env -u KIMI_API_KEY bin/gf pi --provider kimi-coding --model k3 -p test > "
                          tmp-out
                          " 2>&1"
                        ) ;string-append
              30
            ) ;%run-shell
       ) ;ret
       (out (if (file-exists? tmp-out) (path-read-text tmp-out) ""))
      ) ;
  (check ret => 1)
  (check-false (string-contains? out "401"))
  (when (file-exists? tmp-out)
    (delete-file tmp-out)
  ) ;when
) ;let*

(display "=== 验收点 4: 门禁检查 ===\n")
(check (%run-shell "xmake b goldfish" 600) => 0)
(check (%run-shell "bin/gf fmt" 120) => 0)
(check (%run-shell "bin/gf check" 120) => 0)
(check (%run-shell "bin/gf doc --build-json" 60) => 0)

(check-report)
