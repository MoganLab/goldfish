(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (liii check)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii pi models builtin)
  (liii pi models route)
  (liii pi args)
  (liii pi config)
  (liii pi config record)
) ;import

(check-set-mode! 'report-failed)

;; [022] --model 跨 provider 自动路由 验收脚本

(display "=== 验收点 1: 路由函数单元与行为断言 ===\n")

;; 1.1 内置受支持 provider 列表
(check-true (pair? (member "google" PI_SUPPORTED_PROVIDERS)))
(check-true (pair? (member "kimi-coding" PI_SUPPORTED_PROVIDERS)))
(check-true (pair? (member "deepseek" PI_SUPPORTED_PROVIDERS)))
(check-true (pair? (member "glm" PI_SUPPORTED_PROVIDERS)))
(check-true (pair? (member "anthropic" PI_SUPPORTED_PROVIDERS)))

;; 1.2 内置模型识别 (pi-provider-has-model?)
(check-true (pi-provider-has-model? "kimi-coding" "k3" '()))
(check-true (pi-provider-has-model? "kimi-coding" "kimi-for-coding" '()))
(check-true (pi-provider-has-model? "google" "gemini-2.5-flash" '()))
(check-true (pi-provider-has-model? "glm" "glm-4-plus" '()))
(check-true (pi-provider-has-model? "deepseek" "deepseek-chat" '()))
(check-false (pi-provider-has-model? "google" "k3" '()))
(check-false (pi-provider-has-model? "kimi-coding" "non-existent" '()))

;; 1.3 跨 provider 自动探测 (pi-find-model-provider)
(check (pi-find-model-provider "k3" '()) => "kimi-coding")
(check (pi-find-model-provider "gemini-2.5-flash" '()) => "google")
(check (pi-find-model-provider "glm-4-plus" '()) => "glm")
(check (pi-find-model-provider "deepseek-chat" '()) => "deepseek")
(check (pi-find-model-provider "claude-3-7-sonnet-20250219" '()) => "anthropic")
(check (pi-find-model-provider "non-existent-anywhere" '()) => #f)

;; 1.4 解析路由 (pi-route-provider-and-model)
;; 1.4.1 未指定 provider，指定 k3 -> 自动选 kimi-coding
(let-values (((p m) (pi-route-provider-and-model #f "k3" '() '() '())))
  (check p => "kimi-coding")
  (check m => "k3")
) ;let-values

;; 1.4.2 未指定 provider，未指定 model -> 取默认 (google, gemini-3.8-flash)
(let-values (((p m) (pi-route-provider-and-model #f #f '() '() '())))
  (check p => "google")
  (check m => "gemini-3.8-flash")
) ;let-values

;; 1.4.3 显式指定 provider 时以用户指定为准 (google, k3)
(let-values (((p m) (pi-route-provider-and-model "google" "k3" '() '() '())))
  (check p => "google")
  (check m => "k3")
) ;let-values

;; 1.4.4 未命中模型仍走当前 provider 兜底
(let-values (((p m) (pi-route-provider-and-model #f "unknown-model-xyz" '() '() '())))
  (check p => "google")
  (check m => "unknown-model-xyz")
) ;let-values

(display "=== 验收点 2: 独立单元测试文件全量通过 ===\n")
(check (run "bin/gf test tools/pi/tests/liii/pi/models/builtin/PI_SUPPORTED_PROVIDERS-test.scm"
         :timeout 30
       ) ;run
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi/models/builtin/PI_PROVIDER_BUILTIN_MODELS-test.scm"
         :timeout 30
       ) ;run
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi/models/route/pi-provider-default-model-test.scm"
         :timeout 30
       ) ;run
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi/models/route/pi-provider-has-model-test.scm"
         :timeout 30
       ) ;run
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi/models/route/pi-find-model-provider-test.scm"
         :timeout 30
       ) ;run
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi/models/route/pi-ensure-catalog-meta-test.scm"
         :timeout 30
       ) ;run
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi/models/route/pi-route-provider-and-model-test.scm"
         :timeout 30
       ) ;run
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi-config-test.scm" :timeout 30)
  =>
  0
) ;check

(display "=== 验收点 3: tools/pi 单元测试套件全量通过 ===\n")
(check (run "bin/gf test tools/pi/tests/liii/" :timeout 120) => 0)

(display "=== 验收点 4: 跨 provider 自动路由真实冒烟（k3 -> Kimi）===\n"
) ;display
(let* ((tmp-out "/tmp/pi-022-smoke-k3.txt")
       (ret (run (string-append "sh -c 'bin/gf pi -p --model k3 \"你是什么模型?\" > "
                   tmp-out
                   " 2>&1'"
                 ) ;string-append
              :timeout 60
            ) ;run
       ) ;ret
       (out (if (file-exists? tmp-out) (path-read-text tmp-out) ""))
      ) ;
  (check ret => 0)
  (check-true (or (string-contains? out "Kimi") (string-contains? out "kimi")))
  (when (file-exists? tmp-out)
    (delete-file tmp-out)
  ) ;when
) ;let*

(display "=== 验收点 5: 未命中模型走当前 provider 兜底并输出 Warning ===\n"
) ;display
(let* ((tmp-out "/tmp/pi-022-smoke-fallback.txt")
       (ret (run (string-append "sh -c 'bin/gf pi -p --model non-existent-anywhere-xyz \"hi\" > "
                   tmp-out
                   " 2>&1'"
                 ) ;string-append
              :timeout 30
            ) ;run
       ) ;ret
       (out (if (file-exists? tmp-out) (path-read-text tmp-out) ""))
      ) ;
  ;; 未命中模型兜底 google 后调用不存在模型导致 API 报错（退出码 1）
  (check ret => 1)
  (check-true (string-contains? out
                "Warning: Model \"non-existent-anywhere-xyz\" not found in models store; using fallback defaults"
              ) ;string-contains?
  ) ;check-true
  (when (file-exists? tmp-out)
    (delete-file tmp-out)
  ) ;when
) ;let*

(display "=== 验收点 6: 门禁检查通过 ===\n")
(check (run "timeout 600 xmake b goldfish" :timeout 650) => 0)
(check (run "bin/gf fmt" :timeout 120) => 0)
(check (run "bin/gf check" :timeout 120) => 0)

(check-report)
