(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (liii check)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii pi bash)
  (liii pi args)
  (liii pi tools)
  (liii pi config)
  (liii pi gemini)
  (liii pi anthropic)
) ;import

(check-set-mode! 'report-failed)

;; [025] --no-tools / --no-builtin-tools 支持 验收脚本

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(display "=== 验收点 1: CLI 参数解析与别名支持 ===\n")
;; 1.1 --no-tools 解析为 #t
(let ((res (parse-pi-args '("--no-tools"))))
  (check (pi-args-ref res 'no-tools) => #t)
) ;let

;; 1.2 --no-builtin-tools 别名解析为 #t
(let ((res (parse-pi-args '("--no-builtin-tools"))))
  (check (pi-args-ref res 'no-tools) => #t)
) ;let

;; 1.3 默认未指定时为 #f
(let ((res (parse-pi-args '())))
  (check (pi-args-ref res 'no-tools) => #f)
) ;let

(display "=== 验收点 2: 关闭优先策略 (resolve-active-tools) ===\n")
(let ((args-no-tools (parse-pi-args '("--no-tools")))
      (args-no-builtin (parse-pi-args '("--no-builtin-tools")))
      (args-override-tools (parse-pi-args '("--tools" "read,bash" "--no-tools")))
      (args-override-builtin (parse-pi-args '("--tools"
                                              "read,bash"
                                              "--no-builtin-tools"))
      ) ;args-override-builtin
      (args-override-exclude (parse-pi-args '("--exclude-tools"
                                              "bash"
                                              "--no-tools")))
      (args-override-all (parse-pi-args '("--tools" "read,bash"
                                          "--exclude-tools" "bash" "--no-tools"))
      ) ;args-override-all
      (settings '((default-tools "read" "bash" "edit" "write")))
     ) ;
  ;; 2.1 仅指定 --no-tools 返回空列表
  (check (resolve-active-tools args-no-tools settings) => '())
  ;; 2.2 仅指定 --no-builtin-tools 返回空列表
  (check (resolve-active-tools args-no-builtin settings) => '())
  ;; 2.3 --tools 与 --no-tools 组合：关闭优先
  (check (resolve-active-tools args-override-tools settings) => '())
  ;; 2.4 --tools 与 --no-builtin-tools 组合：关闭优先
  (check (resolve-active-tools args-override-builtin settings) => '())
  ;; 2.5 --exclude-tools 与 --no-tools 组合：关闭优先
  (check (resolve-active-tools args-override-exclude settings) => '())
  ;; 2.6 --tools、--exclude-tools 与 --no-tools 共存：关闭优先
  (check (resolve-active-tools args-override-all settings) => '())
) ;let

;; 2.7 默认未指定时返回默认工具集
(let ((args-def (parse-pi-args '()))
      (settings '((default-tools "read" "bash" "edit" "write")))
     ) ;
  (check (resolve-active-tools args-def settings)
    =>
    '("read" "bash" "edit" "write")
  ) ;check
) ;let

;; 2.8 resolve-pi-config 端到端 no-tools 验证
(let* ((fixture-dir "tools/pi/tests/fixtures/fake-pi-dir")
       (args-1 (parse-pi-args '("--no-tools")))
       (args-2 (parse-pi-args '("--no-builtin-tools")))
       (env `(("PI_CODING_AGENT_DIR" . ,fixture-dir)))
       (cfg-1 (resolve-pi-config args-1 env))
       (cfg-2 (resolve-pi-config args-2 env))
      ) ;
  (check (cdr (assoc 'tools cfg-1)) => '())
  (check (cdr (assoc 'tools cfg-2)) => '())
) ;let*

(display "=== 验收点 3: API 请求体无 tools 字段与纯文本模式 ===\n"
) ;display
;; 3.1 Gemini: 无 tools 时请求体中不含 tools 与 toolConfig 字段
(let* ((contents #((("role" . "user") ("parts" . #((("text" . "hi")))))))
       (body (gemini-build-request-body contents "system" '()))
      ) ;
  (check (assoc "tools" body) => #f)
  (check (assoc "toolConfig" body) => #f)
) ;let*

;; 3.2 Anthropic: 无 tools 时请求体中不含 tools 字段
(let* ((body (anthropic-build-request-body "claude-3-7-sonnet-20250219"
               #((("role" . "user") ("content" . "hi")))
               "system"
               '()
               4096
             ) ;anthropic-build-request-body
       ) ;body
      ) ;
  (check (assoc "tools" body) => #f)
) ;let*

(display "=== 验收点 4: 帮助文本与文档 ===\n")
(let* ((tmp-out "tmp-025-help.txt")
       (ret (%run-shell (string-append "bin/gf pi --help > " tmp-out " 2>&1") 10))
       (out (if (file-exists? tmp-out) (path-read-text tmp-out) ""))
      ) ;
  (check ret => 0)
  (check-true (string-contains? out "--no-tools"))
  (check-true (string-contains? out "--no-builtin-tools"))
  (check-true (string-contains? out "Disable all tools"))
  (check-true (string-contains? out "Alias for --no-tools"))
  (when (file-exists? tmp-out)
    (delete-file tmp-out)
  ) ;when
) ;let*

;; 4.2 PI_SPEC.md 包含对应用法说明
(let ((spec-text (path-read-text "PI_SPEC.md")))
  (check-true (string-contains? spec-text "--no-tools"))
  (check-true (string-contains? spec-text "--no-builtin-tools"))
) ;let

(display "=== 验收点 5: 单元测试全量通过 ===\n")
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi-args-test.scm" 30) => 0)
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi-config-test.scm" 30)
  =>
  0
) ;check
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi/gemini-build-request-body-test.scm"
         30
       ) ;%run-shell
  =>
  0
) ;check
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi/anthropic-build-request-body-test.scm"
         30
       ) ;%run-shell
  =>
  0
) ;check

(display "=== 验收点 6: 真实冒烟（纯文本对话且无工具调用） ===\n"
) ;display
;; 6.1 --no-tools 纯文本问答
(let* ((tmp-out "tmp-025-smoke-1.txt")
       (ret (%run-shell (string-append "bin/gf pi --print --no-tools \"1+1等于几？直接回答数字\" > "
                          tmp-out
                          " 2>&1"
                        ) ;string-append
              60
            ) ;%run-shell
       ) ;ret
       (out (if (file-exists? tmp-out) (path-read-text tmp-out) ""))
      ) ;
  (check ret => 0)
  (check-true (string-contains? out "2"))
  (when (file-exists? tmp-out)
    (delete-file tmp-out)
  ) ;when
) ;let*

;; 6.2 --no-builtin-tools 纯文本问答
(let* ((tmp-out "tmp-025-smoke-2.txt")
       (ret (%run-shell (string-append "bin/gf pi --print --no-builtin-tools \"1+1等于几？直接回答数字\" > "
                          tmp-out
                          " 2>&1"
                        ) ;string-append
              60
            ) ;%run-shell
       ) ;ret
       (out (if (file-exists? tmp-out) (path-read-text tmp-out) ""))
      ) ;
  (check ret => 0)
  (check-true (string-contains? out "2"))
  (when (file-exists? tmp-out)
    (delete-file tmp-out)
  ) ;when
) ;let*

;; 6.3 --tools 与 --no-tools 组合（关闭优先）
(let* ((tmp-out "tmp-025-smoke-3.txt")
       (ret (%run-shell (string-append "bin/gf pi --print --tools read,bash --no-tools \"1+1等于几？直接回答数字\" > "
                          tmp-out
                          " 2>&1"
                        ) ;string-append
              60
            ) ;%run-shell
       ) ;ret
       (out (if (file-exists? tmp-out) (path-read-text tmp-out) ""))
      ) ;
  (check ret => 0)
  (check-true (string-contains? out "2"))
  (when (file-exists? tmp-out)
    (delete-file tmp-out)
  ) ;when
) ;let*

(display "=== 验收点 7: 门禁检查 ===\n")
(check (%run-shell "xmake b goldfish" 600) => 0)
(check (%run-shell "bin/gf fmt" 120) => 0)
(check (%run-shell "git diff --exit-code tools/pi/liii/ tools/pi/tests/" 30)
  =>
  0
) ;check
(check (%run-shell "bin/gf check" 120) => 0)
(check (%run-shell "bin/gf doc --build-json" 60) => 0)

(check-report)
