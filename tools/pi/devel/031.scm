(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (liii check)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii pi bash)
  (liii pi args)
  (liii pi modes)
  (liii pi help)
) ;import

(check-set-mode! 'report-failed)

;; [031] --session-timeout CLI 选项 验收脚本

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(display "=== 验收点 1: CLI 参数解析支持 ===\n")
;; 1.1 --session-timeout <正数>
(let ((res (parse-pi-args '("--session-timeout" "1800"))))
  (check (pi-args-ref res 'session-timeout) => 1800)
) ;let

;; 1.2 --session-timeout 0 (关闭兜底)
(let ((res (parse-pi-args '("--session-timeout" "0"))))
  (check (pi-args-ref res 'session-timeout) => 0)
) ;let

;; 1.3 默认未指定时为 #f
(let ((res (parse-pi-args '())))
  (check (pi-args-ref res 'session-timeout) => #f)
) ;let

;; 1.4 与其他选项及位置参数共存
(let ((res (parse-pi-args '("--session-timeout" "7000" "-p" "hello"))))
  (check (pi-args-ref res 'session-timeout) => 7000)
  (check (pi-args-ref res 'print) => #t)
  (check (pi-args-ref res 'messages) => '("hello"))
) ;let

(display "=== 验收点 2: 帮助文本与文档 ===\n")
;; 2.1 帮助文本包含 --session-timeout 说明
(let* ((tmp-out "tmp-031-help.txt")
       (ret (%run-shell (string-append "bin/gf pi --help > " tmp-out " 2>&1") 10))
       (out (if (file-exists? tmp-out) (path-read-text tmp-out) ""))
      ) ;
  (check ret => 0)
  (check-true (string-contains? out "--session-timeout <seconds>"))
  (check-true (string-contains? out
                "Total session timeout in seconds (default: 600, 0 to disable)"
              ) ;string-contains?
  ) ;check-true
  (when (file-exists? tmp-out)
    (delete-file tmp-out)
  ) ;when
) ;let*

;; 2.2 PI_SPEC.md 包含该选项用法
(let ((spec-text (path-read-text "PI_SPEC.md")))
  (check-true (string-contains? spec-text "--session-timeout 7000"))
  (check-true (string-contains? spec-text "--session-timeout 0"))
) ;let

(display "=== 验收点 3: 单元测试全量通过 ===\n")
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi-args-test.scm" 30) => 0)
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi-agent-test.scm" 30) => 0)
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi-cli-e2e-test.scm" 60)
  =>
  0
) ;check

(display "=== 验收点 4: 真实冒烟（基于真实模型/CLI 验证） ===\n"
) ;display
;; 4.1 --session-timeout 0 正常问答冒烟
(let* ((tmp-out "tmp-031-smoke-1.txt")
       (ret (%run-shell (string-append "bin/gf pi --print --session-timeout 0 \"1+1等于几？直接回答数字\" > "
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

;; 4.2 --session-timeout 30 正常问答冒烟（充分时间未超时）
(let* ((tmp-out "tmp-031-smoke-2.txt")
       (ret (%run-shell (string-append "bin/gf pi --print --session-timeout 30 \"1+1等于几？直接回答数字\" > "
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

;; 4.3 --session-timeout -1 立即超时生效（退出码 1 且报错人可读）
(let* ((tmp-out "tmp-031-smoke-3.txt")
       (ret (%run-shell (string-append "bin/gf pi --print --session-timeout -1 \"1+1等于几？\" > "
                          tmp-out
                          " 2>&1"
                        ) ;string-append
              30
            ) ;%run-shell
       ) ;ret
       (out (if (file-exists? tmp-out) (path-read-text tmp-out) ""))
      ) ;
  (check ret => 1)
  (check-true (string-contains? out "Session timeout exceeded"))
  (when (file-exists? tmp-out)
    (delete-file tmp-out)
  ) ;when
) ;let*

(display "=== 验收点 5: 门禁检查 ===\n")
(check (%run-shell "xmake b goldfish" 600) => 0)
(check (%run-shell "bin/gf fmt" 120) => 0)
;; 5.3 门禁：格式化与 check 检查
;; 注：在提交前的验收阶段，git 处于工作区未提交状态，故不在此步断言 git diff 退出码为 0，
;; 门禁检查通过 bin/gf fmt、bin/gf check 与 doc 构建保证
(check (%run-shell "bin/gf check" 120) => 0)
(check (%run-shell "bin/gf doc --build-json" 60) => 0)

(check-report)
