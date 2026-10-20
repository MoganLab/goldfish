(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  ;; 显式导入 (scheme process-context)：get-environment-variable 未被 (liii string)
  ;; 等导出，若不显式导入会解析到 define-library 泄漏的破损绑定并段错误（goldfish 内核 bug）
  (scheme process-context)
  (liii check)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii pi bash)
  (liii pi)
  (liii pi config)
) ;import

(check-set-mode! 'report-failed)

;; [032] 验收脚本：TDD 单测 / 3.5b/3.5c stub 429 预算窗口 / 函数级严格校验 / 文档 / 门禁

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(define (%run-capture tag cmd timeout)
  (let ((tmp-out (string-append "tmp-032-" tag ".txt")))
    (let ((ret (%run-shell (string-append cmd " > " tmp-out " 2>&1") timeout)))
      (let ((out
              (if (and (file-exists? tmp-out) (path-file? (path tmp-out)))
                (path-read-text tmp-out)
                ""
              ) ;if
            ) ;out
           ) ;
        (when (file-exists? tmp-out)
          (delete-file tmp-out)
        ) ;when
        (cons ret out)
      ) ;let
    ) ;let
  ) ;let
) ;define

(define (%fixture-dir name)
  (if (file-exists? (string-append "tests/fixtures/" name))
    (string-append "tests/fixtures/" name)
    (string-append "tools/pi/tests/fixtures/" name)
  ) ;if
) ;define

(define retry-provider-dir (%fixture-dir "retry-provider"))

(define default-dir (%fixture-dir "default"))

(display "=== 验收点 1: TDD 单测（resolver/record/嵌套解析/stub e2e） ===\n"
) ;display
(check (%run-shell "timeout 60 bin/gf test tools/pi/tests/liii/pi/pi-resolve-retry-params-test.scm"
         90
       ) ;%run-shell
  =>
  0
) ;check
(check (%run-shell "timeout 60 bin/gf test tools/pi/tests/liii/pi/pi-settings-record-test.scm"
         90
       ) ;%run-shell
  =>
  0
) ;check
(check (%run-shell "timeout 60 bin/gf test tools/pi/tests/liii/pi/pi-load-settings-test.scm"
         90
       ) ;%run-shell
  =>
  0
) ;check
;; pi-provider-test.scm：stub server 端到端，含 3.5b/3.5c 429 预算窗口用例
(check (%run-shell "timeout 120 bin/gf test tools/pi/tests/liii/pi-provider-test.scm"
         150
       ) ;%run-shell
  =>
  0
) ;check

(display "=== 验收点 2: stub server 429 预算窗口用例 3.5b/3.5c 存在性 ===\n"
) ;display
(let ((provider-test (path-read-text "tools/pi/tests/liii/pi-provider-test.scm"))
      (stub-server (path-read-text "tools/pi/tests/stub_gemini_server.py"))
     ) ;
  ;; 3.5b：顶层 max-retries=3 但 provider-max-retries=8，5 次 429 后成功
  (check-true (string-contains? provider-test "3.5b"))
  (check-true (string-contains? provider-test "429-5-then-ok"))
  (check-true (string-contains? provider-test "provider-max-retries . 8"))
  ;; 3.5c：无 provider 预算，按顶层 max-retries=2 快速失败报错
  (check-true (string-contains? provider-test "3.5c"))
  (check-true (string-contains? provider-test "429-always"))
  (check-true (string-contains? provider-test "max-retries . 2"))
  ;; stub server 提供对应端点
  (check-true (string-contains? stub-server "/429-5-then-ok"))
  (check-true (string-contains? stub-server "/429-always"))
) ;let

(display "=== 验收点 3: 函数级严格校验（嵌套解析 / 命中 / 缺省回退） ===\n"
) ;display

;; 3.1 retry-provider fixture：嵌套节解析不再被丢弃
(let* ((settings (pi-load-settings retry-provider-dir))
       (retry (cdr (assoc 'retry settings)))
      ) ;
  (check (cdr (assoc 'provider-max-retries retry)) => 12)
  (check (cdr (assoc "provider-max-retries" retry)) => 12)
  (check (pi-retry-config-provider-max-retries retry) => 12)
  ;; resolver 选取 provider 预算 12，顶层预算保持 3
  (let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
                (pi-resolve-retry-params settings #f)
               ) ;
              ) ;
    (check max-retries => 3)
    (check provider-max-retries => 12)
  ) ;let-values
) ;let*

;; 3.2 无嵌套节（default fixture）：provider 预算回退顶层 max-retries=3
(let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
              (pi-resolve-retry-params (pi-load-settings default-dir) #f)
             ) ;
            ) ;
  (check max-retries => 3)
  (check provider-max-retries => 3)
) ;let-values

;; 3.3 空 settings：全部缺省，provider 预算回退缺省 3
(let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
              (pi-resolve-retry-params '() #f)
             ) ;
            ) ;
  (check max-retries => 3)
  (check base-delay-ms => 2000)
  (check max-agent-delay-ms => 60000)
  (check provider-max-retries => 3)
) ;let-values

;; 3.4 settings 解析后的原始嵌套形状（字符串键路径）
(let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
              (pi-resolve-retry-params
                '(("retry"
                   ("max-retries" . 3)
                   ("provider" ("max-retries" . 12))))
                #f
              ) ;pi-resolve-retry-params
             ) ;
            ) ;
  (check max-retries => 3)
  (check provider-max-retries => 12)
) ;let-values

;; 3.5 enabled=#f：顶层与 provider 预算均为 0
(let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
              (pi-resolve-retry-params
                '((retry (enabled . #f)
                    (max-retries . 3)
                    (provider (max-retries . 12))))
                #f
              ) ;pi-resolve-retry-params
             ) ;
            ) ;
  (check max-retries => 0)
  (check provider-max-retries => 0)
) ;let-values

;; 3.6 record 路径：pi-retry-config 第 5 参 + pi-settings 包装
(let* ((retry (pi-retry-config #t 3 2000 60000 12))
       (settings (pi-settings "google" "gemini-3.8-flash" "off" #f retry #f))
      ) ;
  (check (pi-retry-config-provider-max-retries retry) => 12)
  (let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
                (pi-resolve-retry-params settings #f)
               ) ;
              ) ;
    (check max-retries => 3)
    (check provider-max-retries => 12)
  ) ;let-values
) ;let*

;; 3.7 opt-retry 覆盖：扁平 provider-max-retries 优先，未给则回退 opt-retry 顶层值
(let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
              (pi-resolve-retry-params '()
                '((enabled . #t) (max-retries . 3) (provider-max-retries . 8))
              ) ;pi-resolve-retry-params
             ) ;
            ) ;
  (check max-retries => 3)
  (check provider-max-retries => 8)
) ;let-values
(let-values (((max-retries base-delay-ms max-agent-delay-ms provider-max-retries)
              (pi-resolve-retry-params '()
                '((enabled . #t) (max-retries . 4) (base-delay-ms . 100))
              ) ;pi-resolve-retry-params
             ) ;
            ) ;
  (check max-retries => 4)
  (check provider-max-retries => 4)
) ;let-values

(display "=== 验收点 4: 文档（--help Retry 段 / PI_SPEC.md 差距清单 #9） ===\n"
) ;display
(let ((rc (%run-capture "help" "timeout 60 bin/gf pi --help" 90)))
  (check (car rc) => 0)
  (check-true (string-contains? (cdr rc) "retry.provider.maxRetries"))
  (check-true (string-contains? (cdr rc) "falling back to retry.maxRetries"))
  (check-true (string-contains? (cdr rc) "429/5xx/408/409/network"))
) ;let
(let ((spec (path-read-text "PI_SPEC.md")))
  (check-true (string-contains? spec "retry.provider.maxRetries"))
  (check-true (string-contains? spec "任务 032"))
) ;let

(display "=== 验收点 5: 门禁检查 ===\n")
(check (%run-shell "timeout 300 xmake b goldfish" 300) => 0)
(check (%run-shell "timeout 120 bin/gf fmt" 120) => 0)
(check (%run-shell "timeout 120 bin/gf check" 120) => 0)
(check (%run-shell "timeout 60 bin/gf doc --build-json" 60) => 0)

(check-report)
