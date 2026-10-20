(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii check)
  (liii string)
  (liii path)
  (liii json)
  (liii subprocess)
  (liii pi bash)
  (liii pi)
) ;import

(check-set-mode! 'report-failed)

;; [051] 验收脚本：pi-config-test 环境卫生化
;; 逐条对应 tools/pi/devel/051.md 验收点：
;;   1) 门禁检查：构建 / fmt --check / check / doc
;;   2) 单文件行数 <= 300 行约束
;;   3) 相关单元测试验收（7 个修改/新增的测试文件）
;;   4) 全量回归真绿：271/271 零失败，彻底移除失败白名单
;;   5) 进程环境变量污染下复跑仍全绿（抗污染防回归断言）

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(define (%run-capture tag cmd timeout)
  (let ((tmp-out (string-append "tmp-051-" tag ".txt")))
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

(define (%run-ok? tag cmd timeout)
  (car (%run-capture tag cmd timeout))
) ;define

(display "=== 验收点 1: 门禁检查（构建 / fmt / check / doc） ===\n")
(check (%run-ok? "build" "timeout 300 xmake b goldfish" 300) => 0)
(check (%run-ok? "fmt-chk" "timeout 120 bin/gf fmt --check" 120) => 0)
(check (%run-ok? "check" "timeout 120 bin/gf check" 120) => 0)
(check (%run-ok? "doc" "timeout 120 bin/gf -I tools/pi doc --build-json" 120)
  =>
  0
) ;check

(display "=== 验收点 2: 任务 051 相关代码与测试文件行数约束 (<= 300 行) ===\n"
) ;display
(let ((files '("tools/pi/tests/liii/pi-config-test.scm"
               "tools/pi/tests/liii/pi-config-env-test.scm"
               "tools/pi/tests/liii/pi-cli-e2e-test.scm"
               "tools/pi/tests/liii/pi/pi-get-agent-dir-test.scm"
               "tools/pi/tests/liii/pi/pi-resolve-model-metadata-test.scm"
               "tools/pi/tests/liii/pi/models/catalog/pi-build-models-catalog-test.scm"
               "tools/pi/tests/liii/pi/models/route/pi-route-provider-and-model-test.scm"
               "tools/pi/devel/051.scm")
      ) ;files
     ) ;
  (for-each
    (lambda (f)
      (check-true
        (<= (length (string-split (path-read-text f) "\n")) 300)
      ) ;check-true
    ) ;lambda
    files
  ) ;for-each
) ;let

(display "=== 验收点 3: 任务 051 相关单元测试验收 ===\n")
(check (%run-ok? "t-cfg"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi-config-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-env"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi-config-env-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-cli-e2e"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi-cli-e2e-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-agent-dir"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/pi-get-agent-dir-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-meta"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/pi-resolve-model-metadata-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-catalog"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/models/catalog/pi-build-models-catalog-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-route"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/models/route/pi-route-provider-and-model-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check

(display "=== 验收点 4: 全量回归真绿（271/271 零失败，零失败白名单） ===\n"
) ;display
(let* ((res (%run-capture "all-regress" "timeout 300 bin/gf test tools/pi/tests/liii/" 300)
       ) ;res
       (rc (car res))
       (out (cdr res))
      ) ;
  (check rc => 0)
  (check-true (string-contains? out "Passed: 271"))
  (check-false (string-contains? out "Failed Test Files:"))
  (check-false (string-contains? out "pi-config-test.scm ... FAIL"))
) ;let*

(display "=== 验收点 5: 真实环境污染下复跑仍全绿（隔离防回归） ===\n"
) ;display
(let* ((pollute-cmd (string-append "env HOME=/nonexistent-home "
                      "PI_CODING_AGENT_DIR=/tmp/pi-polluted-agent-dir "
                      "ANTHROPIC_BASE_URL=https://polluted.example.com/anthropic "
                      "ANTHROPIC_AUTH_TOKEN=polluted-anthropic-token "
                      "ANTHROPIC_DEFAULT_SONNET_MODEL=polluted-sonnet-model "
                      "DEEPSEEK_BASE_URL=https://polluted.example.com/deepseek "
                      "DEEPSEEK_MODEL=polluted-deepseek-model "
                      "GF_PI_BASE_URL=https://polluted.example.com/gf-pi "
                      "GF_PI_MODEL=polluted-model "
                      "bin/gf test tools/pi/tests/liii/pi-config-test.scm"
                    ) ;string-append
       ) ;pollute-cmd
       (res (%run-capture "pollute-cfg" (string-append "timeout 120 " pollute-cmd) 120)
       ) ;res
      ) ;
  (check (car res) => 0)
  (check-true (string-contains? (cdr res) "PASS"))
) ;let*

(let* ((pollute-cmd (string-append "env HOME=/nonexistent-home "
                      "PI_CODING_AGENT_DIR=/tmp/pi-polluted-agent-dir "
                      "ANTHROPIC_BASE_URL=https://polluted.example.com/anthropic "
                      "DEEPSEEK_BASE_URL=https://polluted.example.com/deepseek "
                      "bin/gf test tools/pi/tests/liii/pi-config-env-test.scm"
                    ) ;string-append
       ) ;pollute-cmd
       (res (%run-capture "pollute-env" (string-append "timeout 120 " pollute-cmd) 120)
       ) ;res
      ) ;
  (check (car res) => 0)
  (check-true (string-contains? (cdr res) "PASS"))
) ;let*

(check-report)
