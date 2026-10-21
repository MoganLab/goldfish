(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii check)
  (liii error)
  (liii string)
  (liii path)
  (liii sys)
  (liii subprocess)
  (liii os)
  (liii pi bash)
  (liii pi tools)
  (liii pi args)
  (liii pi agent tools)
) ;import

(check-set-mode! 'report-failed)

;; [079] 验收脚本：工具调用 timeout 语义重定：对齐原生 pi + 默认 600s

(define (%run-ok? tag cmd timeout)
  (let ((tmp (string-append "tmp-079-" tag ".txt")))
    (let ((ret (run (list (pi-find-bash-path) "-c" (string-append cmd " > " tmp " 2>&1"))
                 :stdin   'null
                 :timeout timeout
               ) ;run
          ) ;ret
         ) ;
      (when (file-exists? tmp)
        (delete-file tmp)
      ) ;when
      ret
    ) ;let
  ) ;let
) ;define

(display "=== 验收点 1: 门禁检查（构建 / fmt / check / doc） ===\n")
(check (%run-ok? "build" "xmake b goldfish" 300) => 0)
(check (%run-ok? "fmt-chk" "bin/gf fmt --dry-run" 120) => 0)
(check (%run-ok? "check" "bin/gf check" 120) => 0)
(check (%run-ok? "doc" "bin/gf doc --build-json" 120) => 0)

(display "=== 验收点 2: 任务 079 代码与测试文件行数约束 (<= 300 行) ===\n"
) ;display
(for-each
  (lambda (f)
    (let ((lines (length (string-split (path-read-text (path f)) "\n"))))
      (check-true (<= lines 300))
    ) ;let
  ) ;lambda
  '("tools/pi/liii/pi/bash.scm" "tools/pi/liii/pi/tools.scm"
    "tools/pi/liii/pi/args.scm" "tools/pi/liii/pi/modes/flow.scm"
    "tools/pi/liii/pi/agent.scm"
    "tools/pi/tests/liii/pi/pi-execute-bash-test.scm"
    "tools/pi/tests/liii/pi/pi-bash-schema-test.scm"
    "tools/pi/tests/liii/pi-tools-basic-test.scm" "tools/pi/devel/079.scm"
    "tools/pi/devel/079.md")
) ;for-each

(display "=== 验收点 3: 默认值提升至 600s 且 Schema 描述动态反映真实默认超时 ===\n"
) ;display
;; 3.1 默认 pi-bash-schema 描述反映 600s
(let* ((schema (pi-bash-schema))
       (props (cdr (assoc "properties" schema)))
       (to-prop (cdr (assoc "timeout" props)))
       (desc (cdr (assoc "description" to-prop)))
      ) ;
  (check-true (string-contains? desc "defaults to 600s"))
) ;let*

;; 3.2 显式传入默认超时时，schema 动态反映该默认值
(let* ((schema (pi-bash-schema 180))
       (props (cdr (assoc "properties" schema)))
       (to-prop (cdr (assoc "timeout" props)))
       (desc (cdr (assoc "description" to-prop)))
      ) ;
  (check-true (string-contains? desc "defaults to 180s"))
) ;let*

;; 3.3 get-tool-definition 缺省生成 600s 描述的 schema
(let* ((def (get-tool-definition "bash"))
       (schema (pi-tool-definition-parameters def))
       (props (cdr (assoc "properties" schema)))
       (to-prop (cdr (assoc "timeout" props)))
       (desc (cdr (assoc "description" to-prop)))
      ) ;
  (check-true (string-contains? desc "defaults to 600s"))
) ;let*

;; 3.4 get-tool-definition 支持自定义默认超时
(let* ((def (get-tool-definition "bash" 120))
       (schema (pi-tool-definition-parameters def))
       (props (cdr (assoc "properties" schema)))
       (to-prop (cdr (assoc "timeout" props)))
       (desc (cdr (assoc "description" to-prop)))
      ) ;
  (check-true (string-contains? desc "defaults to 120s"))
) ;let*

;; 3.5 pi-execute-bash 未指定参数时默认超时为 600s
(let-values (((out is-err) (pi-execute-bash '(("command" . "echo test")) :timed-out? #t)))
  (check is-err => #t)
  (check-true (string-contains? out "Command timed out after 600 seconds"))
) ;let-values

(display "=== 验收点 4: 模型显式值即生效值（解除 min clamp，对齐原生 pi） ===\n"
) ;display
;; 4.1 模型指定 1000s，哪怕 opts 默认超时为 1s，生效值仍为 1000s（不被 clamp 截断）
(let-values (((out is-err)
              (pi-execute-bash '(("command" . "sleep 5") ("timeout" . 1000))
                :timed-out? #t
                :timeout    1
              ) ;pi-execute-bash
             ) ;
            ) ;
  (check is-err => #t)
  (check-true (string-contains? out "Command timed out after 1000 seconds"))
) ;let-values

;; 4.2 模型指定更小超时 10s，生效值为 10s
(let-values (((out is-err)
              (pi-execute-bash '(("command" . "sleep 5") ("timeout" . 10))
                :timed-out? #t
                :timeout    600
              ) ;pi-execute-bash
             ) ;
            ) ;
  (check is-err => #t)
  (check-true (string-contains? out "Command timed out after 10 seconds"))
) ;let-values

;; 4.3 实际执行超时按模型指定生效
(let-values (((out is-err) (pi-execute-bash '(("command" . "sleep 5")
                                              ("timeout" . 1)))))
  (check is-err => #t)
  (check-true (string-contains? out "Command timed out after 1 seconds"))
) ;let-values

(display "=== 验收点 5: 非法 timeout 显式报错与 Sanity Cap 保护 ===\n"
) ;display
;; 5.1 常量 PI_MAX_TIMEOUT_SECONDS 验证 (2147483.647)
(check-true (> PI_MAX_TIMEOUT_SECONDS 2147483))
(check-true (< PI_MAX_TIMEOUT_SECONDS 2147484))

;; 5.2 非法值抛出异常（0、负数、非数值）
(check-catch 'error (pi-execute-bash '(("command" . "echo 1") ("timeout" . 0))))
(check-catch 'error
  (pi-execute-bash '(("command" . "echo 1") ("timeout" . -10)))
) ;check-catch
(check-catch 'error
  (pi-execute-bash '(("command" . "echo 1") ("timeout" . "invalid")))
) ;check-catch

;; 5.3 超过 sanity cap 抛出异常
(check-catch 'error
  (pi-execute-bash '(("command" . "echo 1") ("timeout" . 2147484)))
) ;check-catch

;; 5.4 工具调用层面结构化捕获非法 timeout 为 isError
(let ((res (execute-tool-call "bash" '(("command" . "echo 1") ("timeout" . 0)))))
  (check (pi-tool-result-is-error res) => #t)
  (check-true (string-contains? (pi-extract-tool-result-text res) "Invalid timeout")
  ) ;check-true
) ;let
(let ((res (execute-tool-call "bash" '(("command" . "echo 1") ("timeout" . -5)))))
  (check (pi-tool-result-is-error res) => #t)
  (check-true (string-contains? (pi-extract-tool-result-text res) "Invalid timeout")
  ) ;check-true
) ;let
(let ((res (execute-tool-call "bash" '(("command" . "echo 1")
                                       ("timeout" . 2147484))))
     ) ;
  (check (pi-tool-result-is-error res) => #t)
  (check-true (string-contains? (pi-extract-tool-result-text res) "Invalid timeout")
  ) ;check-true
) ;let

(display "=== 验收点 6: CLI 参数语义与 --timeout 0 冲突消除 ===\n")
;; 6.1 args 缺省为 #f
(let ((p (parse-pi-args '())))
  (check (pi-args-ref p 'timeout) => #f)
) ;let

;; 6.2 args 显式传入解析正确
(let ((p (parse-pi-args '("--timeout" "300"))))
  (check (pi-args-ref p 'timeout) => 300)
) ;let
(let ((p (parse-pi-args '("--timeout" "0"))))
  (check (pi-args-ref p 'timeout) => 0)
) ;let

;; 6.3 :timeout 0 作为缺省/重置选项安全回退到 600s
(let-values (((out is-err)
              (pi-execute-bash '(("command" . "echo test")) :timed-out? #t :timeout 0)
             ) ;
            ) ;
  (check is-err => #t)
  (check-true (string-contains? out "Command timed out after 600 seconds"))
) ;let-values

(display "=== 验收点 7: 其余内置工具超时统一与全量回归测试 ===\n"
) ;display
;; 7.1 内置工具基础定义与调用结构正常
(for-each
  (lambda (name)
    (let ((def (get-tool-definition name)))
      (check-true (pi-tool-definition? def))
    ) ;let
  ) ;lambda
  '("read" "write" "edit" "ls" "find" "grep" "web_search")
) ;for-each

;; 7.2 运行任务 079 相关单元测试
(check (%run-ok? "t-bash"
         "bin/gf test tools/pi/tests/liii/pi/pi-execute-bash-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-schema"
         "bin/gf test tools/pi/tests/liii/pi/pi-bash-schema-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-tools"
         "bin/gf test tools/pi/tests/liii/pi-tools-basic-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-args" "bin/gf test tools/pi/tests/liii/pi-args-test.scm" 120)
  =>
  0
) ;check

;; 7.3 全量单测套件回归
(check (%run-ok? "all-tests" "bin/gf test tools/pi/tests/liii/" 300) => 0)

(check-report)
