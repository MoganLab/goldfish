(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (liii check)
  (liii error)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii os)
  (liii pi bash)
  (liii tui text-width)
  (liii tui loader)
  (liii pi modes tui)
  (liii pi modes tui state)
  (liii pi modes tui state tool)
  (liii pi modes tui viewport)
  (liii pi modes tui tool-card)
  (liii pi modes tui loop)
) ;import

(check-set-mode! 'report-failed)

;; [088] 验收脚本：工具调用状态卡片与执行中 Spinner 动态反馈

(define (%run-ok? tag cmd timeout)
  (let* ((tmp (string-append "tmp-088-" tag ".txt"))
         (ret (run (list (pi-find-bash-path) "-c" (string-append cmd " > " tmp " 2>&1"))
                :stdin   'null
                :timeout timeout
              ) ;run
         ) ;ret
        ) ;
    (when (file-exists? tmp)
      (delete-file tmp)
    ) ;when
    ret
  ) ;let*
) ;define

(define (%run-with-output tag cmd timeout)
  (let* ((tmp (string-append "tmp-088-" tag ".txt"))
         (ret (run (list (pi-find-bash-path) "-c" (string-append cmd " > " tmp " 2>&1"))
                :stdin   'null
                :timeout timeout
              ) ;run
         ) ;ret
         (out (if (file-exists? tmp) (path-read-text (path tmp)) ""))
        ) ;
    (when (file-exists? tmp)
      (delete-file tmp)
    ) ;when
    (cons ret out)
  ) ;let*
) ;define

(display "=== 验收点 1: 门禁检查（构建 / fmt / check / doc） ===\n")
(check (%run-ok? "build" "xmake b goldfish" 300) => 0)
(check (%run-ok? "fmt-chk" "bin/gf fmt --dry-run" 120) => 0)
(check (%run-ok? "check" "bin/gf check" 120) => 0)
(check (%run-ok? "doc" "bin/gf doc --build-json" 120) => 0)

(display "=== 验收点 2: 任务 088 代码与测试文件行数约束 (<= 300 行) ===\n"
) ;display
(for-each
  (lambda (f)
    (let ((lines (length (string-split (path-read-text (path f)) "\n"))))
      (check-true (<= lines 300))
    ) ;let
  ) ;lambda
  '("tools/pi/liii/pi/modes/tui/tool-card.scm"
    "tools/pi/liii/pi/modes/tui/state/tool.scm"
    "tools/pi/liii/pi/modes/tui/state.scm"
    "tools/pi/liii/pi/modes/tui/viewport.scm"
    "tools/pi/liii/pi/modes/tui/loop.scm" "tools/pi/liii/pi/modes/tui.scm"
    "tools/pi/tests/liii/pi/modes/tui/tool_card-test.scm"
    "tools/pi/devel/088.md" "tools/pi/devel/088.scm")
) ;for-each

(display "=== 验收点 3: ToolCard 状态模型、Spinner 集成与类型校验 ===\n"
) ;display
(let ((c (make-pi-tui-tool-card :id "c1" :tool-name "bash")))
  (check-true (pi-tui-tool-card? c))
  (check (tool-card-id c) => "c1")
  (check (tool-card-tool-name c) => "bash")
  (check (tool-card-args-summary c) => "")
  (check (tool-card-status c) => 'running)
  (check (tool-card-elapsed c) => 0.0)
  (check-true (spinner? (tool-card-spinner c)))
  (check (spinner-status (tool-card-spinner c)) => 'spinning)
) ;let

(let ((c (make-pi-tui-tool-card
           :id             "c2"
           :tool-name      "read"
           :args-summary   "path: file.txt"
           :status         'success
           :elapsed        0.42
           :result-summary "10 lines read"
         ) ;make-pi-tui-tool-card
      ) ;c
     ) ;
  (check (tool-card-id c) => "c2")
  (check (tool-card-tool-name c) => "read")
  (check (tool-card-status c) => 'success)
  (check (tool-card-elapsed c) => 0.42)
  (check (tool-card-result-summary c) => "10 lines read")
) ;let

;; 非法参数拦截
(check-catch 'type-error (make-pi-tui-tool-card :id 123 :tool-name "bash"))
(check-catch 'type-error (make-pi-tui-tool-card :id "t" :tool-name 'bash))
(check-catch 'type-error
  (make-pi-tui-tool-card :id "t" :tool-name "b" :status 'unknown)
) ;check-catch
(check-catch 'type-error
  (make-pi-tui-tool-card :id "t" :tool-name "b" :elapsed "0.5s")
) ;check-catch

;; 状态流转与 Spinner tick
(let ((c (make-pi-tui-tool-card :id "c3" :tool-name "bash" :args-summary "git status")
      ) ;c
     ) ;
  (let ((f0 (spinner-current-frame (tool-card-spinner c))))
    (tool-card-tick! c)
    (check-true
      (not (string=? f0 (spinner-current-frame (tool-card-spinner c))))
    ) ;check-true
  ) ;let
  (tool-card-complete! c 0.35 "On branch main")
  (check (tool-card-status c) => 'success)
  (check (tool-card-elapsed c) => 0.35)
  (check (tool-card-result-summary c) => "On branch main")
  (check-true
    (not (eq? (spinner-status (tool-card-spinner c)) 'spinning))
  ) ;check-true
  (tool-card-fail! c 1.2 "target failed")
  (check (tool-card-status c) => 'failed)
  (check (tool-card-elapsed c) => 1.2)
) ;let

(display "=== 验收点 4: tool_execution_start/end 事件处理与状态机集成 ===\n"
) ;display
(let* ((st (make-pi-tui-state))
       (args-table (list (cons "command" "cargo build --release")))
      ) ;
  (state-clear-dirty! st)
  (let ((c (state-handle-tool-start! st "c5" "bash" args-table)))
    (check-true (pi-tui-tool-card? c))
    (check-true (state-dirty? st))
    (check (tool-card-status c) => 'running)
    (check (tool-card-tool-name c) => "bash")
    (check (tool-card-args-summary c) => "cargo build --release")
    (check (eq? (state-active-tool-card st) c) => #t)
  ) ;let

  ;; 处于运行态时 tick
  (state-clear-dirty! st)
  (check-true (state-tick-active-tools! st))
  (check-true (state-dirty? st))

  ;; 结束事件 - 成功
  (state-clear-dirty! st)
  (state-handle-tool-end! st "c5" "Compiling success" #f 2.4)
  (check-true (state-dirty? st))
  (let ((c (car (reverse (state-messages st)))))
    (check (tool-card-status c) => 'success)
    (check (tool-card-elapsed c) => 2.4)
    (check (tool-card-result-summary c) => "Compiling success")
  ) ;let
  (check (state-active-tool-card st) => #f)

  ;; 结束事件 - 失败
  (state-handle-tool-start! st "c6" "read" '(("path" . "/root/secret")))
  (state-handle-tool-end! st "c6" "Permission denied" #t 0.1)
  (let ((c (car (reverse (state-messages st)))))
    (check (tool-card-status c) => 'failed)
    (check (tool-card-elapsed c) => 0.1)
    (check (tool-card-result-summary c) => "Permission denied")
  ) ;let
) ;let*

(display "=== 验收点 5: 渲染格式规范（运行态/成功态/失败态与超长折行） ===\n"
) ;display
;; 运行态格式：[⠋] bash: git status
(let* ((c (make-pi-tui-tool-card :id "t1" :tool-name "bash" :args-summary "git status")
       ) ;c
       (lines (render-tool-card c 80))
      ) ;
  (check (length lines) => 1)
  (let ((plain (strip-ansi (car lines))))
    (check-true (string-contains plain "bash: git status"))
    (check-true (string-contains plain "["))
    (check-true (string-contains plain "]"))
  ) ;let
) ;let*

;; 成功态格式：[✓] bash (0.4s) 与下方结果摘要
(let* ((c (make-pi-tui-tool-card
            :id             "t2"
            :tool-name      "bash"
            :args-summary   "git status"
            :status         'success
            :elapsed        0.4
            :result-summary "branch clean"
          ) ;make-pi-tui-tool-card
       ) ;c
       (lines (render-tool-card c 80))
      ) ;
  (check (length lines) => 2)
  (let ((header (strip-ansi (car lines))) (body (strip-ansi (cadr lines))))
    (check-true (string-contains header "[✓]"))
    (check-true (string-contains header "bash (0.4s)"))
    (check-true (string-contains body "branch clean"))
  ) ;let
) ;let*

;; 失败态格式：[✗] bash: failed (1.0s) 与下方错误摘要
(let* ((c (make-pi-tui-tool-card
            :id             "t3"
            :tool-name      "bash"
            :args-summary   "make"
            :status         'failed
            :elapsed        1.0
            :result-summary "exit status 2"
          ) ;make-pi-tui-tool-card
       ) ;c
       (lines (render-tool-card c 80))
      ) ;
  (check (length lines) => 2)
  (let ((header (strip-ansi (car lines))) (body (strip-ansi (cadr lines))))
    (check-true (string-contains header "[✗]"))
    (check-true (string-contains header "bash: failed (1.0s)"))
    (check-true (string-contains body "exit status 2"))
  ) ;let
) ;let*

;; 视口集成渲染
(let* ((c (make-pi-tui-tool-card :id "t4" :tool-name "ls" :status 'success :elapsed 0.1)
       ) ;c
       (msgs (list (cons 'user "run ls") c))
       (rendered (render-messages-viewport msgs 60 10 0))
      ) ;
  (check-true (>= (length rendered) 2))
  (check-true (string-contains (strip-ansi (cadr rendered)) "ls (0.1s)"))
) ;let*

(display "=== 验收点 6: 门面导出接口完整性 ===\n")
(for-each (lambda (proc) (check-true (procedure? proc)))
  (list make-pi-tui-tool-card pi-tui-tool-card? tool-card-id tool-card-tool-name
    tool-card-args-summary tool-card-status tool-card-elapsed
    tool-card-result-summary tool-card-spinner tool-card-start-time
    tool-card-tick! tool-card-complete! tool-card-fail! render-tool-card
    format-tool-args-summary format-tool-result-summary state-active-tool-card
    state-handle-tool-start! state-tick-active-tools! state-handle-tool-end!
  ) ;list
) ;for-each

(display "=== 验收点 7: 真实冒烟（非 TTY 下启动 bin/gf pi 打印帮助并以 0 退出） ===\n"
) ;display
(let ((res (%run-with-output "smoke" "bin/gf pi" 30)))
  (check (car res) => 0)
  (check-true (string-contains (cdr res) "gf pi [options]"))
) ;let

(display "=== 验收点 8: 全量测试套件执行并断言总计数与通过数（Total:  329, Passed: 329） ===\n"
) ;display
(let ((res (%run-with-output "all-tests" "bin/gf test tools/pi/tests/liii/" 300)))
  (check (car res) => 0)
  (let ((out (cdr res)))
    (check-true (string-contains out "Total:  329"))
    (check-true (string-contains out "Passed: 329"))
  ) ;let
) ;let

(check-report)
