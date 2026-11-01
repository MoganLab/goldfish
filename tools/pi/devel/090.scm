(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (liii check)
  (liii error)
  (liii list)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii os)
  (liii tui text-width)
  (liii pi bash)
  (liii pi modes tui)
  (liii pi modes tui state)
  (liii pi modes tui editor)
  (liii pi modes tui confirm)
) ;import

(check-set-mode! 'report-failed)

;; [090] 验收脚本：人在回路敏感操作模态确认
(load "tools/pi/devel/090-support.scm")

(verify-090-gates!)
(verify-090-line-limits!)

(display "=== 验收点 3: 高危工具调用识别与分类 ===\n")
(check (tool-dangerous? "write" '(("path" . "a.scm") ("content" . "x"))) => #t)
(check (classify-tool-danger "write" '(("path" . "a.scm") ("content" . "x")))
  =>
  'high
) ;check
(check (tool-dangerous? "edit" '(("path" . "b.scm") ("edits"))) => #t)
(check (classify-tool-danger "edit" '(("path" . "b.scm") ("edits"))) => 'medium)
(check (tool-dangerous? "bash" '(("command" . "rm -rf /tmp/data"))) => #t)
(check (classify-tool-danger "bash" '(("command" . "rm -rf /tmp/data")))
  =>
  'critical
) ;check
(check (tool-dangerous? "bash" '(("command" . "git reset --hard HEAD~1")))
  =>
  #t
) ;check
(check (classify-tool-danger "bash" '(("command" . "git reset --hard HEAD~1")))
  =>
  'critical
) ;check
(check (tool-dangerous? "bash" '(("command" . "git push origin main --force")))
  =>
  #t
) ;check
(check (classify-tool-danger "bash" '(("command"
                                       . "git push origin main --force")))
  =>
  'critical
) ;check
(check (tool-dangerous? "bash" '(("command" . "git clean -fd"))) => #t)
(check (classify-tool-danger "bash" '(("command" . "kill -9 1234"))) => 'high)
(check (tool-dangerous? "read" '(("path" . "file.scm"))) => #f)
(check (tool-dangerous? "bash" '(("command" . "git status"))) => #f)
(check-catch 'type-error (classify-tool-danger 123 '()))
(check-catch 'type-error (classify-tool-danger "bash" "not-a-list"))

(display "=== 验收点 4: ConfirmDialog 数据模型与状态机整合 ===\n")
(let ((dlg (make-confirm-dialog
             :id             "c_test"
             :tool-name      "write"
             :args           '(("path" . "out.txt") ("content" . "d"))
             :danger-level   'high
             :target-summary "write: out.txt"
             :diff-preview   "d"
           ) ;make-confirm-dialog
      ) ;dlg
     ) ;
  (check-true (confirm-dialog? dlg))
  (check (confirm-dialog-id dlg) => "c_test")
  (check (confirm-dialog-tool-name dlg) => "write")
  (check (confirm-dialog-danger-level dlg) => 'high)
  (check (confirm-dialog-target-summary dlg) => "write: out.txt")
  (check (confirm-dialog-diff-preview dlg) => "d")
  (check (confirm-dialog-decision dlg) => 'pending)
  (confirm-dialog-set-decision! dlg 'allowed)
  (check (confirm-dialog-decision dlg) => 'allowed)
) ;let
(check-catch 'type-error (make-confirm-dialog :id 123 :tool-name "bash"))
(check-catch 'type-error (make-confirm-dialog :id "1" :tool-name 123))

(let ((st (make-pi-tui-state)))
  (check (state-confirm-dialog st) => #f)
  (check (state-status st) => 'idle)
  (let ((dlg (make-confirm-dialog
               :id           "c1"
               :tool-name    "bash"
               :args         '()
               :danger-level 'critical
             ) ;make-confirm-dialog
        ) ;dlg
       ) ;
    (state-request-confirm! st dlg)
    (check (state-status st) => 'confirm)
    (check-true (state-dirty? st))
    (check (eq? (state-confirm-dialog st) dlg) => #t)
  ) ;let
) ;let

(display "=== 验收点 5: 居中模态对话框渲染与自适应折行截断 ===\n"
) ;display
(let* ((dlg (make-confirm-dialog
              :id             "c_ui"
              :tool-name      "bash"
              :args           '(("command" . "rm -rf /"))
              :danger-level   'critical
              :target-summary "bash: rm -rf /"
              :diff-preview   "rm -rf /"
            ) ;make-confirm-dialog
       ) ;dlg
       (lines (render-confirm-dialog dlg 80 24))
       (combined (string-join lines "\n"))
      ) ;
  (check-true (pair? lines))
  (check-true (<= (length lines) 24))
  (check-true (string-contains combined "CONFIRM"))
  (check-true (string-contains combined "bash"))
  (check-true (string-contains combined "[y] 允许 / [n] 阻止 / [e] 编辑"))
) ;let*

(let* ((dlg (make-confirm-dialog
              :id             "c_nar"
              :tool-name      "write"
              :args           '(("path" . "a/b/c.scm") ("content" . "x\ny"))
              :danger-level   'high
              :target-summary "write: a/b/c.scm"
              :diff-preview   "x\ny"
            ) ;make-confirm-dialog
       ) ;dlg
       (lines (render-confirm-dialog dlg 30 10))
      ) ;
  (check-true (pair? lines))
  (check-true (<= (length lines) 10))
  (for-each
    (lambda (ln) (check-true (<= (display-width ln) 30)))
    lines
  ) ;for-each
) ;let*

(let* ((dlg (make-confirm-dialog
              :id             "c_ov"
              :tool-name      "bash"
              :args           '()
              :danger-level   'high
              :target-summary "bash: test"
              :diff-preview   ""
            ) ;make-confirm-dialog
       ) ;dlg
       (bg (make-list 20 (make-string 80 #\.)))
       (overlaid (overlay-confirm-dialog-lines dlg bg 80 20))
       (combined (string-join overlaid "\n"))
      ) ;
  (check (length overlaid) => 20)
  (check-true (string-contains combined "[y] 允许"))
) ;let*

(display "=== 验收点 6: y / n / e / Esc 按键交互响应闭环 ===\n")
(let ((st (make-pi-tui-state :status 'running))
      (dlg (make-confirm-dialog
             :id           "c_y"
             :tool-name    "bash"
             :args         '()
             :danger-level 'critical
           ) ;make-confirm-dialog
      ) ;dlg
     ) ;
  (state-request-confirm! st dlg)
  (let ((handled? (dispatch-confirm-key! st "y")))
    (check handled? => #t)
    (check (confirm-dialog-decision dlg) => 'allowed)
    (check (state-status st) => 'running)
    (check-true (state-dirty? st))
    (check (state-confirm-dialog st) => #f)
  ) ;let
) ;let

(let ((st (make-pi-tui-state :status 'running))
      (dlg (make-confirm-dialog :id "c_n" :tool-name "write" :args '() :danger-level 'high)
      ) ;dlg
     ) ;
  (state-request-confirm! st dlg)
  (let ((handled? (dispatch-confirm-key! st "n")))
    (check handled? => #t)
    (check (confirm-dialog-decision dlg) => 'rejected)
    (check (state-status st) => 'idle)
    (check (state-confirm-dialog st) => #f)
    (let ((res (confirm-dialog-rejected-result dlg)))
      (check (cdr (assq 'isError res)) => #t)
      (check-true (string-contains (cdr (assq 'content res)) "Operation rejected by user")
      ) ;check-true
    ) ;let
  ) ;let
) ;let

(let ((st (make-pi-tui-state :status 'running))
      (dlg (make-confirm-dialog
             :id           "c_esc"
             :tool-name    "write"
             :args         '()
             :danger-level 'high
           ) ;make-confirm-dialog
      ) ;dlg
     ) ;
  (state-request-confirm! st dlg)
  (let ((handled? (dispatch-confirm-key! st 'escape)))
    (check handled? => #t)
    (check (confirm-dialog-decision dlg) => 'rejected)
    (check (state-status st) => 'idle)
  ) ;let
) ;let

(let* ((ed (make-pi-tui-editor))
       (st (make-pi-tui-state :status 'running :input-editor ed))
       (dlg (make-confirm-dialog
              :id             "c_e"
              :tool-name      "bash"
              :args           '(("command" . "git push --force"))
              :danger-level   'critical
              :target-summary "bash: git push --force"
              :diff-preview   "git push --force"
            ) ;make-confirm-dialog
       ) ;dlg
      ) ;
  (state-request-confirm! st dlg)
  (let ((handled? (dispatch-confirm-key! st "e")))
    (check handled? => #t)
    (check (confirm-dialog-decision dlg) => 'edit)
    (check (state-status st) => 'idle)
    (check (state-confirm-dialog st) => #f)
    (check (pi-tui-editor-text ed) => "git push --force")
  ) ;let
) ;let*

(let ((st (make-pi-tui-state :status 'idle)))
  (check (dispatch-confirm-key! st "y") => #f)
) ;let

(display "=== 验收点 7: 拦截钩子生成器与模块全量测试 ===\n")
(let* ((interceptor (make-confirm-tool-call-interceptor))
       (safe-call '(("id" . "1")
                    ("name" . "read")
                    ("tool" . "read")
                    ("input" ("path" . "foo.scm")))
       ) ;safe-call
       (unsafe-call '(("id" . "2")
                      ("name" . "bash")
                      ("tool" . "bash")
                      ("input" ("command" . "rm -rf *")))
       ) ;unsafe-call
      ) ;
  (check (interceptor safe-call) => #f)
  (let ((res (interceptor unsafe-call)))
    (check-true (confirm-dialog? res))
    (check (confirm-dialog-danger-level res) => 'critical)
  ) ;let
) ;let*

(let ((res (%run-output "confirm-unit"
             "bin/gf test tools/pi/tests/liii/pi/modes/tui/confirm-test.scm"
             120
           ) ;%run-output
      ) ;res
     ) ;
  (check (car res) => 0)
  (check-true (string-contains (cdr res) "PASS"))
  (check-true (string-contains (cdr res) "100 correct, 0 failed"))
) ;let

(let ((res (%run-output "all-tests" "bin/gf test tools/pi/tests/liii/" 600)))
  (check (car res) => 0)
  (check-true (string-contains (cdr res) "Passed: 331"))
  (check-true (string-contains (cdr res) "Total:  331"))
) ;let

(check-report)
