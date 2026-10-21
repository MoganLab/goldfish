(import (scheme base)
  (liii check)
  (liii string)
  (liii tui text-width)
  (liii pi modes tui)
  (liii pi modes tui state)
  (liii pi modes tui editor)
  (liii pi modes tui confirm)
) ;import

;; ============================================================================
;; 1.0 高危工具调用拦截识别机制
;; ============================================================================

;; 1.1 write 工具判定为高危 ('high)
(check (tool-dangerous? "write" '(("path" . "foo.scm") ("content" . "hello"))) => #t)
(check (classify-tool-danger "write" '(("path" . "foo.scm") ("content" . "hello"))) => 'high)

;; 1.2 edit 工具判定为中危 ('medium)
(check (tool-dangerous? "edit" '(("path" . "foo.scm") ("edits" . ()))) => #t)
(check (classify-tool-danger "edit" '(("path" . "foo.scm") ("edits" . ()))) => 'medium)

;; 1.3 bash 破坏性命令判定
(check (tool-dangerous? "bash" '(("command" . "rm -rf /tmp/foo"))) => #t)
(check (classify-tool-danger "bash" '(("command" . "rm -rf /tmp/foo"))) => 'critical)

(check (tool-dangerous? "bash" '(("command" . "git reset --hard HEAD~1"))) => #t)
(check (classify-tool-danger "bash" '(("command" . "git reset --hard HEAD~1"))) => 'critical)

(check (tool-dangerous? "bash" '(("command" . "git push origin main --force"))) => #t)
(check (classify-tool-danger "bash" '(("command" . "git push origin main --force"))) => 'critical)

(check (tool-dangerous? "bash" '(("command" . "git clean -fd"))) => #t)
(check (classify-tool-danger "bash" '(("command" . "git clean -fd"))) => 'high)

(check (tool-dangerous? "bash" '(("command" . "kill -9 1234"))) => #t)
(check (classify-tool-danger "bash" '(("command" . "kill -9 1234"))) => 'high)

(check (tool-dangerous? "bash" '(("command" . "killall goldfish"))) => #t)
(check (classify-tool-danger "bash" '(("command" . "killall goldfish"))) => 'high)

(check (tool-dangerous? "bash" '(("command" . "dd if=/dev/zero of=/dev/sda"))) => #t)
(check (classify-tool-danger "bash" '(("command" . "dd if=/dev/zero of=/dev/sda"))) => 'critical)

(check (tool-dangerous? "bash" '(("command" . "mkfs.ext4 /dev/sdb1"))) => #t)
(check (classify-tool-danger "bash" '(("command" . "mkfs.ext4 /dev/sdb1"))) => 'critical)

(check (tool-dangerous? "bash" '(("command" . "chmod -R 777 /"))) => #t)
(check (classify-tool-danger "bash" '(("command" . "chmod -R 777 /"))) => 'high)

;; 1.4 普通只读或良性命令判定为非高危 (#f)
(check (tool-dangerous? "read" '(("path" . "foo.scm"))) => #f)
(check (classify-tool-danger "read" '(("path" . "foo.scm"))) => #f)
(check (tool-dangerous? "ls" '()) => #f)
(check (tool-dangerous? "grep" '(("pattern" . "test"))) => #f)
(check (tool-dangerous? "find" '(("pattern" . "*.scm"))) => #f)
(check (tool-dangerous? "web_search" '(("query" . "scheme"))) => #f)
(check (tool-dangerous? "bash" '(("command" . "git status"))) => #f)
(check (classify-tool-danger "bash" '(("command" . "git status"))) => #f)
(check (tool-dangerous? "bash" '(("command" . "ls -la"))) => #f)
(check (tool-dangerous? "bash" '(("command" . "cat file.txt"))) => #f)

;; 1.5 非法参数类型抛出 type-error
(check-catch 'type-error (classify-tool-danger 123 '()))
(check-catch 'type-error (classify-tool-danger "bash" "not-a-list"))
(check-catch 'type-error (tool-dangerous? 123 '()))
(check-catch 'type-error (tool-dangerous? "bash" "not-a-list"))

;; ============================================================================
;; 2.0 模态确认对话框数据模型与状态机
;; ============================================================================

;; 2.1 目标摘要与差异预览提取
(check (extract-target-summary "write" '(("path" . "foo.scm") ("content" . "bar"))) => "write: foo.scm")
(check (extract-target-summary "edit" '(("path" . "src/bar.scm"))) => "edit: src/bar.scm")
(check (extract-target-summary "bash" '(("command" . "rm -rf test"))) => "bash: rm -rf test")

(check (string? (extract-diff-preview "write" '(("path" . "a.txt") ("content" . "line1\nline2")))) => #t)
(check (string? (extract-diff-preview "edit" '(("path" . "a.txt") ("edits" . ((("oldText" . "1") ("newText" . "2"))))))) => #t)
(check (extract-diff-preview "bash" '(("command" . "rm -rf /tmp/data"))) => "rm -rf /tmp/data")

;; 2.2 ConfirmDialog 构造与访问器
(let ((dlg (make-confirm-dialog
             :id "call_123"
             :tool-name "write"
             :args '(("path" . "hello.txt") ("content" . "world"))
             :danger-level 'high
             :target-summary "write: hello.txt"
             :diff-preview "world"
           )))
  (check (confirm-dialog? dlg) => #t)
  (check (confirm-dialog-id dlg) => "call_123")
  (check (confirm-dialog-tool-name dlg) => "write")
  (check (confirm-dialog-danger-level dlg) => 'high)
  (check (confirm-dialog-target-summary dlg) => "write: hello.txt")
  (check (confirm-dialog-diff-preview dlg) => "world")
  (check (confirm-dialog-decision dlg) => 'pending)
  (confirm-dialog-set-decision! dlg 'allowed)
  (check (confirm-dialog-decision dlg) => 'allowed)
)

;; 2.3 非法构造抛出 type-error
(check-catch 'type-error (make-confirm-dialog :id 123 :tool-name "write"))
(check-catch 'type-error (make-confirm-dialog :id "1" :tool-name 123))
(check-catch 'type-error (make-confirm-dialog :id "1" :tool-name "write" :danger-level 'unknown))

;; 2.4 状态机切入 'confirm 与挂起
(let* ((st (make-pi-tui-state :status 'running))
       (dlg (make-confirm-dialog
              :id "tc_1"
              :tool-name "bash"
              :args '(("command" . "rm -rf foo"))
              :danger-level 'critical
              :target-summary "bash: rm -rf foo"
              :diff-preview "rm -rf foo"
            )))
  (check (state-confirm-dialog st) => #f)
  (state-request-confirm! st dlg)
  (check (state-status st) => 'confirm)
  (check (state-dirty? st) => #t)
  (check (eq? (state-confirm-dialog st) dlg) => #t)
)

;; ============================================================================
;; 3.0 居中模态对话框排版与差异预览渲染
;; ============================================================================

(let* ((dlg (make-confirm-dialog
              :id "tc_2"
              :tool-name "bash"
              :args '(("command" . "rm -rf /important"))
              :danger-level 'critical
              :target-summary "bash: rm -rf /important"
              :diff-preview "rm -rf /important"
            ))
       (lines (render-confirm-dialog dlg 60 15)))
  ;; 渲染输出非空列表且宽度符合
  (check (pair? lines) => #t)
  (check (<= (length lines) 15) => #t)
  ;; 验证包含关健文本与操作提示
  (let ((combined (apply string-append lines)))
    (check (string-contains combined "CONFIRM") => #t)
    (check (string-contains combined "bash") => #t)
    (check (string-contains combined "[y] 允许 / [n] 阻止 / [e] 编辑") => #t)
  )
)

;; 3.2 窄终端自适应，不报错
(let* ((dlg (make-confirm-dialog
              :id "tc_3"
              :tool-name "write"
              :args '(("path" . "very/long/nested/path/to/target/source/file.scm"))
              :danger-level 'high
              :target-summary "write: very/long/nested/path/to/target/source/file.scm"
              :diff-preview "line1\nline2\nline3\nline4\nline5\nline6"
            ))
       (narrow-lines (render-confirm-dialog dlg 30 10)))
  (check (pair? narrow-lines) => #t)
  (check (<= (length narrow-lines) 10) => #t)
  ;; 所有渲染行显示宽度不超过终端宽度 30
  (for-each
    (lambda (ln)
      (check (<= (display-width ln) 30) => #t)
    )
    narrow-lines
  )
)

;; 3.3 overlay-confirm-dialog-lines 覆盖后总行数恒等于屏幕高度
(let* ((dlg (make-confirm-dialog
              :id "tc_4"
              :tool-name "edit"
              :args '(("path" . "foo.scm"))
              :danger-level 'medium
              :target-summary "edit: foo.scm"
              :diff-preview "- old\n+ new"
            ))
       (bg-lines (make-list 20 (make-string 80 #\.)))
       (overlaid (overlay-confirm-dialog-lines dlg bg-lines 80 20)))
  (check (length overlaid) => 20)
  ;; 居中区域包含对话框内容
  (let ((combined (apply string-append overlaid)))
    (check (string-contains combined "[y] 允许") => #t)
  )
)

;; ============================================================================
;; 4.0 用户按键响应与工具执行放行/拒绝/编辑闭环
;; ============================================================================

;; 4.1 按 'y' 或 'Y'：允许执行，放行并恢复到 'running
(let* ((st (make-pi-tui-state :status 'running))
       (dlg (make-confirm-dialog
              :id "tc_y"
              :tool-name "write"
              :args '(("path" . "foo.txt") ("content" . "bar"))
              :danger-level 'high
              :target-summary "write: foo.txt"
              :diff-preview "bar"
            )))
  (state-request-confirm! st dlg)
  (check (state-status st) => 'confirm)
  ;; 模拟用户按 'y'
  (let ((handled? (dispatch-confirm-key! st "y")))
    (check handled? => #t)
    (check (confirm-dialog-decision dlg) => 'allowed)
    (check (state-status st) => 'running)
    (check (state-dirty? st) => #t)
    (check (state-confirm-dialog st) => #f)
  )
)

;; 4.2 按 'n' 或 'N' 或 'escape'：阻止执行，记录拒绝并恢复到 'idle
(let* ((st (make-pi-tui-state :status 'running))
       (dlg (make-confirm-dialog
              :id "tc_n"
              :tool-name "bash"
              :args '(("command" . "rm -rf /"))
              :danger-level 'critical
              :target-summary "bash: rm -rf /"
              :diff-preview "rm -rf /"
            )))
  (state-request-confirm! st dlg)
  (let ((handled? (dispatch-confirm-key! st 'escape)))
    (check handled? => #t)
    (check (confirm-dialog-decision dlg) => 'rejected)
    (check (state-status st) => 'idle)
    (check (state-confirm-dialog st) => #f)
    (let ((res (confirm-dialog-rejected-result dlg)))
      (check (cdr (assq 'isError res)) => #t)
      (check (string-contains (cdr (assq 'content res)) "Operation rejected by user") => #t)
    )
  )
)

;; 4.3 按 'e' 或 'E'：进入编辑模式，将目标命令或内容填入输入框
(let* ((ed (make-pi-tui-editor))
       (st (make-pi-tui-state :status 'running :input-editor ed))
       (dlg (make-confirm-dialog
              :id "tc_e"
              :tool-name "bash"
              :args '(("command" . "git push --force"))
              :danger-level 'critical
              :target-summary "bash: git push --force"
              :diff-preview "git push --force"
            )))
  (state-request-confirm! st dlg)
  (let ((handled? (dispatch-confirm-key! st "e")))
    (check handled? => #t)
    (check (confirm-dialog-decision dlg) => 'edit)
    (check (state-status st) => 'idle)
    (check (state-confirm-dialog st) => #f)
    ;; 目标命令填入 editor 供微调
    (check (pi-tui-editor-text ed) => "git push --force")
  )
)

;; 4.4 其他任意按键在 'confirm 状态下被吞掉
(let* ((st (make-pi-tui-state :status 'running))
       (dlg (make-confirm-dialog
              :id "tc_misc"
              :tool-name "write"
              :args '()
              :danger-level 'high
              :target-summary "write"
              :diff-preview ""
            )))
  (state-request-confirm! st dlg)
  (check (dispatch-confirm-key! st "a") => #t)
  (check (state-status st) => 'confirm)
  (check (confirm-dialog-decision dlg) => 'pending)
)

;; 4.5 非 'confirm 状态下 dispatch-confirm-key! 不处理按键
(let ((st (make-pi-tui-state :status 'idle)))
  (check (dispatch-confirm-key! st "y") => #f)
)

;; 4.6 Extension 拦截器钩子集成：make-confirm-tool-call-interceptor
(let* ((interceptor (make-confirm-tool-call-interceptor))
       (safe-call '(("id" . "1") ("name" . "read") ("tool" . "read") ("input" . (("path" . "foo.scm")))))
       (unsafe-call '(("id" . "2") ("name" . "bash") ("tool" . "bash") ("input" . (("command" . "rm -rf *"))))))
  ;; 安全调用放行
  (check (interceptor safe-call) => #f)
  ;; 高危调用拦截，返回生成的 ConfirmDialog
  (let ((res (interceptor unsafe-call)))
    (check (confirm-dialog? res) => #t)
    (check (confirm-dialog-danger-level res) => 'critical)
  )
)

(check-report)
