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
  (liii tui component)
  (liii tui box)
  (liii tui terminal)
  (liii pi modes tui)
  (liii pi modes tui state)
  (liii pi modes tui loop)
  (liii pi modes tui editor)
) ;import

(check-set-mode! 'report-failed)

;; [083] 验收脚本：用户输入组件集成（(liii tui editor) 与多行编辑）

(define (%run-ok? tag cmd timeout)
  (let ((tmp (string-append "tmp-083-" tag ".txt")))
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

(display "=== 验收点 2: 任务 083 代码与测试文件行数约束 (<= 300 行) ===\n"
) ;display
(for-each
  (lambda (f)
    (let ((lines (length (string-split (path-read-text (path f)) "\n"))))
      (check-true (<= lines 300))
    ) ;let
  ) ;lambda
  '("tools/pi/liii/pi/modes/tui/editor.scm"
    "tools/pi/liii/pi/modes/tui/editor/handle.scm"
    "tools/pi/liii/pi/modes/tui/editor/render.scm"
    "tools/pi/liii/pi/modes/tui/editor/types.scm"
    "tools/pi/liii/pi/modes/tui/loop.scm" "tools/pi/liii/pi/modes/tui/state.scm"
    "tools/pi/liii/pi/modes/tui.scm"
    "tools/pi/tests/liii/pi/modes/tui/editor-test.scm" "tools/pi/devel/083.md")
) ;for-each

(display "=== 验收点 3: 集成 (liii tui editor) 输入组件 ===\n")
(let ((ed (make-pi-tui-editor)))
  (check-true (pi-tui-editor? ed))
  (check-true (component? ed))
  (check (pi-tui-editor-text ed) => "")
  (check (pi-tui-editor-lines ed) => '(""))
  (check (pi-tui-editor-cursor ed) => '(0 0))
  (check (pi-tui-editor-prompt ed) => "> ")
  (check (pi-tui-editor-placeholder ed)
    =>
    "Ask a question or type / for commands..."
  ) ;check
  (check-true (pi-tui-editor-focused? ed))

  (pi-tui-editor-set-prompt! ed "$ ")
  (check (pi-tui-editor-prompt ed) => "$ ")
  (pi-tui-editor-set-placeholder! ed "Ask something...")
  (check (pi-tui-editor-placeholder ed) => "Ask something...")
  (pi-tui-editor-set-focused! ed #f)
  (check-false (pi-tui-editor-focused? ed))
  (pi-tui-editor-set-focused! ed #t)
  (check-true (pi-tui-editor-focused? ed))
  (pi-tui-editor-set-text! ed "hello world")
  (check (pi-tui-editor-text ed) => "hello world")
  (pi-tui-editor-clear! ed)
  (check (pi-tui-editor-text ed) => "")
) ;let

(display "=== 验收点 4: 完整按键映射与编辑能力 ===\n")
(let ((ed (make-pi-tui-editor)))
  (check-true (pi-tui-editor-handle-key ed "test"))
  (check (pi-tui-editor-text ed) => "test")
  (check-true (pi-tui-editor-handle-key ed " 方案"))
  (check (pi-tui-editor-text ed) => "test 方案")
  (check-true (pi-tui-editor-handle-key ed 'backspace))
  (check (pi-tui-editor-text ed) => "test 方")
  (check-true (pi-tui-editor-handle-key ed 'left))
  (check-true (pi-tui-editor-handle-key ed 'left))
  (check-true (pi-tui-editor-handle-key ed "!"))
  (check (pi-tui-editor-text ed) => "test! 方")
  (check-true (pi-tui-editor-handle-key ed 'home))
  (check (pi-tui-editor-cursor ed) => '(0 0))
  (check-true (pi-tui-editor-handle-key ed 'end))
  (check (pi-tui-editor-cursor ed) => (list 0 (string-length "test! 方")))
  (check-true (pi-tui-editor-handle-key ed 'home))
  (check-true (pi-tui-editor-handle-key ed 'delete))
  (check (pi-tui-editor-text ed) => "est! 方")
) ;let

(display "=== 验收点 5: 会话历史记录导航 ===\n")
(let ((ed (make-pi-tui-editor)))
  (pi-tui-editor-push-history! ed "history 1")
  (pi-tui-editor-push-history! ed "history 2")
  (check-true (pi-tui-editor-handle-key ed 'up))
  (check (pi-tui-editor-text ed) => "history 2")
  (check-true (pi-tui-editor-handle-key ed 'up))
  (check (pi-tui-editor-text ed) => "history 1")
  (check-true (pi-tui-editor-handle-key ed 'up))
  (check (pi-tui-editor-text ed) => "history 1")
  (check-true (pi-tui-editor-handle-key ed 'down))
  (check (pi-tui-editor-text ed) => "history 2")
  (check-true (pi-tui-editor-handle-key ed 'down))
  (check (pi-tui-editor-text ed) => "")
) ;let

(display "=== 验收点 6: 多行折行与回车提交逻辑 ===\n")
(let* ((submitted #f)
       (ed (make-pi-tui-editor :on-submit (lambda (self txt) (set! submitted txt))))
      ) ;
  (pi-tui-editor-handle-key ed "row 1")
  (check-true (pi-tui-editor-handle-key ed 'shift-enter))
  (check (length (pi-tui-editor-lines ed)) => 2)
  (pi-tui-editor-handle-key ed "row 2")
  (check (pi-tui-editor-text ed) => "row 1\nrow 2")
  (check-true (pi-tui-editor-handle-key ed 'up))
  (check (car (pi-tui-editor-cursor ed)) => 0)
  (check-true (pi-tui-editor-handle-key ed 'down))
  (check (car (pi-tui-editor-cursor ed)) => 1)

  ;; 反斜杠折行
  (pi-tui-editor-clear! ed)
  (pi-tui-editor-handle-key ed "first \\")
  (check-true (pi-tui-editor-handle-key ed 'enter))
  (check (pi-tui-editor-lines ed) => '("first " ""))
  (pi-tui-editor-handle-key ed "second")
  (check (pi-tui-editor-text ed) => "first \nsecond")

  ;; Enter 提交
  (check-false submitted)
  (check-true (pi-tui-editor-handle-key ed 'enter))
  (check submitted => "first \nsecond")
) ;let*

(display "=== 验收点 7: 现代化外框包裹渲染与 Component 协议 ===\n"
) ;display
(let ((ed (make-pi-tui-editor :prompt "> ")))
  (pi-tui-editor-set-text! ed "prompt cmd")
  (let ((lines (pi-tui-editor-render ed 40)))
    (check-true (pair? lines))
    (check-true (> (string-length (car lines)) 0))
  ) ;let
  (let ((boxed (pi-tui-editor-render-boxed ed 30 :style 'rounded :title "Input")))
    (check-true (pair? boxed))
    (check-true (>= (length boxed) 3))
  ) ;let
  (let* ((bx (make-box ed :style 'rounded :title "Boxed Component"))
         (rendered (component-render bx 30))
        ) ;
    (check-true (pair? rendered))
    (check-true (>= (length rendered) 3))
  ) ;let*
) ;let

(display "=== 验收点 8: 状态机集成与协同 ===\n")
(let* ((ed (make-pi-tui-editor)) (st (make-pi-tui-state :input-editor ed)))
  (pi-tui-editor-set-text! ed "status query")
  (check (state-input-text st) => "status query")
  (state-submit-prompt! st)
  (check (state-status st) => 'running)
  (check (state-messages st) => '((user . "status query")))
  (check (state-input-text st) => "")
  (check (pi-tui-editor-text ed) => "")
  (check-true (pi-tui-editor-handle-key ed 'up))
  (check (pi-tui-editor-text ed) => "status query")
) ;let*

(display "=== 验收点 9: VirtualTerminal 按键流集成驱动 ===\n")
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (ed (make-pi-tui-editor))
       (st (make-pi-tui-state :input-editor ed :status 'idle))
      ) ;
  (virtual-terminal-push-key! vt "q")
  (virtual-terminal-push-key! vt "u")
  (virtual-terminal-push-key! vt "e")
  (virtual-terminal-push-key! vt "r")
  (virtual-terminal-push-key! vt "y")
  (virtual-terminal-push-key! vt 'backspace)
  (virtual-terminal-push-key! vt 'shift-enter)
  (virtual-terminal-push-key! vt "line2")
  (virtual-terminal-push-key! vt 'escape)

  (let ((final-st (run-tui-loop vt :state st :return-state #t)))
    (check (pi-tui-state? final-st) => #t)
    (check (state-status final-st) => 'exit)
    (check (pi-tui-editor-text ed) => "quer\nline2")
  ) ;let
) ;let*

(display "=== 验收点 10: tools/pi 全量单测运行（断言 Total: 323, Passed: 323） ===\n"
) ;display
(let* ((test-cmd (string-append "bin/gf test tools/pi/tests/liii/ > tmp-083-summary.txt 2>&1")
       ) ;test-cmd
       (status (run (list (pi-find-bash-path) "-c" test-cmd) :stdin 'null :timeout 300)
       ) ;status
       (out (if (file-exists? "tmp-083-summary.txt")
              (path-read-text (path "tmp-083-summary.txt"))
              ""
            ) ;if
       ) ;out
      ) ;
  (when (file-exists? "tmp-083-summary.txt")
    (delete-file "tmp-083-summary.txt")
  ) ;when
  (check status => 0)
  (check-true (string-contains out "Total:  323"))
  (check-true (string-contains out "Passed: 323"))
) ;let*

(check-report)
