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
  (liii pi bash)
  (liii tui terminal)
  (liii pi modes tui)
  (liii pi modes tui loop)
  (liii pi modes tui editor)
  (liii pi modes tui state)
) ;import

(check-set-mode! 'report-failed)

;; [092] 验收脚本：修复 TUI 按下回车键不工作

(define (%run-ok? tag cmd timeout)
  (let* ((tmp (string-append "tmp-092-" tag ".txt"))
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
  (let* ((tmp (string-append "tmp-092-" tag ".txt"))
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

(display "=== 验收点 1: 门禁检查（构建 / fmt / diff / check / doc） ===\n"
) ;display
(check (%run-ok? "build" "xmake b goldfish" 300) => 0)
(check (%run-ok? "fmt-chk" "bin/gf fmt --dry-run" 120) => 0)
(check (%run-ok? "check" "bin/gf check tools/pi/liii/ tools/pi/tests/" 120)
  =>
  0
) ;check
(check (%run-ok? "doc" "bin/gf doc --build-json" 120) => 0)

(display "=== 验收点 2: 任务 092 代码与测试文件行数约束 (<= 300 行) ===\n"
) ;display
(for-each
  (lambda (f)
    (let ((lines (length (string-split (path-read-text (path f)) "\n"))))
      (check-true (<= lines 300))
    ) ;let
  ) ;lambda
  '("tools/pi/liii/pi/modes/tui/loop/dispatch.scm"
    "tools/pi/tests/liii/pi/modes/tui/enter-send-test.scm"
    "tools/pi/devel/092.md" "tools/pi/devel/092.scm")
) ;for-each

(display "=== 验收点 3: 输入文本后按 Enter 正常发送 (IDLE -> RUNNING, 消息入视口) ===\n"
) ;display
;; 3.1 验证在 TUI 主循环中按下 Enter 触发提交
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (ed (make-pi-tui-editor :focused? #t))
       (st (make-pi-tui-state :input-editor ed :status 'idle))
      ) ;
  (virtual-terminal-push-key! vt "h")
  (virtual-terminal-push-key! vt "e")
  (virtual-terminal-push-key! vt "l")
  (virtual-terminal-push-key! vt "l")
  (virtual-terminal-push-key! vt "o")
  (virtual-terminal-push-key! vt 'enter)

  (let ((captured-status #f))
    (run-tui-loop vt
      :state     st
      :max-ticks 10
      :on-tick
      (lambda (s ticks)
        (when (pair? (state-messages s))
          (set! captured-status (state-status s))
          'exit
        ) ;when
      ) ;lambda
    ) ;run-tui-loop
    (check captured-status => 'running)
    ;; 验证用户消息已成功推入视口
    (check (state-messages st) => '((user . "hello")))
    ;; 验证输入框已被清空
    (check (pi-tui-editor-text ed) => "")
    ;; 验证处于脏状态 (需要渲染)
    (check (state-dirty? st) => #t)
  ) ;let
) ;let*

;; 3.2 验证回车字符 "\r" 与 "\n" 同样具备发送语义
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (ed (make-pi-tui-editor :focused? #t))
       (st (make-pi-tui-state :input-editor ed :status 'idle))
      ) ;
  (virtual-terminal-push-key! vt "ping")
  (virtual-terminal-push-key! vt "\r")

  (let ((captured-status #f))
    (run-tui-loop vt
      :state     st
      :max-ticks 10
      :on-tick
      (lambda (s ticks)
        (when (pair? (state-messages s))
          (set! captured-status (state-status s))
          'exit
        ) ;when
      ) ;lambda
    ) ;run-tui-loop
    (check captured-status => 'running)
    (check (state-messages st) => '((user . "ping")))
    (check (pi-tui-editor-text ed) => "")
  ) ;let
) ;let*

(display "=== 验收点 4: 空文本按 Enter 静默不发送 (防误触) ===\n"
) ;display
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (ed (make-pi-tui-editor :focused? #t))
       (st (make-pi-tui-state :input-editor ed :status 'idle))
      ) ;
  (virtual-terminal-push-key! vt 'enter)
  (virtual-terminal-push-key! vt "\n")

  (let ((final-st (run-tui-loop vt :state st :max-ticks 5 :return-state #t)))
    ;; 消息列表仍为空
    (check (state-messages final-st) => '())
    ;; 状态仍为 'idle
    (check (state-status final-st) => 'idle)
  ) ;let
) ;let*

(display "=== 验收点 5: Shift+Enter 折行不回归 ===\n")
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (ed (make-pi-tui-editor :focused? #t))
       (st (make-pi-tui-state :input-editor ed :status 'idle))
      ) ;
  (virtual-terminal-push-key! vt "first line")
  (virtual-terminal-push-key! vt 'shift-enter)
  (virtual-terminal-push-key! vt "second line")

  (let ((final-st (run-tui-loop vt :state st :max-ticks 5 :return-state #t)))
    ;; 未按普通 enter，消息列表仍为空
    (check (state-messages final-st) => '())
    ;; 状态仍为 idle
    (check (state-status final-st) => 'idle)
    ;; 输入框完整包含换行内容
    (check (pi-tui-editor-text ed) => "first line\nsecond line")
  ) ;let
) ;let*

(display "=== 验收点 6: 多行文本组合 Shift+Enter 与 Enter 完整链路 ===\n"
) ;display
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (ed (make-pi-tui-editor :focused? #t))
       (st (make-pi-tui-state :input-editor ed :status 'idle))
      ) ;
  (virtual-terminal-push-key! vt "foo")
  (virtual-terminal-push-key! vt 'shift-enter)
  (virtual-terminal-push-key! vt "bar")
  (virtual-terminal-push-key! vt 'enter)

  (let ((captured-status #f))
    (run-tui-loop vt
      :state     st
      :max-ticks 10
      :on-tick
      (lambda (s ticks)
        (when (pair? (state-messages s))
          (set! captured-status (state-status s))
          'exit
        ) ;when
      ) ;lambda
    ) ;run-tui-loop
    (check captured-status => 'running)
    ;; 提交的多行消息完整入视口
    (check (state-messages st) => '((user . "foo\nbar")))
    (check (pi-tui-editor-text ed) => "")
  ) ;let
) ;let*

(display "=== 验收点 7: 回归测试套件全绿运行 ===\n")
(let ((res (%run-with-output "reg-test"
             "bin/gf test tools/pi/tests/liii/pi/modes/tui/enter-send-test.scm"
             120
           ) ;%run-with-output
      ) ;res
     ) ;
  (check (car res) => 0)
  (check-true (string-contains (cdr res) "Passed: 1"))
) ;let

(display "=== 验收点 8: 全量单元测试集稳定全绿 ===\n")
(let ((res (%run-with-output "all-test" "bin/gf test tools/pi/tests/" 300)))
  (check (car res) => 0)
  (check-true (string-contains (cdr res) "Passed: 333"))
) ;let

(check-report)
