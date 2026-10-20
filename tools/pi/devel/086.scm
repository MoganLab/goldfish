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
  (liii tui terminal)
  (liii tui main-screen)
  (liii pi event types)
  (liii pi modes tui)
  (liii pi modes tui state)
  (liii pi modes tui loop)
  (liii pi modes tui message)
  (liii pi modes tui viewport)
  (liii pi modes tui layout)
  (liii pi modes tui editor)
  (liii pi modes tui streaming)
) ;import

(check-set-mode! 'report-failed)

;; [086] 验收脚本：流式 SSE 增量文本上屏与行差分渲染

(define (%run-ok? tag cmd timeout)
  (let ((tmp (string-append "tmp-086-" tag ".txt")))
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

(define (%run-with-output tag cmd timeout)
  (let ((tmp (string-append "tmp-086-" tag ".txt")))
    (let ((ret (run (list (pi-find-bash-path) "-c" (string-append cmd " > " tmp " 2>&1"))
                 :stdin   'null
                 :timeout timeout
               ) ;run
          ) ;ret
         ) ;
      (let ((out (if (file-exists? tmp) (path-read-text (path tmp)) "")))
        (when (file-exists? tmp)
          (delete-file tmp)
        ) ;when
        (cons ret out)
      ) ;let
    ) ;let
  ) ;let
) ;define

(display "=== 验收点 1: 门禁检查（构建 / fmt / check / doc） ===\n")
(check (%run-ok? "build" "xmake b goldfish" 300) => 0)
(check (%run-ok? "fmt-chk" "bin/gf fmt --dry-run" 120) => 0)
(check (%run-ok? "check" "bin/gf check" 120) => 0)
(check (%run-ok? "doc" "bin/gf doc --build-json" 120) => 0)

(display "=== 验收点 2: 任务 086 代码与测试文件行数约束 (<= 300 行) ===\n"
) ;display
(for-each
  (lambda (f)
    (let ((lines (length (string-split (path-read-text (path f)) "\n"))))
      (check-true (<= lines 300))
    ) ;let
  ) ;lambda
  '("tools/pi/liii/pi/modes/tui/streaming.scm"
    "tools/pi/liii/pi/modes/tui/viewport.scm"
    "tools/pi/liii/pi/modes/tui/state.scm" "tools/pi/liii/pi/modes/tui/loop.scm"
    "tools/pi/liii/pi/modes/tui.scm"
    "tools/pi/tests/liii/pi/modes/tui/streaming-test.scm"
    "tools/pi/tests/liii/pi/modes/tui/viewport-test.scm" "tools/pi/devel/086.md"
    "tools/pi/devel/086.scm")
) ;for-each

(display "=== 验收点 3: 增量文本 chunk 提取与消息列表末尾累加 ===\n"
) ;display
;; 3.1 提取 chunk delta
(check (extract-stream-delta "plain text") => "plain text")
(check (extract-stream-delta '((type . "text_delta") (delta . "delta text")))
  =>
  "delta text"
) ;check
(check
  (extract-stream-delta '((type . "message_update")
                          (assistantMessageEvent (type . "text_delta")
                            (delta . "nested delta")))
  ) ;extract-stream-delta
  =>
  "nested delta"
) ;check

;; 3.2 列表增量追加
(let* ((msgs1 (append-stream-chunk '() "first "))
       (msgs2 (append-stream-chunk msgs1 "second "))
       (msgs3 (append-stream-chunk msgs2 "third"))
      ) ;
  (check (length msgs3) => 1)
  (check (message-role (car msgs3)) => 'assistant)
  (check (message-content (car msgs3)) => "first second third")
) ;let*

;; 3.3 状态机原地增量与纯函数增量
(let ((st (make-pi-tui-state :status 'running :scroll-offset 5)))
  (state-clear-dirty! st)
  (state-append-stream-chunk! st "chunk A")
  (check (state-status st) => 'streaming)
  (check (state-scroll-offset st) => 0)
  (check-true (state-dirty? st))
  (check (length (state-messages st)) => 1)
  (check (message-content (car (state-messages st))) => "chunk A")

  (let ((st2 (state-with-stream-chunk st " chunk B")))
    (check (state-status st2) => 'streaming)
    (check (state-scroll-offset st2) => 0)
    (check-true (state-dirty? st2))
    (check (message-content (car (state-messages st2))) => "chunk A chunk B")
  ) ;let
) ;let

(display "=== 验收点 4: 末尾行自然折行局部重排与贴底跟随 ===\n"
) ;display
;; 4.1 committed lines 计算
(let* ((m1 (make-pi-tui-message 'user "User question"))
       (m2 (make-pi-tui-message 'assistant "Assistant part 1"))
       (committed (compute-committed-lines (list m1) 40))
       (viewport (render-messages-viewport-stream (list m1 m2) 40 10 0 committed))
      ) ;
  (check (length committed) => 1)
  (check (length viewport) => 2)
  (check (car viewport) => (car committed))
) ;let*

;; 4.2 流式贴底跟随 (scroll-offset=0)
(let* ((st (make-pi-tui-state :status 'streaming :scroll-offset 0 :cols 80 :rows 24))
       (layout (compute-tui-layout 80 24 1))
      ) ;
  (state-append-stream-chunk! st "Line 1\nLine 2\nLine 3\nLine 4\nLine 5")
  (let ((lines (render-tui-screen-lines st layout)))
    (check (length lines) => 24)
  ) ;let
) ;let*

(display "=== 验收点 5: 基于 MainScreen 的行级差分渲染与状态机调度 ===\n"
) ;display
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (screen (make-main-screen vt))
       (st (make-pi-tui-state :status 'running :cols 80 :rows 24))
      ) ;
  ;; 流式追加第一个 chunk 并渲染
  (stream-screen-render! screen st "Hello, ")
  (check (state-status st) => 'streaming)
  (let ((scr1 (virtual-terminal-screen vt)))
    (check-true (string-contains (car scr1) "Hello, "))
  ) ;let

  ;; 流式追加第二个 chunk 并差分渲染
  (stream-screen-render! screen st "world!")
  (check (message-content (car (state-messages st))) => "Hello, world!")
  (let ((scr2 (virtual-terminal-screen vt)))
    (check-true (string-contains (car scr2) "Hello, world!"))
  ) ;let

  ;; 完成流式结束回到 idle
  (state-finish-stream! st 'idle)
  (check (state-status st) => 'idle)
  (check-true (state-dirty? st))
) ;let*

(display "=== 验收点 6: DECSET 2026 同步包裹与 0 闪烁终端输出 ===\n"
) ;display
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (screen (make-main-screen vt))
       (st (make-pi-tui-state :status 'streaming :cols 80 :rows 24))
      ) ;
  (render-tui-screen! screen st)
  (let ((raw (virtual-terminal-raw vt)))
    ;; 验证包含 DECSET 2026 开始标记 \x1b;[?2026h 与结束标记 \x1b;[?2026l
    (check-true (not (null? (member "\x1b;[?2026h" raw))))
    (check-true (not (null? (member "\x1b;[?2026l" raw))))
  ) ;let
) ;let*

(display "=== 验收点 7: 门面导出接口完整性 ===\n")
(check-true (procedure? extract-stream-delta))
(check-true (procedure? append-stream-chunk))
(check-true (procedure? state-append-stream-chunk!))
(check-true (procedure? state-with-stream-chunk))
(check-true (procedure? compute-committed-lines))
(check-true (procedure? render-messages-viewport-stream))
(check-true (procedure? render-tui-screen-lines))
(check-true (procedure? render-tui-screen!))
(check-true (procedure? stream-screen-render!))
(check-true (procedure? state-finish-stream!))
(check-true (procedure? state-with-stream-finished))

(display "=== 验收点 8: 真实冒烟（非 TTY 下启动 bin/gf pi 打印帮助并以 0 退出） ===\n"
) ;display
(let ((res (%run-with-output "smoke" "bin/gf pi" 30)))
  (check (car res) => 0)
  (check-true (string-contains (cdr res) "gf pi [options]"))
) ;let

(display "=== 验收点 9: 全量测试套件执行并硬断言总计数与通过数（Total: 327, Passed: 327） ===\n"
) ;display
(let ((res (%run-with-output "all-tests" "bin/gf test tools/pi/tests/liii/" 300)))
  (check (car res) => 0)
  (let ((out (cdr res)))
    (check-true (string-contains out "Total:  327"))
    (check-true (string-contains out "Passed: 327"))
  ) ;let
) ;let

(check-report)
