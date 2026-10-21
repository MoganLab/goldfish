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
  (liii tui main-screen)
  (liii pi modes tui)
  (liii pi modes tui loop)
  (liii pi modes tui editor)
  (liii pi modes tui state)
  (liii pi modes tui layout)
  (liii pi modes tui runner)
  (liii pi modes tui streaming)
) ;import

(check-set-mode! 'report-failed)

;; [091] 验收脚本：修复 bin/gf pi 首次启动黑屏

(define (%run-ok? tag cmd timeout)
  (let* ((tmp (string-append "tmp-091-" tag ".txt"))
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
  (let* ((tmp (string-append "tmp-091-" tag ".txt"))
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
(check (%run-ok? "check" "bin/gf check tools/pi/liii/ tools/pi/tests/" 120)
  =>
  0
) ;check
(check (%run-ok? "doc" "bin/gf doc --build-json" 120) => 0)

(display "=== 验收点 2: 任务 091 代码与测试文件行数约束 (<= 300 行) ===\n"
) ;display
(for-each
  (lambda (f)
    (let ((lines (length (string-split (path-read-text (path f)) "\n"))))
      (check-true (<= lines 300))
    ) ;let
  ) ;lambda
  '("tools/pi/liii/pi/modes/tui/runner.scm"
    "tools/pi/liii/pi/modes/tui/streaming.scm"
    "tools/pi/tests/liii/pi/modes/tui-test.scm" "tools/pi/devel/091.md"
    "tools/pi/devel/091.scm")
) ;for-each

(display "=== 验收点 3: 启动首屏即时可见完整界面 (三分区与非空缓冲区) ===\n"
) ;display
;; 虚拟终端验证 run-tui-flow 启动后首帧绝非全黑
(let ((vt (make-virtual-terminal :width 80 :height 24)))
  (virtual-terminal-push-key! vt 'escape)
  (let ((ret (run-tui-flow '() '() 'tui "" vt)))
    (check ret => 0)
    (check (terminal-cursor-visible? vt) => #t)
    (let ((raw (virtual-terminal-raw vt)) (screen (virtual-terminal-screen vt)))
      ;; 备用屏进出序列正常
      (check-true (not (not (member "\x1b;[?1049h" raw))))
      (check-true (not (not (member "\x1b;[?1049l" raw))))
      ;; 验证首屏绝非全空白，且包含三区关键特征
      (check-true (pair? screen))
      ;; 输入提示符在屏幕上呈现
      (check-true
        (not
          (null? (filter (lambda (line) (string-contains line ">")) screen))
        ) ;not
      ) ;check-true
      ;; 状态栏在屏幕上呈现
      (check-true
        (not
          (null? (filter (lambda (line) (string-contains line "IDLE")) screen))
        ) ;not
      ) ;check-true
    ) ;let
  ) ;let
) ;let

(display "=== 验收点 4: 输入框默认获焦与键盘输入即时可用 ===\n"
) ;display
(let ((vt (make-virtual-terminal :width 80 :height 24)))
  ;; 键入字符并在输入框回显
  (virtual-terminal-push-key! vt "g")
  (virtual-terminal-push-key! vt "o")
  (virtual-terminal-push-key! vt "l")
  (virtual-terminal-push-key! vt "d")
  (virtual-terminal-push-key! vt "f")
  (virtual-terminal-push-key! vt "i")
  (virtual-terminal-push-key! vt "s")
  (virtual-terminal-push-key! vt "h")
  (virtual-terminal-push-key! vt 'escape)
  (let ((ret (run-tui-flow '() '() 'tui "" vt)))
    (check ret => 0)
    (let ((screen (virtual-terminal-screen vt)))
      (check-true
        (not
          (null? (filter (lambda (line) (string-contains line "goldfish")) screen))
        ) ;not
      ) ;check-true
    ) ;let
  ) ;let
) ;let

(display "=== 验收点 5: 终端生命周期与退出安全还原 ===\n")
;; 验证异常发生时 dynamic-wind 亦能 100% 还原光标与备用屏
(let ((vt (make-virtual-terminal :width 80 :height 24)) (caught? #f))
  (catch #t
    (lambda ()
      (with-pi-tui-terminal vt
        (lambda () (error 'abort-test "intentional abort inside tui"))
      ) ;with-pi-tui-terminal
    ) ;lambda
    (lambda (k . args) (set! caught? #t))
  ) ;catch
  (check caught? => #t)
  (check (terminal-cursor-visible? vt) => #t)
  (let ((raw (virtual-terminal-raw vt)))
    (check-true (not (not (member "\x1b;[?1049l" raw))))
  ) ;let
) ;let

(display "=== 验收点 6: 全量单元测试无回归与 TUI 单测通过 ===\n"
) ;display
(let* ((res (%run-with-output "test-all" "bin/gf test tools/pi/tests/liii/" 300))
       (code (car res))
       (out (cdr res))
      ) ;
  (check code => 0)
  (check-true (not (not (string-contains out "Total:  331"))))
  (check-true (not (not (string-contains out "Passed: 331"))))
) ;let*

(check-report)
