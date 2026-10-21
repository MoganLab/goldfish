(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii check)
  (liii error)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii os)
  (liii pi bash)
  (liii pi)
  (liii pi args)
  (liii pi modes)
  (liii pi modes tui)
  (liii tui terminal)
) ;import

(check-set-mode! 'report-failed)

;; [081] 验收脚本：TUI 架构骨架与启动路由（StandardTerminal 切换与安全还原）

(define (%run-ok? tag cmd timeout)
  (let ((tmp (string-append "tmp-081-" tag ".txt")))
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

(display "=== 验收点 2: 任务 081 代码与测试文件行数约束 (<= 300 行) ===\n"
) ;display
(for-each
  (lambda (f)
    (let ((lines (length (string-split (path-read-text (path f)) "\n"))))
      (check-true (<= lines 300))
    ) ;let
  ) ;lambda
  '("tools/pi/liii/pi/args.scm" "tools/pi/liii/pi/cli.scm"
    "tools/pi/liii/pi/modes.scm" "tools/pi/liii/pi/modes/tui.scm"
    "tools/pi/tests/liii/pi/modes/tui-test.scm"
    "tools/pi/tests/liii/pi/resolve-app-mode-test.scm" "tools/pi/devel/081.scm"
    "tools/pi/devel/081.md")
) ;for-each

(display "=== 验收点 3: CLI 参数解析与启动路由支持 TUI 缺省模式 ===\n"
) ;display
;; 3.1 resolve-app-mode 缺省无参判定
;; 当未显式指定 --print、--mode 且 stdin/stdout 均为 TTY 时，解析为 'tui
(check (resolve-app-mode #f #f #t #t) => 'tui)
;; 当标准输入或输出非 TTY 时，退回 'print
(check (resolve-app-mode #f #f #f #t) => 'print)
(check (resolve-app-mode #f #f #t #f) => 'print)
(check (resolve-app-mode #f #f #f #f) => 'print)

;; 3.2 显式参数与覆盖优先级
(check (resolve-app-mode "tui" #f #f #f) => 'tui)
(check (resolve-app-mode 'tui #f #t #t) => 'tui)
(check (resolve-app-mode #f #t #t #t) => 'print)
(check (resolve-app-mode "text" #f #t #t) => 'print)
(check (resolve-app-mode "json" #f #t #t) => 'json)

;; 3.3 结合 parse-pi-args 解析联动
(let ((res (parse-pi-args '())))
  (check (pi-args-ref res 'mode) => #f)
  (check (pi-args-ref res 'print) => #f)
  (check (resolve-app-mode (pi-args-ref res 'mode) (pi-args-ref res 'print) #t #t)
    =>
    'tui
  ) ;check
) ;let

(display "=== 验收点 4: 终端全屏初始化（StandardTerminal）与模式切换 ===\n"
) ;display
;; 4.1 with-pi-tui-terminal 进入时初始化 Alternate screen、隐藏光标
(let ((vt (make-virtual-terminal :width 80 :height 24)) (during-visible #t))
  (check (terminal-cursor-visible? vt) => #t)
  (with-pi-tui-terminal vt
    (lambda () (set! during-visible (terminal-cursor-visible? vt)))
  ) ;with-pi-tui-terminal
  (check during-visible => #f)
  (let ((raw (virtual-terminal-raw vt)))
    (check-true (not (not (member "\x1b;[?1049h" raw))))
  ) ;let
) ;let

(display "=== 验收点 5: dynamic-wind 安全还原兜底（100% 健壮防卡死） ===\n"
) ;display
;; 5.1 正常退出时还原
(let ((vt (make-virtual-terminal :width 80 :height 24)))
  (with-pi-tui-terminal vt (lambda () #t))
  (check (terminal-cursor-visible? vt) => #t)
  (let ((raw (virtual-terminal-raw vt)))
    (check-true (not (not (member "\x1b;[?1049l" raw))))
  ) ;let
) ;let

;; 5.2 抛出异常中断时 100% 恢复光标与退出 Alternate screen
(let ((vt (make-virtual-terminal :width 80 :height 24)) (caught? #f))
  (catch #t
    (lambda ()
      (with-pi-tui-terminal vt
        (lambda () (error 'fatal-test "error inside tui body"))
      ) ;with-pi-tui-terminal
    ) ;lambda
    (lambda _ (set! caught? #t))
  ) ;catch
  (check caught? => #t)
  (check (terminal-cursor-visible? vt) => #t)
  (let ((raw (virtual-terminal-raw vt)))
    (check-true (not (not (member "\x1b;[?1049l" raw))))
  ) ;let
) ;let

(display "=== 验收点 6: 最小事件响应骨架与优雅退出 ===\n")
;; 6.1 按 'escape 退出
(let ((vt (make-virtual-terminal :width 80 :height 24)))
  (virtual-terminal-push-key! vt 'escape)
  (check (run-tui-loop vt) => 0)
) ;let

;; 6.2 按 'q' / 'Q' 退出
(let ((vt1 (make-virtual-terminal :width 80 :height 24))
      (vt2 (make-virtual-terminal :width 80 :height 24))
     ) ;
  (virtual-terminal-push-key! vt1 "q")
  (check (run-tui-loop vt1) => 0)
  (virtual-terminal-push-key! vt2 "Q")
  (check (run-tui-loop vt2) => 0)
) ;let

;; 6.3 忽略其他字符并持续轮询直到退出
(let ((vt (make-virtual-terminal :width 80 :height 24)) (seen '()))
  (virtual-terminal-push-key! vt "a")
  (virtual-terminal-push-key! vt "b")
  (virtual-terminal-push-key! vt 'escape)
  (let ((ret (run-tui-loop vt :on-key (lambda (k) (set! seen (cons k seen))))))
    (check ret => 0)
    (check (reverse seen) => '("a" "b" escape))
  ) ;let
) ;let

(display "=== 验收点 7: 虚拟终端确定性测试覆盖与真实冒烟 ===\n"
) ;display
;; 7.1 run-tui-flow 完整链路闭环
(let ((vt (make-virtual-terminal :width 80 :height 24)))
  (virtual-terminal-push-key! vt 'escape)
  (let ((ret (run-tui-flow '() '() 'tui "" vt)))
    (check ret => 0)
    (check (terminal-cursor-visible? vt) => #t)
  ) ;let
) ;let

;; 7.2 单测子集执行
(check (%run-ok? "tui-test"
         "bin/gf test tools/pi/tests/liii/pi/modes/tui-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "mode-test"
         "bin/gf test tools/pi/tests/liii/pi/resolve-app-mode-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check

;; 7.3 CLI 冒烟：help 输出正常
(check (%run-ok? "cli-help" "bin/gf pi -h" 30) => 0)

(check-report)
