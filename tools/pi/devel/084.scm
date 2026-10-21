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
  (liii tui main-screen)
  (liii tui terminal)
  (liii pi modes tui)
  (liii pi modes tui state)
  (liii pi modes tui loop)
  (liii pi modes tui editor)
  (liii pi modes tui layout)
) ;import

(check-set-mode! 'report-failed)

;; [084] 验收脚本：MainScreen 动态布局划分与窗口 Resize 自适应

(define (%run-ok? tag cmd timeout)
  (let ((tmp (string-append "tmp-084-" tag ".txt")))
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

(display "=== 验收点 2: 任务 084 代码与测试文件行数约束 (<= 300 行) ===\n"
) ;display
(for-each
  (lambda (f)
    (let ((lines (length (string-split (path-read-text (path f)) "\n"))))
      (check-true (<= lines 300))
    ) ;let
  ) ;lambda
  '("tools/pi/liii/pi/modes/tui/layout.scm"
    "tools/pi/liii/pi/modes/tui/loop.scm"
    "tools/pi/liii/pi/modes/tui/loop/opts.scm"
    "tools/pi/liii/pi/modes/tui/state.scm"
    "tools/pi/liii/pi/modes/tui/state/types.scm"
    "tools/pi/liii/pi/modes/tui/state/parse.scm"
    "tools/pi/liii/pi/modes/tui.scm"
    "tools/pi/tests/liii/pi/modes/tui/layout-test.scm"
    "tools/pi/tests/liii/pi/modes/tui/loop-test.scm"
    "tools/pi/tests/liii/pi/modes/tui/state-test.scm" "tools/pi/devel/084.md")
) ;for-each

(display "=== 验收点 3: 界面三段式拓扑划分（Message Viewport / Editor Box / Status Bar） ===\n"
) ;display
(let ((layout (compute-tui-layout 80 24 1)))
  (check-true (tui-layout? layout))
  (check (layout-cols layout) => 80)
  (check (layout-rows layout) => 24)
  (check-false (layout-narrow? layout))
  (check-false (layout-compact? layout))

  ;; 消息视口：顶部 (top: 0, height: 20)
  (let ((msg (layout-messages-rect layout)))
    (check-true (tui-rect? msg))
    (check (rect-top msg) => 0)
    (check (rect-left msg) => 0)
    (check (rect-width msg) => 80)
    (check (rect-height msg) => 20)
  ) ;let

  ;; 输入框：中部 (top: 20, height: 3)
  (let ((ed (layout-editor-rect layout)))
    (check-true (tui-rect? ed))
    (check (rect-top ed) => 20)
    (check (rect-left ed) => 0)
    (check (rect-width ed) => 80)
    (check (rect-height ed) => 3)
  ) ;let

  ;; 状态栏：底部 (top: 23, height: 1)
  (let ((st (layout-status-rect layout)))
    (check-true (tui-rect? st))
    (check (rect-top st) => 23)
    (check (rect-left st) => 0)
    (check (rect-width st) => 80)
    (check (rect-height st) => 1)
  ) ;let
) ;let

(display "=== 验收点 4: 输入框高度自适应弹性约束（compute-editor-height） ===\n"
) ;display
(check (compute-editor-height 24 1) => 3)
(check (compute-editor-height 24 2) => 4)
(check (compute-editor-height 24 5) => 7)
;; 24 行终端下最大 40% 为 9 行
(check (compute-editor-height 24 15) => 9)
;; 50 行终端下最大 40% 为 20 行
(check (compute-editor-height 50 10) => 12)
(check (compute-editor-height 50 25) => 20)
;; 极小屏幕保底
(check (compute-editor-height 5 1) => 3)
(check (compute-editor-height 2 1) => 2)
;; 参数类型校验
(check-catch 'type-error (compute-editor-height 0 1))
(check-catch 'type-error (compute-editor-height 24 -1))

(display "=== 验收点 5: 终端尺寸动态感知（detect-terminal-dimensions） ===\n"
) ;display
(let ((vt (make-virtual-terminal :width 120 :height 35)))
  (let-values (((cols rows) (detect-terminal-dimensions vt)))
    (check cols => 120)
    (check rows => 35)
  ) ;let-values
) ;let
(check-catch 'type-error (detect-terminal-dimensions "invalid-terminal"))

(display "=== 验收点 6: 窄屏与紧凑屏幕降级（Narrow / Compact Adaptation） ===\n"
) ;display
;; 窄屏宽度 < 60
(let ((l-narrow (compute-tui-layout 55 24 1)))
  (check-true (layout-narrow? l-narrow))
  (check-true (layout-compact? l-narrow))
  (check (rect-width (layout-messages-rect l-narrow)) => 55)
  (check (rect-width (layout-editor-rect l-narrow)) => 55)
  (check (rect-width (layout-status-rect l-narrow)) => 55)
) ;let

;; 紧凑屏高度 < 15
(let ((l-compact (compute-tui-layout 80 12 1)))
  (check-false (layout-narrow? l-compact))
  (check-true (layout-compact? l-compact))
  (check (rect-height (layout-status-rect l-compact)) => 1)
  (check (rect-height (layout-editor-rect l-compact)) => 3)
  (check (rect-height (layout-messages-rect l-compact)) => 8)
) ;let

;; 极小高度屏幕 (< 10) 状态栏折叠为 0
(let ((l-tiny (compute-tui-layout 80 8 1)))
  (check-true (layout-compact? l-tiny))
  (check (rect-height (layout-status-rect l-tiny)) => 0)
  (check (rect-height (layout-editor-rect l-tiny)) => 3)
  (check (rect-height (layout-messages-rect l-tiny)) => 5)
) ;let

(display "=== 验收点 7: 窗口 Resize 监听与 screen-resize! 自适应驱动 ===\n"
) ;display
(let* ((vt (make-virtual-terminal :width 80 :height 24)) (ms (make-main-screen vt)))
  ;; 无变化返回 #f
  (check-false (handle-screen-resize! ms 80 24 80 24))
  ;; 变动返回 #t 并调整屏幕尺寸
  (check-true (handle-screen-resize! ms 80 24 100 30))
  (let ((sz (terminal-size vt)))
    (check (car sz) => 100)
    (check (cadr sz) => 30)
  ) ;let
) ;let*
(check-catch 'type-error (handle-screen-resize! "not-screen" 80 24 100 30))

(display "=== 验收点 8: State 状态机与 loop Resize 协调 ===\n")
(let ((st (make-pi-tui-state :cols 80 :rows 24)))
  (check (state-cols st) => 80)
  (check (state-rows st) => 24)

  ;; state-with-dimensions 纯函数更新
  (let ((st2 (state-with-dimensions st 120 40)))
    (check (state-cols st) => 80)
    (check (state-rows st) => 24)
    (check (state-cols st2) => 120)
    (check (state-rows st2) => 40)
    (check-true (state-dirty? st2))
  ) ;let

  ;; state-set-dimensions! 原地更新并置脏
  (state-clear-dirty! st)
  (state-set-dimensions! st 90 28)
  (check (state-cols st) => 90)
  (check (state-rows st) => 28)
  (check-true (state-dirty? st))
) ;let

(display "=== 验收点 9: 虚拟终端 E2E Resize 与输入共存驱动 ===\n")
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (ms (make-main-screen vt))
       (ed (make-pi-tui-editor))
       (st (make-pi-tui-state :input-editor ed :cols 80 :rows 24))
       (render-called-with-new-size? #f)
      ) ;
  ;; 注入输入内容
  (virtual-terminal-push-key! vt "r")
  (virtual-terminal-push-key! vt "e")
  (virtual-terminal-push-key! vt "s")
  (virtual-terminal-push-key! vt "i")
  (virtual-terminal-push-key! vt "z")
  (virtual-terminal-push-key! vt "e")

  ;; 模拟外部调整终端大小
  (virtual-terminal-resize! vt 110 32)
  (virtual-terminal-push-key! vt 'escape)

  (let ((final-st
          (run-tui-loop vt
            :state        st
            :screen       ms
            :return-state #t
            :render
            (lambda (s term)
              (when (and (= (state-cols s) 110) (= (state-rows s) 32))
                (set! render-called-with-new-size? #t)
              ) ;when
            ) ;lambda
          ) ;run-tui-loop
        ) ;final-st
       ) ;
    (check (state-status final-st) => 'exit)
    (check (state-cols final-st) => 110)
    (check (state-rows final-st) => 32)
    (check (pi-tui-editor-text ed) => "resize")
    (check-true render-called-with-new-size?)
  ) ;let
) ;let*

(display "=== 验收点 10: tools/pi 全量单测运行（断言 Total: 324, Passed: 324） ===\n"
) ;display
(let* ((test-cmd "bin/gf test tools/pi/tests/liii/ > tmp-084-summary.txt 2>&1")
       (status (run (list (pi-find-bash-path) "-c" test-cmd) :stdin 'null :timeout 300)
       ) ;status
       (out (if (file-exists? "tmp-084-summary.txt")
              (path-read-text (path "tmp-084-summary.txt"))
              ""
            ) ;if
       ) ;out
      ) ;
  (when (file-exists? "tmp-084-summary.txt")
    (delete-file "tmp-084-summary.txt")
  ) ;when
  (check status => 0)
  (check-true (string-contains out "Total:  324"))
  (check-true (string-contains out "Passed: 324"))
) ;let*

(check-report)
