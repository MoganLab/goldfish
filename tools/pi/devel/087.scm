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
  (liii pi modes tui)
  (liii pi modes tui state)
  (liii pi modes tui loop dispatch)
  (liii pi modes tui viewport)
  (liii pi modes tui editor)
  (liii pi modes tui thinking)
) ;import

(check-set-mode! 'report-failed)

;; [087] 验收脚本：Thinking 推理思考过程动态折叠卡片组件

(define (%run-ok? tag cmd timeout)
  (let* ((tmp (string-append "tmp-087-" tag ".txt"))
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
  (let* ((tmp (string-append "tmp-087-" tag ".txt"))
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

(display "=== 验收点 2: 任务 087 代码与测试文件行数约束 (<= 300 行) ===\n"
) ;display
(for-each
  (lambda (f)
    (let ((lines (length (string-split (path-read-text (path f)) "\n"))))
      (check-true (<= lines 300))
    ) ;let
  ) ;lambda
  '("tools/pi/liii/pi/modes/tui/thinking.scm"
    "tools/pi/liii/pi/modes/tui/state.scm"
    "tools/pi/liii/pi/modes/tui/viewport.scm"
    "tools/pi/liii/pi/modes/tui/loop/dispatch.scm"
    "tools/pi/liii/pi/modes/tui.scm"
    "tools/pi/tests/liii/pi/modes/tui/thinking-test.scm" "tools/pi/devel/087.md"
    "tools/pi/devel/087.scm")
) ;for-each

(display "=== 验收点 3: Thinking 卡片状态机设计与类型校验 ===\n")
(let ((card (make-pi-tui-thinking-card)))
  (check-true (pi-tui-thinking-card? card))
  (check (thinking-card-content card) => "")
  (check (thinking-card-token-count card) => 0)
  (check (thinking-card-collapsed? card) => #t)
  (check (thinking-card-completed? card) => #f)
  (check (thinking-card-scroll-offset card) => 0)
) ;let

(let ((card (make-pi-tui-thinking-card
              :content       "thought"
              :token-count   42
              :collapsed?    #f
              :completed?    #t
              :scroll-offset 2
            ) ;make-pi-tui-thinking-card
      ) ;card
     ) ;
  (check (thinking-card-content card) => "thought")
  (check (thinking-card-token-count card) => 42)
  (check (thinking-card-collapsed? card) => #f)
  (check (thinking-card-completed? card) => #t)
  (check (thinking-card-scroll-offset card) => 2)
) ;let

;; 非法参数拦截抛出 type-error
(check-catch 'type-error (make-pi-tui-thinking-card :content 123))
(check-catch 'type-error (make-pi-tui-thinking-card :token-count -1))
(check-catch 'type-error (make-pi-tui-thinking-card :collapsed? "invalid"))
(check-catch 'type-error (make-pi-tui-thinking-card :completed? 1))
(check-catch 'type-error (make-pi-tui-thinking-card :scroll-offset -1))

;; 状态修改与切换
(let ((card (make-pi-tui-thinking-card :collapsed? #t)))
  (check (thinking-card-collapsed? (thinking-card-toggle! card)) => #f)
  (check (thinking-card-collapsed? (thinking-card-toggle! card)) => #t)
) ;let

(let* ((c1 (make-pi-tui-thinking-card :collapsed? #t))
       (c2 (thinking-card-with-toggle c1))
      ) ;
  (check (thinking-card-collapsed? c1) => #t)
  (check (thinking-card-collapsed? c2) => #f)
) ;let*

(let ((card (make-pi-tui-thinking-card :content "Part 1. ")))
  (thinking-card-append-content! card "Part 2.")
  (check (thinking-card-content card) => "Part 1. Part 2.")
  (thinking-card-set-completed! card #t)
  (check (thinking-card-completed? card) => #t)
  (thinking-card-set-token-count! card 88)
  (check (thinking-card-token-count card) => 88)
  (thinking-card-set-scroll-offset! card 4)
  (check (thinking-card-scroll-offset card) => 4)
) ;let

(display "=== 验收点 4: 折叠态单行紧凑摘要渲染 ===\n")
;; 思考进行中摘要与已完成摘要
(let* ((c1 (make-pi-tui-thinking-card :token-count 55 :completed? #f))
       (l1 (render-thinking-collapsed c1 80))
       (c2 (make-pi-tui-thinking-card :completed? #t))
       (l2 (render-thinking-collapsed c2 80))
       (l3 (render-thinking-collapsed c2 10))
      ) ;
  (check (length l1) => 1)
  (check (strip-ansi (car l1))
    =>
    "> [Thinking] 正在深度思考 (55 tokens)..."
  ) ;check
  (check (length l2) => 1)
  (check (strip-ansi (car l2)) => "> [Thinking] 推理已完成 (按 Tab 展开)")
  (check-true
    (<= (display-width (strip-ansi (car l3))) 10)
  ) ;check-true
) ;let*

(display "=== 验收点 5: 展开态 Box 边框包裹与内部独立视口排版 ===\n"
) ;display
(let* ((card (make-pi-tui-thinking-card
               :content    "First line of thought\nSecond line of thought"
               :collapsed? #f
             ) ;make-pi-tui-thinking-card
       ) ;card
       (lines (render-thinking-expanded card 40))
      ) ;
  (check-true (>= (length lines) 3))
  (check-true (string-contains (car lines) "Thinking"))
  (check-true (string-contains (car (reverse lines)) "┘"))
  (for-each
    (lambda (l) (check-true (<= (display-width (strip-ansi l)) 40)))
    lines
  ) ;for-each
) ;let*

;; 展开态高度受限、统一调度与视口集成
(let* ((c1 (make-pi-tui-thinking-card :content "1\n2\n3\n4\n5\n6"))
       (c2 (make-pi-tui-thinking-card :collapsed? #t))
       (msgs (list (cons 'user "prompt") c2))
      ) ;
  (check (length (render-thinking-expanded c1 40 5)) => 5)
  (check (length (render-thinking-card c2 50)) => 1)
  (thinking-card-toggle! c2)
  (check-true (>= (length (render-thinking-card c2 50)) 3))
  (thinking-card-toggle! c2)
  (check (length (render-messages-viewport msgs 60 10 0)) => 2)
  (thinking-card-toggle! c2)
  (check-true (>= (length (render-messages-viewport msgs 60 10 0)) 4))
) ;let*

(display "=== 验收点 6: 快捷键调度与折叠状态切换（Tab / Ctrl+O） ===\n"
) ;display
(let* ((card (make-pi-tui-thinking-card :content "analyzing code"))
       (st (make-pi-tui-state :messages (list (cons 'user "hi") card)))
      ) ;
  (check-true (pi-tui-thinking-card? (state-find-active-thinking-card st)))
  (state-clear-dirty! st)
  (check-true (pi-tui-thinking-card? (state-toggle-thinking-card! st)))
  (check (thinking-card-collapsed? card) => #f)
  (check-true (state-dirty? st))
  (state-toggle-thinking-card! st)
  (check (thinking-card-collapsed? card) => #t)
) ;let*

;; 输入框为空与非空下的按键调度
(let* ((c1 (make-pi-tui-thinking-card :collapsed? #t))
       (st1 (make-pi-tui-state :messages (list c1)))
       (c2 (make-pi-tui-thinking-card :collapsed? #t))
       (ed (make-pi-tui-editor :text "echo test"))
       (st2 (make-pi-tui-state :messages (list c2) :input-editor ed))
      ) ;
  (%dispatch-tui-key st1 'tab #f)
  (check (thinking-card-collapsed? c1) => #f)
  (%dispatch-tui-key st1 'tab #f)
  (check (thinking-card-collapsed? c1) => #t)
  (%dispatch-tui-key st2 'tab #f)
  (check (thinking-card-collapsed? c2) => #t)
  (%dispatch-tui-key st2 'ctrl-o #f)
  (check (thinking-card-collapsed? c2) => #f)
  (%dispatch-tui-key st2 "\x0f;" #f)
  (check (thinking-card-collapsed? c2) => #t)
) ;let*

(display "=== 验收点 7: 门面导出接口完整性 ===\n")
(for-each (lambda (proc) (check-true (procedure? proc)))
  (list make-pi-tui-thinking-card pi-tui-thinking-card? thinking-card-content
    thinking-card-token-count thinking-card-collapsed? thinking-card-completed?
    thinking-card-scroll-offset thinking-card-toggle! thinking-card-with-toggle
    thinking-card-append-content! thinking-card-set-completed!
    render-thinking-collapsed render-thinking-expanded render-thinking-card
    state-find-active-thinking-card state-toggle-thinking-card!
  ) ;list
) ;for-each

(display "=== 验收点 8: 真实冒烟（非 TTY 下启动 bin/gf pi 打印帮助并以 0 退出） ===\n"
) ;display
(let ((res (%run-with-output "smoke" "bin/gf pi" 30)))
  (check (car res) => 0)
  (check-true (string-contains (cdr res) "gf pi [options]"))
) ;let

(display "=== 验收点 9: 全量测试套件执行并硬断言总计数与通过数（Total:  328, Passed: 328） ===\n"
) ;display
(let ((res (%run-with-output "all-tests" "bin/gf test tools/pi/tests/liii/" 300)))
  (check (car res) => 0)
  (let ((out (cdr res)))
    (check-true (string-contains out "Total:  328"))
    (check-true (string-contains out "Passed: 328"))
  ) ;let
) ;let

(check-report)
