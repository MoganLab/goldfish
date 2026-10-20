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
  (liii tui terminal)
  (liii pi modes tui)
  (liii pi modes tui state)
  (liii pi modes tui loop)
) ;import

(check-set-mode! 'report-failed)

;; [082] 验收脚本：TUI 会话状态机与主事件循环驱动器

(define (%run-ok? tag cmd timeout)
  (let ((tmp (string-append "tmp-082-" tag ".txt")))
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

(display "=== 验收点 2: 任务 082 代码与测试文件行数约束 (<= 300 行) ===\n"
) ;display
(for-each
  (lambda (f)
    (let ((lines (length (string-split (path-read-text (path f)) "\n"))))
      (check-true (<= lines 300))
    ) ;let
  ) ;lambda
  '("tools/pi/liii/pi/modes/tui/state/types.scm"
    "tools/pi/liii/pi/modes/tui/state.scm" "tools/pi/liii/pi/modes/tui/loop.scm"
    "tools/pi/liii/pi/modes/tui.scm"
    "tools/pi/tests/liii/pi/modes/tui/state-test.scm"
    "tools/pi/tests/liii/pi/modes/tui/loop-test.scm" "tools/pi/devel/082.md")
) ;for-each

(display "=== 验收点 3: pi-tui-state 核心会话状态模型与原语 ===\n"
) ;display
;; 3.1 默认构造与字段取值
(let ((st (make-pi-tui-state)))
  (check-true (pi-tui-state? st))
  (check (state-status st) => 'idle)
  (check (state-messages st) => '())
  (check (state-input-editor st) => "")
  (check (state-active-tool st) => #f)
  (check (state-scroll-offset st) => 0)
  (check (state-dirty? st) => #t)
  ;; 别名访问器
  (check (pi-tui-state-status st) => 'idle)
  (check (pi-tui-state-messages st) => '())
  (check (pi-tui-state-input-editor st) => "")
  (check (pi-tui-state-active-tool st) => #f)
  (check (pi-tui-state-scroll-offset st) => 0)
  (check (pi-tui-state-dirty? st) => #t)
) ;let

;; 3.2 自定义命名参数构造
(let ((st (make-pi-tui-state
            :status        'running
            :messages      '((user . "hello"))
            :input-editor  "draft"
            :active-tool   'bash
            :scroll-offset 2
            :dirty?        #f
          ) ;make-pi-tui-state
      ) ;st
     ) ;
  (check (state-status st) => 'running)
  (check (state-messages st) => '((user . "hello")))
  (check (state-input-editor st) => "draft")
  (check (state-active-tool st) => 'bash)
  (check (state-scroll-offset st) => 2)
  (check (state-dirty? st) => #f)
) ;let

;; 3.3 状态谓词验证
(check-true (state-valid-status? 'idle))
(check-true (state-valid-status? 'running))
(check-true (state-valid-status? 'streaming))
(check-true (state-valid-status? 'confirm))
(check-true (state-valid-status? 'exit))
(check-false (state-valid-status? 'unknown))
(check-false (state-valid-status? "idle"))

;; 3.4 纯函数式状态更新（state-with-*）
(let* ((s0 (make-pi-tui-state :dirty? #f))
       (s1 (state-with-status s0 'running))
       (s2 (state-with-messages s1 '((assistant . "done"))))
       (s3 (state-with-input-editor s2 "buffer"))
       (s4 (state-with-active-tool s3 'read))
       (s5 (state-with-scroll-offset s4 5))
       (s6 (state-with-dirty s5 #f))
      ) ;
  (check (state-status s0) => 'idle)
  (check (state-dirty? s0) => #f)
  (check (state-status s1) => 'running)
  (check (state-dirty? s1) => #t)
  (check (state-messages s2) => '((assistant . "done")))
  (check (state-dirty? s2) => #t)
  (check (state-input-editor s3) => "buffer")
  (check (state-active-tool s4) => 'read)
  (check (state-scroll-offset s5) => 5)
  (check (state-dirty? s6) => #f)
) ;let*

;; 3.5 受控原地变异（state-set-*! 与 dirty 控制）
(let ((st (make-pi-tui-state)))
  (check (state-dirty? st) => #t)
  (state-clear-dirty! st)
  (check (state-dirty? st) => #f)
  (state-set-status! st 'streaming)
  (check (state-status st) => 'streaming)
  (check (state-dirty? st) => #t)
  (state-clear-dirty! st)
  (state-set-messages! st '((user . "hi")))
  (check (state-messages st) => '((user . "hi")))
  (check (state-dirty? st) => #t)
  (state-clear-dirty! st)
  (state-set-input-editor! st "changed")
  (check (state-input-editor st) => "changed")
  (check (state-dirty? st) => #t)
  (state-clear-dirty! st)
  (state-set-active-tool! st 'bash)
  (check (state-active-tool st) => 'bash)
  (check (state-dirty? st) => #t)
  (state-clear-dirty! st)
  (state-set-scroll-offset! st 8)
  (check (state-scroll-offset st) => 8)
  (check (state-dirty? st) => #t)
  (state-clear-dirty! st)
  (state-mark-dirty! st)
  (check (state-dirty? st) => #t)
) ;let

;; 3.6 状态异常防护
(let ((st (make-pi-tui-state)))
  (check-catch 'type-error (make-pi-tui-state :status 'invalid))
  (check-catch 'type-error (make-pi-tui-state :messages "str"))
  (check-catch 'type-error (make-pi-tui-state :scroll-offset -2))
  (check-catch 'type-error (state-with-status st 'invalid))
  (check-catch 'type-error (state-with-scroll-offset st -1))
  (check-catch 'type-error (state-set-status! st 'invalid))
  (check-catch 'type-error (state-set-scroll-offset! st -1))
) ;let

(display "=== 验收点 4: 主事件循环驱动器与节拍控制 ===\n")
;; 4.1 exit 状态跳出返回 0
(let ((vt (make-virtual-terminal :width 80 :height 24))
      (st (make-pi-tui-state :status 'exit))
     ) ;
  (check (run-tui-loop vt :state st) => 0)
) ;let

;; 4.2 max-ticks 节拍控制
(let ((vt (make-virtual-terminal :width 80 :height 24)) (ticks-ran 0))
  (let ((ret
          (run-tui-loop vt
            :max-ticks 6
            :interval  0.001
            :on-tick   (lambda (s t) (set! ticks-ran (+ ticks-ran 1)))
          ) ;run-tui-loop
        ) ;ret
       ) ;
    (check ret => 0)
    (check ticks-ran => 6)
  ) ;let
) ;let

;; 4.3 循环参数异常类型检查
(check-catch 'type-error (run-tui-loop "not-terminal"))
(check-catch 'type-error (run-tui-loop (make-virtual-terminal) :state "bad"))
(check-catch 'type-error (run-tui-loop (make-virtual-terminal) :render "bad"))
(check-catch 'type-error (run-tui-loop (make-virtual-terminal) :on-key "bad"))
(check-catch 'type-error (run-tui-loop (make-virtual-terminal) :on-tick "bad"))

(display "=== 验收点 5: 脏标记与 render 调度机制 ===\n")
;; 仅在 dirty? 时触发 render，且触发后清零 dirty?
(let ((vt (make-virtual-terminal :width 80 :height 24))
      (st (make-pi-tui-state :dirty? #t))
      (render-count 0)
     ) ;
  (run-tui-loop vt
    :state     st
    :max-ticks 4
    :interval  0.001
    :render    (lambda (s term) (set! render-count (+ render-count 1)))
    :on-tick   (lambda (s tick) (when (= tick 2) (state-mark-dirty! s)))
  ) ;run-tui-loop
  ;; tick 0 初始渲染 1 次，tick 2 标记 dirty 后在下一个可渲染时机渲染 1 次，共 2 次
  (check render-count => 2)
  (check (state-dirty? st) => #f)
) ;let

(display "=== 验收点 6: VirtualTerminal 确定性 TDD 状态流转 ===\n")
;; 6.1 注入按键流驱动状态完整迁移并返回最终状态
(let ((vt (make-virtual-terminal :width 80 :height 24))
      (st (make-pi-tui-state))
      (history '())
     ) ;
  (virtual-terminal-push-key! vt "r")
  (virtual-terminal-push-key! vt "s")
  (virtual-terminal-push-key! vt "c")
  (virtual-terminal-push-key! vt 'escape)

  (let ((final-st
          (run-tui-loop vt
            :state        st
            :return-state #t
            :interval     0.001
            :render
            (lambda (s term) (set! history (cons (state-status s) history)))
            :on-key
            (lambda (s key)
              (cond ((equal? key "r") (state-with-status s 'running))
                    ((equal? key "s") (state-with-status s 'streaming))
                    ((equal? key "c") (state-with-status s 'confirm))
                    (else s)
              ) ;cond
            ) ;lambda
          ) ;run-tui-loop
        ) ;final-st
       ) ;
    (check-true (pi-tui-state? final-st))
    (check (state-status final-st) => 'exit)
    (check (reverse history) => '(idle running streaming confirm))
  ) ;let
) ;let

;; 6.2 默认退出按键 "q" 测试
(let ((vt (make-virtual-terminal :width 80 :height 24)) (st (make-pi-tui-state)))
  (virtual-terminal-push-key! vt "q")
  (let ((final-st (run-tui-loop vt :state st :return-state #t :interval 0.001)))
    (check (state-status final-st) => 'exit)
  ) ;let
) ;let

(display "=== 验收点 7: 门禁复核：全量 322 个单测 PASS ===\n")
(check (%run-ok? "all-tests" "bin/gf test tools/pi/tests/liii/" 300) => 0)

(check-report)
