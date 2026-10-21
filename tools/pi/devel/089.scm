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
  (liii pi extension)
  (liii pi modes tui)
  (liii pi modes tui state)
  (liii pi modes tui loop)
  (liii pi modes tui editor)
  (liii pi modes tui slash-menu)
) ;import

(check-set-mode! 'report-failed)

;; [089] 验收脚本：Slash 命令悬浮菜单与动态自动补全

(define (%run-ok? tag cmd timeout)
  (let* ((tmp (string-append "tmp-089-" tag ".txt"))
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

(display "=== 验收点 1: 门禁检查（构建 / fmt / check / doc） ===\n")
(check (%run-ok? "build" "xmake b goldfish" 300) => 0)
(check (%run-ok? "fmt-chk" "bin/gf fmt --dry-run" 120) => 0)
(check (%run-ok? "check" "bin/gf check" 120) => 0)
(check (%run-ok? "doc" "bin/gf doc --build-json" 120) => 0)

(display "=== 验收点 2: 任务 089 代码与测试文件行数约束 (<= 300 行) ===\n"
) ;display
(for-each
  (lambda (f)
    (let ((lines (length (string-split (path-read-text (path f)) "\n"))))
      (check-true (<= lines 300))
    ) ;let
  ) ;lambda
  '("tools/pi/liii/pi/modes/tui/slash-menu.scm"
    "tools/pi/liii/pi/modes/tui/slash-menu/types.scm"
    "tools/pi/liii/pi/modes/tui/slash-menu/render.scm"
    "tools/pi/liii/pi/modes/tui/state.scm"
    "tools/pi/liii/pi/modes/tui/state/slash.scm"
    "tools/pi/liii/pi/modes/tui/state/pure.scm"
    "tools/pi/liii/pi/modes/tui/loop/dispatch.scm"
    "tools/pi/liii/pi/modes/tui.scm"
    "tools/pi/tests/liii/pi/modes/tui/slash-menu-test.scm"
    "tools/pi/devel/089.md" "tools/pi/devel/089.scm")
) ;for-each

(display "=== 验收点 3: Slash 命令数据模型、扩展收集与兜底内置命令 ===\n"
) ;display
;; 构造基础 SlashCommand
(let ((cmd (make-pi-tui-slash-command "/test" "测试命令描述")))
  (check-true (pi-tui-slash-command? cmd))
  (check (slash-command-name cmd) => "/test")
  (check (slash-command-description cmd) => "测试命令描述")
  (check (slash-command-handler cmd) => #f)
) ;let

;; 非法参数类型抛出 type-error
(check-catch 'type-error (make-pi-tui-slash-command 123 "desc"))
(check-catch 'type-error (make-pi-tui-slash-command "" "desc"))
(check-catch 'type-error (make-pi-tui-slash-command "/test" 456))
(check-catch 'type-error
  (make-pi-tui-slash-command "/test" "desc" "not-a-proc")
) ;check-catch

;; normalize-slash-command 标准化
(let* ((def (make-extension-command-def "review" "审查" (lambda _ "ok")))
       (c1 (normalize-slash-command def))
      ) ;
  (check-true (pi-tui-slash-command? c1))
  (check (slash-command-name c1) => "/review")
  (check (slash-command-description c1) => "审查")
  (check-true (procedure? (slash-command-handler c1)))
) ;let*

;; 兜底内置命令验证（/help, /exit, /clear）
(let ((cmds (collect-slash-commands #f)))
  (check-true (list? cmds))
  (check-true (>= (length cmds) 3))
  (check-true (and (member "/help" (map slash-command-name cmds)) #t))
  (check-true (and (member "/exit" (map slash-command-name cmds)) #t))
  (check-true (and (member "/clear" (map slash-command-name cmds)) #t))
) ;let

;; 从 ExtensionRegistry 动态收集与去重
(let* ((reg (make-extension-registry))
       (def1 (make-extension-command-def "tdd" "TDD 模式" (lambda _ #t)))
       (def2 (make-extension-command-def "/review" "代码评审" (lambda _ #t)))
       (def3 (make-extension-command-def "review" "代码评审别名" (lambda _ #t)))
      ) ;
  (register-command! reg def1)
  (register-command! reg def2)
  (register-command! reg def3)
  (let ((cmds (collect-slash-commands reg)))
    (check-true (and (member "/tdd" (map slash-command-name cmds)) #t))
    (check-true (and (member "/review" (map slash-command-name cmds)) #t))
    (check-true (and (member "/help" (map slash-command-name cmds)) #t))
    (let ((reviews
            (filter (lambda (c) (string=? (slash-command-name c) "/review")) cmds)
          ) ;reviews
         ) ;
      (check (length reviews) => 1)
    ) ;let
  ) ;let
) ;let*

;; SlashMenu 构造校验与非法参数防卫
(let ((menu (make-pi-tui-slash-menu)))
  (check-true (pi-tui-slash-menu? menu))
  (check-false (slash-menu-visible? menu))
  (check (slash-menu-query menu) => "")
  (check (slash-menu-selected-index menu) => 0)
) ;let
(check-catch 'type-error (make-pi-tui-slash-menu :visible? "yes"))
(check-catch 'type-error (make-pi-tui-slash-menu :query 123))
(check-catch 'type-error (make-pi-tui-slash-menu :selected-index -1))
(check-catch 'type-error (make-pi-tui-slash-menu :commands "not-list"))

(display "=== 验收点 4: 输入感知、弹出触发与动态前缀过滤 ===\n"
) ;display
(let* ((c-tdd (make-pi-tui-slash-command "/tdd" "测试驱动"))
       (c-rev (make-pi-tui-slash-command "/review" "评审"))
       (c-sim (make-pi-tui-slash-command "/simplify" "精简"))
       (c-hlp (make-pi-tui-slash-command "/help" "帮助"))
       (menu (make-pi-tui-slash-menu :commands (list c-tdd c-rev c-sim c-hlp)))
      ) ;
  ;; 键入 / 弹出全量候选
  (slash-menu-update-query! menu "/")
  (check-true (slash-menu-visible? menu))
  (check (length (slash-menu-candidates menu)) => 4)

  ;; 动态前缀过滤
  (slash-menu-update-query! menu "/td")
  (check-true (slash-menu-visible? menu))
  (check (length (slash-menu-candidates menu)) => 1)
  (check (slash-command-name (car (slash-menu-candidates menu))) => "/tdd")

  ;; 大小写不敏感过滤
  (slash-menu-update-query! menu "/SIM")
  (check-true (slash-menu-visible? menu))
  (check (length (slash-menu-candidates menu)) => 1)
  (check (slash-command-name (car (slash-menu-candidates menu))) => "/simplify")

  ;; 无匹配情况
  (slash-menu-update-query! menu "/unknown")
  (check-true (slash-menu-visible? menu))
  (check-true (null? (slash-menu-candidates menu)))

  ;; 非 / 开头自动隐藏
  (slash-menu-update-query! menu "hello")
  (check-false (slash-menu-visible? menu))

  ;; 清空自动隐藏
  (slash-menu-update-query! menu "")
  (check-false (slash-menu-visible? menu))

  ;; 出现空格（已完成命令开始输参数）自动隐藏
  (slash-menu-update-query! menu "/tdd test.scm")
  (check-false (slash-menu-visible? menu))
) ;let*

(display "=== 验收点 5: 键盘导航（上下循环切换）与一键补全/取消 ===\n"
) ;display
(let* ((c1 (make-pi-tui-slash-command "/a" "A"))
       (c2 (make-pi-tui-slash-command "/b" "B"))
       (c3 (make-pi-tui-slash-command "/c" "C"))
       (menu (make-pi-tui-slash-menu :commands (list c1 c2 c3) :visible? #t))
      ) ;
  (check (slash-menu-selected-index menu) => 0)
  (check (slash-command-name (slash-menu-selected-command menu)) => "/a")

  ;; 下移
  (slash-menu-select-next! menu)
  (check (slash-menu-selected-index menu) => 1)
  ;; 下移到底部循环到首项
  (slash-menu-select-next! menu)
  (check (slash-menu-selected-index menu) => 2)
  (slash-menu-select-next! menu)
  (check (slash-menu-selected-index menu) => 0)

  ;; 上移循环到底部
  (slash-menu-select-prev! menu)
  (check (slash-menu-selected-index menu) => 2)
) ;let*

;; Tab/Enter 自动补全测试
(let* ((c-tdd (make-pi-tui-slash-command "/tdd" "测试驱动开发"))
       (menu (make-pi-tui-slash-menu :commands (list c-tdd) :visible? #t))
       (ed (make-pi-tui-editor :text "/t"))
      ) ;
  (slash-menu-update-query! menu "/t")
  (check-true (slash-menu-visible? menu))

  (let ((completed? (slash-menu-complete! menu ed)))
    (check-true completed?)
    ;; 自动补全命令名称并在尾部追加空格
    (check (pi-tui-editor-text ed) => "/tdd ")
    ;; 菜单关闭隐藏
    (check-false (slash-menu-visible? menu))
    ;; 光标位于追加空格后
    (check (pi-tui-editor-cursor ed) => '(0 5))
  ) ;let
) ;let*

;; Esc 隐藏取消测试
(let* ((c-tdd (make-pi-tui-slash-command "/tdd" "测试驱动开发"))
       (menu (make-pi-tui-slash-menu :commands (list c-tdd) :visible? #t))
      ) ;
  (slash-menu-hide! menu)
  (check-false (slash-menu-visible? menu))
  (slash-menu-show! menu)
  (check-true (slash-menu-visible? menu))
) ;let*

(display "=== 验收点 6: Box + VStack 浮层渲染排版 ===\n")
(let* ((c1 (make-pi-tui-slash-command "/tdd" "测试驱动开发"))
       (c2 (make-pi-tui-slash-command "/help" "显示帮助"))
       (menu (make-pi-tui-slash-menu :commands (list c1 c2) :visible? #t))
       (rendered (render-slash-menu menu 40))
      ) ;
  (check-true (list? rendered))
  (check-true (>= (length rendered) 4))
  ;; 第一行包含单线 Box 顶边框及 Commands 标题
  (check-true (string-starts? (car rendered) "┌"))
  ;; 包含命令名与高亮标记
  (check-true (string-contains? (cadr rendered) "/tdd"))
  (check-true (or (string-contains? (cadr rendered) ">")
                (string-contains? (caddr rendered) ">")
              ) ;or
  ) ;check-true
  ;; 包含底边框
  (check-true (string-starts? (car (reverse rendered)) "└"))
) ;let*

;; 无匹配时渲染提示
(let* ((menu (make-pi-tui-slash-menu :commands '() :candidates '() :visible? #t))
       (rendered (render-slash-menu menu 30))
      ) ;
  (check-true (list? rendered))
  (check-true (string-contains? (cadr rendered) "无匹配命令"))
) ;let*

(display "=== 验收点 7: TUI State 与事件循环按键调度集成 ===\n")
(let* ((st (make-pi-tui-state :input-editor (make-pi-tui-editor :text ""))))
  (check-true (pi-tui-slash-menu? (state-slash-menu st)))
  (check-false (state-slash-menu-visible? st))

  ;; 输入 / 并同步状态
  (pi-tui-editor-set-text! (state-input-editor st) "/")
  (state-sync-slash-menu! st)
  (check-true (state-slash-menu-visible? st))
  (check-true (state-dirty? st))

  ;; 键盘事件调度：Down 改变高亮项
  (state-clear-dirty! st)
  (let ((handled? (dispatch-slash-menu-key! st 'down)))
    (check-true handled?)
    (check (slash-menu-selected-index (state-slash-menu st)) => 1)
    (check-true (state-dirty? st))
  ) ;let

  ;; 键盘事件调度：Esc 仅关闭菜单而不退出主程序
  (state-clear-dirty! st)
  (let ((handled? (dispatch-slash-menu-key! st 'escape)))
    (check-true handled?)
    (check-false (state-slash-menu-visible? st))
    (check (state-status st) => 'idle)
    (check (pi-tui-editor-text (state-input-editor st)) => "/")
    (check-true (state-dirty? st))
  ) ;let
) ;let*

(display "=== 验收点 8: 全量 TUI 测试套件通过 ===\n")
(check (%run-ok? "tui-tests" "bin/gf test tools/pi/tests/liii/pi/modes/tui/" 300)
  =>
  0
) ;check

(check-report)
