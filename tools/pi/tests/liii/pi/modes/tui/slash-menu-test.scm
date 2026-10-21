(import (scheme base)
  (liii check)
  (liii error)
  (liii list)
  (liii string)
  (liii pi extension)
  (liii pi modes tui editor)
  (liii pi modes tui state)
  (liii pi modes tui loop)
  (liii pi modes tui layout)
  (liii pi modes tui slash-menu)
) ;import

(check-set-mode! 'report-failed)

;; ============================================================================
;; 1. Slash 命令数据模型与标准化
;; ============================================================================

(let ((cmd (make-pi-tui-slash-command "/test" "测试命令描述")))
  (check-true (pi-tui-slash-command? cmd))
  (check (slash-command-name cmd) => "/test")
  (check (slash-command-description cmd) => "测试命令描述")
  (check (slash-command-handler cmd) => #f)
) ;let

;; 参数校验
(check-catch 'type-error (make-pi-tui-slash-command 123 "desc"))
(check-catch 'type-error (make-pi-tui-slash-command "" "desc"))
(check-catch 'type-error (make-pi-tui-slash-command "/test" 456))
(check-catch 'type-error (make-pi-tui-slash-command "/test" "desc" "not-a-proc"))

;; normalize-slash-command
(let* ((def (make-extension-command-def "review" "审查" (lambda _ "ok")))
       (c1 (normalize-slash-command def))
      ) ;c1
  (check-true (pi-tui-slash-command? c1))
  (check (slash-command-name c1) => "/review")
  (check (slash-command-description c1) => "审查")
  (check-true (procedure? (slash-command-handler c1)))
) ;let*

(let* ((c2 (normalize-slash-command (cons "/exit" "退出会话"))))
  (check-true (pi-tui-slash-command? c2))
  (check (slash-command-name c2) => "/exit")
  (check (slash-command-description c2) => "退出会话")
) ;let*

;; ============================================================================
;; 2. 扩展命令收集与兜底内置命令
;; ============================================================================

;; 默认兜底内置命令
(let ((cmds (collect-slash-commands #f)))
  (check-true (list? cmds))
  (check-true (>= (length cmds) 3))
  (check-true (and (member "/help" (map slash-command-name cmds)) #t))
  (check-true (and (member "/exit" (map slash-command-name cmds)) #t))
  (check-true (and (member "/clear" (map slash-command-name cmds)) #t))
) ;let

;; 从 ExtensionRegistry 动态收集并去重规范化
(let* ((reg (make-extension-registry))
       (def1 (make-extension-command-def "tdd" "TDD 模式" (lambda _ #t)))
       (def2 (make-extension-command-def "/review" "代码评审" (lambda _ #t)))
       (def3 (make-extension-command-def "review" "代码评审别名" (lambda _ #t)))
      ) ;def3
  (register-command! reg def1)
  (register-command! reg def2)
  (register-command! reg def3)
  (let ((cmds (collect-slash-commands reg)))
    (check-true (and (member "/tdd" (map slash-command-name cmds)) #t))
    (check-true (and (member "/review" (map slash-command-name cmds)) #t))
    (check-true (and (member "/help" (map slash-command-name cmds)) #t))
    ;; 去重检查：/review 应该只有一个
    (let ((reviews (filter (lambda (c) (string=? (slash-command-name c) "/review")) cmds)))
      (check (length reviews) => 1)
    ) ;let
  ) ;let
) ;let*

(check-catch 'type-error (collect-slash-commands "invalid-registry"))
(check-catch 'type-error (collect-slash-commands 12345))

;; ============================================================================
;; 3. SlashMenu 构造与参数校验
;; ============================================================================

(let ((menu (make-pi-tui-slash-menu)))
  (check-true (pi-tui-slash-menu? menu))
  (check (slash-menu-visible? menu) => #f)
  (check (slash-menu-query menu) => "")
  (check (slash-menu-selected-index menu) => 0)
  (check-true (list? (slash-menu-commands menu)))
  (check-true (list? (slash-menu-candidates menu)))
) ;let

(check-catch 'type-error (make-pi-tui-slash-menu :visible? "yes"))
(check-catch 'type-error (make-pi-tui-slash-menu :query 123))
(check-catch 'type-error (make-pi-tui-slash-menu :selected-index -1))
(check-catch 'type-error (make-pi-tui-slash-menu :selected-index "0"))
(check-catch 'type-error (make-pi-tui-slash-menu :commands "not-list"))
(check-catch 'type-error (make-pi-tui-slash-menu :candidates 456))

;; ============================================================================
;; 4. 动态过滤算法（前缀与模糊过滤、大小写不敏感）
;; ============================================================================

(let* ((c-tdd (make-pi-tui-slash-command "/tdd" "测试驱动"))
       (c-rev (make-pi-tui-slash-command "/review" "评审"))
       (c-sim (make-pi-tui-slash-command "/simplify" "精简"))
       (c-hlp (make-pi-tui-slash-command "/help" "帮助"))
       (all-cmds (list c-tdd c-rev c-sim c-hlp))
      ) ;all-cmds
  ;; 空字符串与单一 / 均返回全部候选
  (check (length (filter-slash-commands all-cmds "")) => 4)
  (check (length (filter-slash-commands all-cmds "/")) => 4)

  ;; 前缀匹配
  (let ((res (filter-slash-commands all-cmds "/td")))
    (check (length res) => 1)
    (check (slash-command-name (car res)) => "/tdd")
  ) ;let
  ;; 不带前导 / 查询
  (let ((res (filter-slash-commands all-cmds "td")))
    (check (length res) => 1)
    (check (slash-command-name (car res)) => "/tdd")
  ) ;let
  ;; 大小写不敏感
  (let ((res (filter-slash-commands all-cmds "/SIM")))
    (check (length res) => 1)
    (check (slash-command-name (car res)) => "/simplify")
  ) ;let
  ;; 无匹配情况
  (let ((res (filter-slash-commands all-cmds "/notfound")))
    (check (null? res) => #t)
  ) ;let
) ;let*

;; ============================================================================
;; 5. 键盘导航（上下切换与循环）与输入感知同步
;; ============================================================================

(let* ((c1 (make-pi-tui-slash-command "/a" "A"))
       (c2 (make-pi-tui-slash-command "/b" "B"))
       (c3 (make-pi-tui-slash-command "/c" "C"))
       (menu (make-pi-tui-slash-menu :commands (list c1 c2 c3) :visible? #t))
      ) ;menu
  (check (slash-menu-selected-index menu) => 0)
  (check (slash-command-name (slash-menu-selected-command menu)) => "/a")

  ;; 下移
  (slash-menu-select-next! menu)
  (check (slash-menu-selected-index menu) => 1)
  (check (slash-command-name (slash-menu-selected-command menu)) => "/b")

  (slash-menu-select-next! menu)
  (check (slash-menu-selected-index menu) => 2)
  (check (slash-command-name (slash-menu-selected-command menu)) => "/c")

  ;; 循环回到首项
  (slash-menu-select-next! menu)
  (check (slash-menu-selected-index menu) => 0)

  ;; 上移循环到底部
  (slash-menu-select-prev! menu)
  (check (slash-menu-selected-index menu) => 2)

  ;; 输入感知同步
  ;; 输入 / 触发弹出
  (slash-menu-update-query! menu "/")
  (check-true (slash-menu-visible? menu))
  (check (length (slash-menu-candidates menu)) => 3)

  ;; 键入 /a 过滤
  (slash-menu-update-query! menu "/a")
  (check-true (slash-menu-visible? menu))
  (check (length (slash-menu-candidates menu)) => 1)
  (check (slash-menu-selected-index menu) => 0)

  ;; 键入非 / 开头自动隐藏
  (slash-menu-update-query! menu "hello")
  (check-false (slash-menu-visible? menu))

  ;; 键入空字符串自动隐藏
  (slash-menu-update-query! menu "")
  (check-false (slash-menu-visible? menu))

  ;; 键入空格（命令后输入参数）自动隐藏
  (slash-menu-update-query! menu "/a param")
  (check-false (slash-menu-visible? menu))
) ;let*

;; ============================================================================
;; 6. Tab / Enter 自动补全与 Esc 取消
;; ============================================================================

(let* ((c-tdd (make-pi-tui-slash-command "/tdd" "测试驱动开发"))
       (menu (make-pi-tui-slash-menu :commands (list c-tdd) :visible? #t))
       (ed (make-pi-tui-editor :text "/t"))
      ) ;ed
  (slash-menu-update-query! menu "/t")
  (check-true (slash-menu-visible? menu))

  ;; 一键补全：自动填充命令并在尾部追加空格，隐藏菜单
  (let ((completed? (slash-menu-complete! menu ed)))
    (check-true completed?)
    (check (pi-tui-editor-text ed) => "/tdd ")
    (check-false (slash-menu-visible? menu))
    ;; 光标位于追加空格后
    (check (pi-tui-editor-cursor ed) => '(0 5))
  ) ;let
) ;let*

(let* ((c-tdd (make-pi-tui-slash-command "/tdd" "测试驱动开发"))
       (menu (make-pi-tui-slash-menu :commands (list c-tdd) :visible? #t))
      ) ;menu
  ;; Esc 隐藏菜单
  (slash-menu-hide! menu)
  (check-false (slash-menu-visible? menu))
  (slash-menu-show! menu)
  (check-true (slash-menu-visible? menu))
) ;let*

;; ============================================================================
;; 7. Box + VStack 浮层渲染
;; ============================================================================

(let* ((c1 (make-pi-tui-slash-command "/tdd" "测试驱动开发"))
       (c2 (make-pi-tui-slash-command "/help" "显示帮助"))
       (menu (make-pi-tui-slash-menu :commands (list c1 c2) :visible? #t))
       (rendered (render-slash-menu menu 40))
      ) ;rendered
  (check-true (list? rendered))
  (check-true (>= (length rendered) 4))
  ;; 第一行包含单线 Box 顶边框及 Commands 标题
  (check-true (string? (car rendered)))
  (check-true (string-starts? (car rendered) "┌"))
  ;; 选中项包含 > 高亮标记
  (check-true (or (string-contains? (cadr rendered) ">")
                  (string-contains? (caddr rendered) ">")
              ) ;or
  ) ;check-true
  ;; 包含命令名
  (check-true (string-contains? (cadr rendered) "/tdd"))
  ;; 底边框
  (check-true (string-starts? (car (reverse rendered)) "└"))
) ;let*

;; 无匹配时渲染提示
(let* ((menu (make-pi-tui-slash-menu :commands '() :candidates '() :visible? #t))
       (rendered (render-slash-menu menu 30))
      ) ;rendered
  (check-true (list? rendered))
  (check-true (string-contains? (cadr rendered) "无匹配命令"))
) ;let*

;; ============================================================================
;; 8. State 与事件循环按键调度集成
;; ============================================================================

(let* ((st (make-pi-tui-state :input-editor (make-pi-tui-editor :text ""))))
  (check-true (pi-tui-slash-menu? (state-slash-menu st)))
  (check-false (state-slash-menu-visible? st))

  ;; 输入 / 并同步状态
  (pi-tui-editor-set-text! (state-input-editor st) "/")
  (state-sync-slash-menu! st)
  (check-true (state-slash-menu-visible? st))
  (check-true (state-dirty? st))

  ;; 键盘事件调度：Up / Down 改变选中项
  (state-clear-dirty! st)
  (let ((handled? (dispatch-slash-menu-key! st 'down)))
    (check-true handled?)
    (check (slash-menu-selected-index (state-slash-menu st)) => 1)
    (check-true (state-dirty? st))
  ) ;let

  ;; 键盘调度：Esc 仅关闭菜单而不退出程序
  (state-clear-dirty! st)
  (let ((handled? (dispatch-slash-menu-key! st 'escape)))
    (check-true handled?)
    (check-false (state-slash-menu-visible? st))
    (check (state-status st) => 'idle)
    (check (pi-tui-editor-text (state-input-editor st)) => "/")
  ) ;let
) ;let*
