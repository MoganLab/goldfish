(import (scheme base)
  (liii list)
  (liii string)
  (liii check)
  (liii tui)
  (liii tui component)
  (liii tui editor state)
  (liii pi agent queue)
  (liii pi code modes interactive app)
  (liii pi code modes interactive run)
) ;import

(check-set-mode! 'report)

;; Ctrl+D 的双重语义：
;;   输入框严格为空（且没有活动选择器）→ EOF 语义，退出主循环；
;;   输入框非空 → 前向删除光标右侧字符（与 Delete 键一致），
;;                删除后恰好变空也不退出，下一次独立 Ctrl+D 才退出。
;; 仅含空格或换行的缓冲区属于非空输入。
;;
;; 主循环退出时输入框已被清空，运行期间的中间状态无法在退出后直接观察，
;; 这里包装编辑器的 on-change 回调记录每次编辑后的文本与光标。

;; 记录编辑器每次 on-change 之后的文本与光标：
;; probes 为可选探针列表，每个探针接收 app 并返回附加记录值。
;; 返回读取器：调用后得到当前编辑日志（按发生顺序排列）。
(define (track-editor-changes app . probes)
  (let* ((ed (pi-app-editor app))
         (data (component-data ed))
         (orig (%ed-on-change data))
         (log '())
        ) ;
    (%ed-on-change-set! data
      (lambda (e t)
        (when (procedure? orig)
          (orig e t)
        ) ;when
        (set! log
          (cons
            (append (list t (editor-cursor e)) (map (lambda (probe) (probe app)) probes))
            log
          ) ;cons
        ) ;set!
      ) ;lambda
    ) ;%ed-on-change-set!
    (lambda () (reverse log))
  ) ;let*
) ;define

;; 1. Ctrl+D 非空输入：前向删除光标处字符（英文行首），不再被主循环吞掉
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (ed (pi-app-editor app))
      ) ;
  (editor-set-text! ed "abc")
  (editor-set-cursor! ed 0 0)
  (let ((changes (track-editor-changes app)))
    (virtual-terminal-push-key! vt 'ctrl-d)
    (virtual-terminal-push-key! vt 'ctrl-c)
    (virtual-terminal-push-key! vt 'ctrl-d)
    (check (pi-app-run app) => #t)
    ;; Ctrl+D 删除光标处字符 → "bc"，光标停在原位；Ctrl+C 清空输入框
    (check (changes) => '(("bc" (0 0)) ("" (0 0))))
  ) ;let
) ;let*

;; 2. Ctrl+D 非空输入：中文按完整字符删除（不得截断半个 UTF-8 字符）
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (ed (pi-app-editor app))
      ) ;
  (editor-set-text! ed "你好吗")
  (editor-set-cursor! ed 0 0)
  (let ((changes (track-editor-changes app)))
    (virtual-terminal-push-key! vt 'ctrl-d)
    (virtual-terminal-push-key! vt 'ctrl-c)
    (virtual-terminal-push-key! vt 'ctrl-d)
    (check (pi-app-run app) => #t)
    ;; 删除完整的 "你" → "好吗"，不得出现乱码或残留半个字符
    (check (changes) => '(("好吗" (0 0)) ("" (0 0))))
  ) ;let
) ;let*

;; 3. Ctrl+D 非空输入：中英文混合文本行内删除，每次只删光标右侧一个字符
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (ed (pi-app-editor app))
      ) ;
  (editor-set-text! ed "A你B好C")
  (editor-set-cursor! ed 0 2)
  (let ((changes (track-editor-changes app)))
    (virtual-terminal-push-key! vt 'ctrl-d)
    (virtual-terminal-push-key! vt 'ctrl-d)
    (virtual-terminal-push-key! vt 'ctrl-c)
    (virtual-terminal-push-key! vt 'ctrl-d)
    (check (pi-app-run app) => #t)
    ;; 先删 "B" → "A你好C"，光标仍在原位（此时位于 "好" 前），
    ;; 再删 "好" → "A你C"；光标始终停在原位，不删除光标左侧字符
    (check (changes) => '(("A你好C" (0 2)) ("A你C" (0 2)) ("" (0 0))))
  ) ;let
) ;let*

;; 4. Ctrl+D 位于缓冲区末尾：无可删除内容时为无操作（不退出、不报错）
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (ed (pi-app-editor app))
      ) ;
  (editor-set-text! ed "abc")
  (editor-set-cursor! ed 0 3)
  (let* ((changes (track-editor-changes app))
         (undo-len (length (%ed-undo-stack (component-data ed))))
        ) ;
    (virtual-terminal-push-key! vt 'ctrl-d)
    (virtual-terminal-push-key! vt 'ctrl-c)
    (virtual-terminal-push-key! vt 'ctrl-d)
    (check (pi-app-run app) => #t)
    ;; 无操作不得产生编辑（不新增撤销单元、不触发 on-change）
    (check (changes) => '(("" (0 0))))
    ;; 撤销栈只多出 Ctrl+C 清空这一次快照，无操作的 Ctrl+D 不得入栈
    (check (length (%ed-undo-stack (component-data ed))) => (+ undo-len 1))
  ) ;let*
) ;let*

;; 5. Ctrl+D 删除最后一个字符：本次只完成删除，不得当场退出；
;;    下一次独立 Ctrl+D 才按 EOF 语义退出
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (ed (pi-app-editor app))
      ) ;
  (editor-set-text! ed "x")
  (editor-set-cursor! ed 0 0)
  (let ((changes (track-editor-changes app)))
    (virtual-terminal-push-key! vt 'ctrl-d)
    ;; 主循环仍在运行：随后的 "z" 被插入到已清空的输入框
    (virtual-terminal-push-key! vt "z")
    (virtual-terminal-push-key! vt 'ctrl-c)
    (virtual-terminal-push-key! vt 'ctrl-d)
    (check (pi-app-run app) => #t)
    (check (changes) => '(("" (0 0)) ("z" (0 1)) ("" (0 0))))
  ) ;let
) ;let*

;; 6. 仅含空格的缓冲区属于非空输入：Ctrl+D 不得按空输入直接退出
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (ed (pi-app-editor app))
      ) ;
  (editor-set-text! ed " ")
  (editor-set-cursor! ed 0 0)
  (let ((changes (track-editor-changes app)))
    (virtual-terminal-push-key! vt 'ctrl-d)
    (virtual-terminal-push-key! vt "z")
    (virtual-terminal-push-key! vt 'ctrl-c)
    (virtual-terminal-push-key! vt 'ctrl-d)
    (check (pi-app-run app) => #t)
    (check (changes) => '(("" (0 0)) ("z" (0 1)) ("" (0 0))))
  ) ;let
) ;let*

;; 7. 仅含换行的缓冲区属于非空输入：Ctrl+D 不得按空输入直接退出
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (ed (pi-app-editor app))
      ) ;
  (editor-set-text! ed "\n")
  (editor-set-cursor! ed 0 0)
  (let ((changes (track-editor-changes app)))
    (virtual-terminal-push-key! vt 'ctrl-d)
    (virtual-terminal-push-key! vt "z")
    (virtual-terminal-push-key! vt 'ctrl-c)
    (virtual-terminal-push-key! vt 'ctrl-d)
    (check (pi-app-run app) => #t)
    (check (changes) => '(("" (0 0)) ("z" (0 1)) ("" (0 0))))
  ) ;let
) ;let*

;; 8. Ctrl+D 多行输入：行尾合并下一行，光标留在原行行尾
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (ed (pi-app-editor app))
      ) ;
  (editor-set-text! ed "abc\n中文")
  (editor-set-cursor! ed 0 3)
  (let ((changes (track-editor-changes app)))
    (virtual-terminal-push-key! vt 'ctrl-d)
    (virtual-terminal-push-key! vt 'ctrl-c)
    (virtual-terminal-push-key! vt 'ctrl-d)
    (check (pi-app-run app) => #t)
    ;; 两行合并为 "abc中文"，光标仍在第一行行尾
    (check (changes) => '(("abc中文" (0 3)) ("" (0 0))))
  ) ;let
) ;let*

;; 9. Ctrl+D 删除后可用 Ctrl+Z 撤销，文本与光标一并恢复
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (ed (pi-app-editor app))
      ) ;
  (editor-set-text! ed "abc")
  (editor-set-cursor! ed 0 0)
  (let ((changes (track-editor-changes app)))
    (virtual-terminal-push-key! vt 'ctrl-d)
    (virtual-terminal-push-key! vt 'ctrl-z)
    (virtual-terminal-push-key! vt 'ctrl-c)
    (virtual-terminal-push-key! vt 'ctrl-d)
    (check (pi-app-run app) => #t)
    (check (changes) => '(("bc" (0 0)) ("abc" (0 0)) ("" (0 0))))
  ) ;let
) ;let*

;; 10. Ctrl+D 在斜杠命令菜单激活时按 on-change 同步过滤菜单，
;;     不触发命令、不发送消息
(let* ((vt (make-virtual-terminal :width 100 :height 40))
       (received '())
       (app (make-pi-app :terminal vt
              :handler (lambda (a msg) (set! received (cons msg received)))))
       (ed (pi-app-editor app))
      ) ;
  (editor-set-text! ed "/mo")
  (editor-set-cursor! ed 0 2)
  (let ((changes (track-editor-changes app
                  (lambda (a)
                    (let ((menu (pi-app-slash-command-menu a)))
                      (if menu
                        (map (lambda (item) (cdr (assoc 'value item)))
                          (select-list-items menu)
                        ) ;map
                        #f
                      ) ;if
                    ) ;let
                  ) ;lambda
                )))
    (virtual-terminal-push-key! vt 'ctrl-d)
    (virtual-terminal-push-key! vt 'ctrl-c)
    (virtual-terminal-push-key! vt 'ctrl-d)
    (check (pi-app-run app) => #t)
    ;; 删除 "o" → "/m"，菜单按 on-change 同步过滤为含 "m" 的命令；
    ;; Ctrl+C 清空后菜单关闭
    (check (changes) => '(("/m" (0 2) ("model" "name" "resume")) ("" (0 0) #f)))
  ) ;let
  ;; 删除字符不得触发命令、不得发送消息
  (check received => '())
) ;let*

;; 11. 选择器激活时 Ctrl+D 仍由选择器处理，不得因主编辑器为空而退出
(let* ((vt (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t) :thinking-level "medium"))
       (ed (pi-app-editor app))
      ) ;
  (pi-app-submit! app "/thinking")
  (check-true (component? (pi-app-active-selector app)))
  (virtual-terminal-push-key! vt 'ctrl-d)
  (virtual-terminal-push-key! vt 'ctrl-c)
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app) => #t)
  (check (pi-app-active-selector app) => #f)
  ;; 所有按键均被消费：若 Ctrl+D 提前退出，后续按键会残留在队列中
  (check (virtual-terminal-pop-key! vt) => #f)
) ;let*

(check-report)
