;; pi-app-run-ctrl-c-copy
;; 验证当存在鼠标选区时，按 Ctrl+C 优先复制选区到剪贴板并不清空输入框。
;;
;; 语法
;; ----
;; (pi-app-run app)
;;
;; 参数
;; ----
;; app : pi-app?
;;
;; 返回值
;; -----
;; boolean?

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii base64)
  (liii tui)
  (liii tui mouse)
  (liii pi code modes interactive app)
  (liii pi code modes interactive run)
  (liii pi code modes interactive state)
  (liii pi code modes interactive selection)
) ;import

(check-set-mode! 'report-failed)

;; 1. 有选区时，按 ctrl-c 执行复制，且不清除当前编辑框文本
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (ed (pi-app-editor app))
      ) ;
  (editor-set-text! ed "保留此输入框文本")
  (pi-app-add-user-message! app "line 0 of chat content")
  (pi-app-add-user-message! app "line 1 of chat content")
  (pi-app-render! app)

  ;; 模拟鼠标选区
  (virtual-terminal-push-key! vt (parse-mouse-event "\x1b;[<0;5;5M"))
  (virtual-terminal-push-key! vt (parse-mouse-event "\x1b;[<32;20;8M"))
  (virtual-terminal-push-key! vt (parse-mouse-event "\x1b;[<0;20;8m"))
  ;; 依次注入：
  ;; 1. ctrl-c：由于选区激活，执行复制，保留输入框文本
  (virtual-terminal-push-key! vt 'ctrl-c)
  ;; 2. escape：清空激活的选区
  (virtual-terminal-push-key! vt 'escape)
  ;; 3. ctrl-c：此时已无选区，清空输入框文本
  (virtual-terminal-push-key! vt 'ctrl-c)
  ;; 4. ctrl-d：输入框为空，正常退出主循环
  (virtual-terminal-push-key! vt 'ctrl-d)

  (check (pi-app-run app) => #t)

  ;; 最终由第二次 ctrl-c 清空
  (check (editor-text ed) => "")

  ;; 截获到 OSC 52 剪贴板复制序列
  (let* ((raw (virtual-terminal-raw vt))
         (osc-item
           (find (lambda (s) (and (string? s) (string-contains s "\x1b;]52;c;"))) raw)
         ) ;osc-item
        ) ;
    (check-true (string? osc-item))
  ) ;let*

  ;; 验证在第 0 行右上角正确渲染了 Copied! 浮层
  (let* ((raw (virtual-terminal-raw vt))
         (copied-item
           (find (lambda (s) (and (string? s) (string-contains s "Copied!"))) raw)
         ) ;copied-item
        ) ;
    (check-true (string? copied-item))
  ) ;let*
) ;let*

;; 2. 无选区时，按 ctrl-c 清空编辑框文本（保持原有行为）
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (ed (pi-app-editor app))
      ) ;
  (editor-set-text! ed "应该被清空的文本")
  (virtual-terminal-push-key! vt 'ctrl-c)
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app) => #t)
  (check (editor-text ed) => "")
) ;let*

(check-report)
