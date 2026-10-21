(import (scheme base)
  (liii check)
  (liii unicode)
  (liii string)
  (liii tui)
  (liii pi tui app)
) ;import

(check-set-mode! 'report)

;; 1. 测试首屏组件布局与总行数
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm))
       (screen (pi-app-screen app))
      ) ;let*
  (pi-app-render! app)
  (let* ((lines (screen-lines screen))
         (plain-lines (map strip-ansi lines))
        ) ;let*
    ;; 初始应包含 Header(7行) + Editor(3行) + Footer(2行) = 12 行
    (check (length lines) => 12)
    ;; Header
    (check (list-ref plain-lines 0) => "")
    (check-true (string-contains? (list-ref plain-lines 1) " pi v"))
    ;; Editor 分隔线
    (check (list-ref plain-lines 7) => (utf8-make-string 100 #\─))
    (check (string-trim (list-ref plain-lines 8)) => "")
    (check (list-ref plain-lines 9) => (utf8-make-string 100 #\─))
    ;; Footer
    (check-true (string-contains? (list-ref plain-lines 11) "(auto)"))
  ) ;let*
) ;let*

;; 2. 测试 Echo 回显消息追加
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm))
       (screen (pi-app-screen app))
       (editor (pi-app-editor app))
      ) ;let*
  (pi-app-render! app)
  ;; 在 Editor 中输入字符
  (component-handle-input editor "ping")
  ;; 按回车触发提交
  (component-handle-input editor 'enter)
  (pi-app-render! app)
  (let ((lines (screen-lines screen)))
    ;; 追加了 2 行回显消息：> ping 和 Echo: ping
    (check (length lines) => 14)
    (check-true (string-contains? (list-ref lines 7) "> ping"))
    (check-true (string-contains? (list-ref lines 8) "Echo: ping"))
    ;; Editor 恢复为空
    (check (editor-text editor) => "")
  ) ;let
) ;let*

(check-report)
