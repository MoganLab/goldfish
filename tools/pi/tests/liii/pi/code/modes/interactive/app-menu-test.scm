(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive model-selector)
) ;import

(check-set-mode! 'report-failed)

;; 1. 输入 / 打开命令菜单，渲染在编辑器下方，光标仍留在编辑器。
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (a msg) #t)))
       (ed (pi-app-editor app))
      ) ;
  (component-handle-input ed "/")
  (check-true (component? (pi-app-slash-command-menu app)))
  (check
    (map (lambda (item) (cdr (assoc 'value item)))
      (select-list-items (pi-app-slash-command-menu app))
    ) ;map
    =>
    '("model" "thinking" "name" "session" "resume" "config")
  ) ;check
  (pi-app-render! app)
  (let ((plain (map strip-ansi (alt-screen-lines (pi-app-screen app)))))
    ;; 全屏帧固定为终端高度（40 行）：对话记录视口占上方，输入区与菜单贴在底部
    (check (length plain) => 40)
    ;; 命令菜单位于输入区下方、页脚上方（顺序即菜单顺序，行号随菜单项数变化）
    (let ((rows (list 32 33 34 35 36)))
      (check-true (string-contains? (list-ref plain (list-ref rows 0)) "/model"))
      (check-true (string-contains? (list-ref plain (list-ref rows 0)) "Select model"))
      (check-true (string-contains? (list-ref plain (list-ref rows 1)) "/thinking"))
      (check-true (string-contains? (list-ref plain (list-ref rows 1)) "Set thinking level"))
      (check-true (string-contains? (list-ref plain (list-ref rows 2)) "/name"))
      (check-true (string-contains? (list-ref plain (list-ref rows 3)) "/session"))
      (check-true (string-contains? (list-ref plain (list-ref rows 4)) "/resume"))
      (check-true (string-contains? (list-ref plain (list-ref rows 4)) "Select and resume a session")
      ) ;check-true
      ;; 菜单最多显示 5 项，第 6 项 /config 需要滚动（此处用计数行体现）
      (check-true (string-contains? (list-ref plain 37) "(1/6)"))
    ) ;let
    ;; 光标仍在输入行（菜单增高一行，输入区随之上移一行）
    (check (terminal-cursor vterm) => '(1 30))
  ) ;let
) ;let*

;; 2. 命令名 fuzzy 过滤；普通文本、行内斜杠和无匹配路径不打开菜单。
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (a msg) #t)))
       (ed (pi-app-editor app))
      ) ;
  (editor-set-text! ed "/th")
  (check
    (map (lambda (item) (cdr (assoc 'value item)))
      (select-list-items (pi-app-slash-command-menu app))
    ) ;map
    =>
    '("thinking")
  ) ;check
  (editor-set-text! ed "/mo")
  (check
    (map (lambda (item) (cdr (assoc 'value item)))
      (select-list-items (pi-app-slash-command-menu app))
    ) ;map
    =>
    '("model")
  ) ;check
  (editor-set-text! ed "hello /")
  (check (pi-app-slash-command-menu app) => #f)
  (editor-set-text! ed "/tmp")
  (check (pi-app-slash-command-menu app) => #f)
  (editor-set-text! ed "/zzz")
  (editor-set-cursor! ed 0 4)
  (check (pi-app-slash-command-menu app) => #f)
  (component-handle-input ed 'backspace)
  (component-handle-input ed 'backspace)
  (component-handle-input ed 'backspace)
  (check (editor-text ed) => "/")
  (check-true (component? (pi-app-slash-command-menu app)))
) ;let*

;; 3. 方向键选择，Tab 仅补全并关闭菜单，Escape 关闭菜单但保留输入。
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vterm :handler (lambda (a msg) #t)))
       (ed (pi-app-editor app))
      ) ;
  (component-handle-input ed "/")
  (pi-app-handle-input! app 'down)
  (pi-app-handle-input! app 'down)
  (pi-app-handle-input! app 'tab)
  (check (editor-text ed) => "/name ")
  (check (editor-cursor ed) => '(0 6))
  (check (pi-app-slash-command-menu app) => #f)
  (editor-set-text! ed "/")
  (pi-app-handle-input! app 'escape)
  (check (pi-app-slash-command-menu app) => #f)
  (check (editor-text ed) => "/")
) ;let*

;; 4. Enter 立即执行当前命令；默认第一项 /model 打开模型选择器。
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (app
         (make-pi-app
           :terminal vterm
           :handler  (lambda (a msg) #t)
           :models
           (list (list (cons 'provider "google")
                   (cons 'id "gemini-2.5-pro")
                   (cons 'name "Gemini 2.5 Pro")
                   (cons 'reasoning #t)
                 ) ;list
           ) ;list
         ) ;make-pi-app
       ) ;app
       (ed (pi-app-editor app))
      ) ;
  (component-handle-input ed "/")
  (pi-app-handle-input! app 'enter)
  (check-true (model-selector? (pi-app-active-selector app)))
  (check (pi-app-slash-command-menu app) => #f)
  (check (editor-text ed) => "")
) ;let*

(check-report)
