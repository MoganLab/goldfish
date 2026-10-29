(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii tui component)
  (liii pi code modes interactive status-card))

(check-set-mode! 'report-failed)

;; 组件谓词与基本渲染：dim 状态行 + 前置空行（对齐上游 showStatus 布局）
(let ((card (make-status-card "Tool output: expanded")))
  (check (component? card) => #t)
  (check (status-card? card) => #t)
  (let ((lines (component-render card 50)))
    (check (length lines) => 2)
    (check (car lines) => "")
    (check (string-contains? (cadr lines) "38;2;126;136;142m") => #t)
    (check (strip-ansi (cadr lines)) => " Tool output: expanded"))
  ;; 非组件对象不是状态卡片
  (check-false (status-card? "Tool output"))
  (check-false (status-card? (list "Tool output"))))

;; 消息原地更新：不换卡片、渲染内容随 set-message! 变化
(let ((card (make-status-card "Tool output: expanded")))
  (status-card-set-message! card "Tool output: collapsed")
  (let ((lines (component-render card 50)))
    (check (strip-ansi (cadr lines)) => " Tool output: collapsed"))
  ;; 更新后仍是同一张状态卡片
  (check (status-card? card) => #t))

;; 宽度 <= 0 时不渲染（对齐其他卡片组件的防御行为）
(check (component-render (make-status-card "x") 0) => '())

(check-report)
