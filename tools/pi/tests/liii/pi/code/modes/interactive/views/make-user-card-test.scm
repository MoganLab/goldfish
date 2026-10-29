(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive views)
) ;import

(check-set-mode! 'report-failed)

;; 用户消息背景、前景、padding 与 spacer
(let* ((card (make-user-card "ls -la")) (w 40) (lines (component-render card w)))
  (check (component? card) => #t)
  (check (pair? lines) => #t)
  (check (length lines) => 4)
  (check (string-contains? (car lines) "48;2;33;59;73") => #t)
  (check-false (string-contains? (car lines) "ls -la"))
  (check (string-contains? (cadr lines) "48;2;33;59;73") => #t)
  (check (string-contains? (cadr lines) "38;2;222;224;225") => #t)
  (check (string-contains? (cadr lines) " ls -la") => #t)
  (check-false (string-contains? (cadr lines) ">"))
  (check (string-contains? (list-ref lines 2) "48;2;33;59;73") => #t)
  (check (list-ref lines 3) => "")
  ;; 内容与宽度不变时复用整个渲染结果，避免历史卡片逐帧重算。
  (check (eq? lines (component-render card w)) => #t)
  (let ((resized (component-render card 20)))
    (check-false (eq? lines resized))
    (check (eq? resized (component-render card 20)) => #t)
  ) ;let
) ;let*

(check-report)
