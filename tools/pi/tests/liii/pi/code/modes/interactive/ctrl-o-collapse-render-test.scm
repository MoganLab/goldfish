(import (scheme base)
  (liii check)
  (liii string)
  (liii list)
  (liii tui)
  (liii pi code modes interactive bash)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive status-card)
) ;import

(check-set-mode! 'report-failed)

;; 复现 bug：inline 模式下 expand→collapse 内容收缩时，diff 渲染不清除
;; 尾部残留行，旧内容（展开时的长输出）残留在屏幕上盖住后续布局，
;; 且光标定位漂移导致 shell prompt 画错位置。
(let* ((buf (open-output-string))
       (term (make-standard-terminal :output-port buf
               :detect-size-fn (lambda () '(80 24))
               :inline #t))
       (bash-exec (lambda (cmd)
                    (values
                      (string-join
                        (map (lambda (i) (string-append "line-" (number->string i)))
                          (iota 40))
                        "\n")
                      0)))
       (app (make-pi-app :terminal term :handler (lambda (a m) #t) :bash-handler bash-exec))
       (ed (pi-app-editor app))
       (screen (pi-app-screen app))
      ) ;
  (pi-app-render! app)
  (component-handle-input ed "!seq 1 40")
  (component-handle-input ed 'enter)
  (pi-app-render! app)
  ;; 第一次 Ctrl+O：展开（40 行全部渲染）
  (pi-app-toggle-tool-output! app)
  (pi-app-render! app)
  (let ((expanded-count (length (screen-lines screen))))
    (check-true (> expanded-count 40))
    ;; 第二次 Ctrl+O：折叠收缩
    (pi-app-toggle-tool-output! app)
    (pi-app-render! app)
    (let* ((out (get-output-string buf))
           (lines (screen-lines screen)))
      ;; 折叠后行数应显著少于展开态（尾部 20 行预览而非 40 行全文）
      (check-true (< (length lines) (- expanded-count 10)))
      (check (string-contains? out "Tool output: collapsed") => #t)
      ;; 光标必须落到折叠后内容底部之下，不能停在残留行
      (check-true (>= (terminal-max-y term) (- (length lines) 1)))
      ;; 超屏收缩触发清屏全量重绘：max-y 回落到新内容底部，
      ;; 不再残留在展开时的高度（旧 bug：59），退出时 prompt 位置正确
      (check-true (<= (terminal-max-y term) (+ (length lines) 2)))
    ) ;let*
  ) ;let
) ;let*

(check-report)
