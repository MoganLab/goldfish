;; pi-app-run-paste
;; Ctrl+V 从外部系统剪贴板读取文本，并复用编辑器的粘贴处理流程。
;;
;; 语法
;; ----
;; (pi-app-run app [initial-msg] [rest-msgs] [force-exit] [clipboard-reader])
;;
;; 参数
;; ----
;; clipboard-reader : procedure（可选）
;; 无参调用并返回外部剪贴板文本或 #f；本测试注入模拟读取器。
;;
;; 返回值
;; ----
;; any
;; 离开 TUI 主循环时返回 #t；其他参数及返回值详见 pi-app-run。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive run)
) ;import

(check-set-mode! 'report-failed)

;; 1. 在当前光标位置插入外部剪贴板文本。
;;    注入读取器，避免测试访问开发机的真实剪贴板。
(let* ((submitted #f)
       (reads 0)
       (vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) (set! submitted m))))
       (ed (pi-app-editor app))
      ) ;
  (component-handle-input ed "ad")
  (component-handle-input ed 'left)
  (virtual-terminal-push-key! vt 'ctrl-v)
  (virtual-terminal-push-key! vt 'enter)
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check
    (pi-app-run app "" '() (lambda () #f) (lambda () (set! reads (+ reads 1)) "bc"))
    =>
    #t
  ) ;check
  (check reads => 1)
  (check submitted => "abcd")
) ;let*

;; 2. 多行外部文本沿用 bracketed paste 的换行归一化规则。
(let* ((submitted #f)
       (vt (make-virtual-terminal :width 100 :height 40))
       (app (make-pi-app :terminal vt :handler (lambda (a m) (set! submitted m))))
      ) ;
  (virtual-terminal-push-key! vt 'ctrl-v)
  (virtual-terminal-push-key! vt 'enter)
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app
           ""
           '()
           (lambda () #f)
           (lambda () "外部第一行\r\n外部第二行")
         ) ;pi-app-run
    =>
    #t
  ) ;check
  (check submitted => "外部第一行\n外部第二行")
) ;let*

;; 3. 剪贴板为空或读取失败时保持原输入，并允许正常提交。
(for-each
  (lambda (clipboard-text)
    (let* ((submitted #f)
           (vt (make-virtual-terminal :width 80 :height 24))
           (app (make-pi-app :terminal vt :handler (lambda (a m) (set! submitted m))))
          ) ;
      (virtual-terminal-push-key! vt "keep")
      (virtual-terminal-push-key! vt 'ctrl-v)
      (virtual-terminal-push-key! vt 'enter)
      (virtual-terminal-push-key! vt 'ctrl-d)
      (check (pi-app-run app "" '() (lambda () #f) (lambda () clipboard-text)) => #t)
      (check submitted => "keep")
    ) ;let*
  ) ;lambda
  (list "" #f)
) ;for-each

;; 4. 当外部剪贴板包含图片时，Ctrl+V 优先将图片路径插入输入框
(let* ((submitted #f)
       (image-reads 0)
       (vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) (set! submitted m))))
      ) ;
  (virtual-terminal-push-key! vt 'ctrl-v)
  (virtual-terminal-push-key! vt 'enter)
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check
    (pi-app-run app
      ""
      '()
      (lambda () #f)
      (lambda () "text-fallback")
      (lambda () (set! image-reads (+ image-reads 1)) "/tmp/pi-clipboard-123.png")
    ) ;pi-app-run
    =>
    #t
  ) ;check
  (check image-reads => 1)
  (check submitted => "/tmp/pi-clipboard-123.png")
) ;let*

(check-report)
