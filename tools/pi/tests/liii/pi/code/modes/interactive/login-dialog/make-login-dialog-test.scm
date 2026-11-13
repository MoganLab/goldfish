;; make-login-dialog
;; 测试 API Key 输入对话框组件的创建、渲染、提交与取消。
;;
;; 语法
;; ----
;; (make-login-dialog provider on-submit on-cancel)
;;
;; 参数
;; ----
;; provider : string?
;; 选定的服务商标识。
;;
;; on-submit : procedure?
;; 提交 API Key 后的回调过程 (lambda (key) ...)。
;;
;; on-cancel : procedure?
;; 取消输入的回调过程 (lambda () ...)。
;;
;; 返回值
;; ----
;; component?
;; 登录凭证输入对话框组件实例。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive login-dialog)
) ;import

(check-set-mode! 'report-failed)

;; 1. 组件识别与初始光标
(let* ((submitted #f)
       (cancelled #f)
       (dialog (make-login-dialog "kimi-coding"
                 (lambda (key) (set! submitted key))
                 (lambda () (set! cancelled #t))
               ) ;make-login-dialog
       ) ;dialog
      ) ;
  (check-true (login-dialog? dialog))
  (check (login-dialog-provider dialog) => "kimi-coding")
  (check (login-dialog-cursor-position dialog) => '(6 2))

  ;; 2. 渲染输出包含供应商标题与提示
  (let ((lines (component-render dialog 80)))
    (check-true (pair? lines))
    (check-true (> (length lines) 6))
  ) ;let

  ;; 3. 按 Escape 触发取消回调
  (component-handle-input dialog 'escape)
  (check cancelled => #t)

  ;; 4. 输入文本并按回车提交
  (for-each (lambda (c) (component-handle-input dialog (string c)))
    (string->list "sk-test-key-123")
  ) ;for-each
  (component-handle-input dialog 'enter)
  (check submitted => "sk-test-key-123")
) ;let*

;; 5. 空文本回车不触发提交
(let* ((submitted #f)
       (dialog (make-login-dialog "deepseek"
                 (lambda (key) (set! submitted key))
                 (lambda () #f)
               ) ;make-login-dialog
       ) ;dialog
      ) ;
  (component-handle-input dialog 'enter)
  (check submitted => #f)
) ;let*

(check-report)
