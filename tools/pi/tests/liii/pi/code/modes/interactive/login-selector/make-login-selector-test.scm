;; make-login-selector
;; 测试登录供应商选择器组件的创建、渲染与输入分发。
;;
;; 语法
;; ----
;; (make-login-selector on-select on-cancel)
;; (make-login-selector on-select on-cancel :current provider :config-dir dir :providers list)
;;
;; 参数
;; ----
;; on-select : procedure?
;; 选择供应商后的回调过程 (lambda (provider-id) ...)。
;;
;; on-cancel : procedure?
;; 取消选择的回调过程 (lambda () ...)。
;;
;; 返回值
;; ----
;; component?
;; 供应商选择器组件实例。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii tempfile)
  (liii pi code modes interactive login-selector)
) ;import

(check-set-mode! 'report-failed)

;; 1. 组件识别与基本属性
(let* ((selected #f)
       (cancelled #f)
       (selector (make-login-selector (lambda (p) (set! selected p))
                   (lambda () (set! cancelled #t))
                 ) ;make-login-selector
       ) ;selector
      ) ;
  (check-true (login-selector? selector))
  (check (login-selector-cursor-position selector) => '(6 2))

  ;; 2. 渲染输出包含关键提示信息
  (let ((lines (component-render selector 80)))
    (check-true (pair? lines))
    (check-true (> (length lines) 6))
  ) ;let

  ;; 3. 按 Escape 触发取消回调
  (component-handle-input selector 'escape)
  (check cancelled => #t)
) ;let*

;; 4. 选择供应商触发回调
(let* ((selected #f)
       (selector (make-login-selector (lambda (p) (set! selected p))
                   (lambda () #f)
                   :providers '("deepseek" "kimi-coding")
                 ) ;make-login-selector
       ) ;selector
      ) ;
  (component-handle-input selector 'enter)
  (check selected => "deepseek")
) ;let*

(check-report)
