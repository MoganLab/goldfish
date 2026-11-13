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
  (liii string)
  (liii path)
  (liii tui)
  (liii tempfile)
  (liii pi ai auth)
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

;; 5. 空配置目录下 ollama 默认未配置，写入 auth.json 后变为已配置
(let* ((tmp (make-temp-dir "gf-login-sel-ollama-" ""))
       (sel1 (make-login-selector (lambda (p) #f)
               (lambda () #f)
               :config-dir tmp
               :providers '("ollama")
             ) ;make-login-selector
       ) ;sel1
       (rendered1 (string-join (component-render sel1 80) "\n"))
      ) ;
  (check-true (string-contains? rendered1 "not configured"))
  (check-false (string-contains? rendered1 "configured ✓"))
  (let ((saved (pi-save-auth-key "ollama" "sk-ollama-1" tmp)))
    (let* ((sel2 (make-login-selector (lambda (p) #f)
                   (lambda () #f)
                   :config-dir tmp
                   :providers '("ollama")
                 ) ;make-login-selector
           ) ;sel2
           (rendered2 (string-join (component-render sel2 80) "\n"))
          ) ;
      (check-true (string-contains? rendered2 "configured"))
      (check-false (string-contains? rendered2 "not configured"))
    ) ;let*
    (path-unlink (path saved))
  ) ;let
  (path-rmdir tmp)
) ;let*

(check-report)
