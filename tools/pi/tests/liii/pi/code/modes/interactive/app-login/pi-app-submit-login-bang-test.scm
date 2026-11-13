;; pi-app-submit-login!
;; 处理 /login 命令的提交，挂载选择器或对话框，或直接保存 Key。
;;
;; 语法
;; ----
;; (pi-app-submit-login! app text)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; text : string?
;; 用户输入的命令文本（如 "/login"）。
;;
;; 返回值
;; ----
;; any
;; 执行界面切换或持久化副作用。

(import (scheme base)
  (liii check)
  (liii path)
  (liii tempfile)
  (liii tui)
  (liii pi ai auth)
  (liii pi code modes interactive state)
  (liii pi code modes interactive app)
  (liii pi code modes interactive login-selector)
  (liii pi code modes interactive login-dialog)
  (liii pi code modes interactive app-login)
) ;import

(check-set-mode! 'report-failed)

;; 构造用于单元测试的最小 mock app
(define (make-test-app tmp-dir)
  (let ((term (make-virtual-terminal :width 80 :height 24)))
    (make-pi-app
      :terminal term
      :provider "deepseek"
      :model "deepseek-chat"
    ) ;make-pi-app
  ) ;let
) ;define

;; 1. /login 唤起供应商选择器
(let* ((tmp (make-temp-dir "gf-login-test-" ""))
       (app (make-test-app tmp))
      ) ;
  (pi-app-submit-login! app "/login")
  (check-true (login-selector? (pi-app-active-selector app)))

  ;; Esc 取消选择器
  (component-handle-input (pi-app-active-selector app) 'escape)
  (check-false (pi-app-active-selector app))

  (path-rmdir tmp)
) ;let*

;; 2. /login <provider> 直接唤起该供应商的 Key 对话框
(let* ((tmp (make-temp-dir "gf-login-test-" ""))
       (app (make-test-app tmp))
      ) ;
  (pi-app-submit-login! app "/login kimi-coding")
  (let ((selector (pi-app-active-selector app)))
    (check-true (login-dialog? selector))
    (check (login-dialog-provider selector) => "kimi-coding")
    ;; Esc 取消对话框
    (component-handle-input selector 'escape)
    (check-false (pi-app-active-selector app))
  ) ;let

  (path-rmdir tmp)
) ;let*

;; 3. /login <provider> <key> 直接写入 auth.json
(let* ((tmp (make-temp-dir "gf-login-test-" ""))
       (app (make-test-app tmp))
       (auth-target (path->string (path-join tmp "auth.json")))
      ) ;
  (pi-save-auth-key "zai-coding-cn" "sk-zai-123" tmp)
  (let ((creds (pi-load-auth tmp)))
    (check (length creds) => 1)
    (check (pi-resolve-provider-key "zai-coding-cn" creds '()) => "sk-zai-123")
  ) ;let
  (path-unlink (path auth-target))
  (path-rmdir tmp)
) ;let*

(check-report)
