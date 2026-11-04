;; pi-app-submit-config!
;; 处理 /config 命令，在交互界面中嵌套启动配置 TUI 并刷新界面。
;;
;; 语法
;; ----
;; (pi-app-submit-config! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (srfi srfi-1)
  (liii check)
  (liii os)
  (liii path)
  (liii sys)
  (liii go)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive app-config)
) ;import

(check-set-mode! 'report-failed)

(define FIXTURE-DIR
  (path->string
    (path-absolute (path (if (file-exists? "tests/fixtures/user-config/config-tui")
                           "tests/fixtures/user-config/config-tui"
                           "tools/pi/tests/fixtures/user-config/config-tui"
                         ) ;if
                   ) ;path
    ) ;path-absolute
  ) ;path->string
) ;define

(define (with-config-dir dir thunk)
  (let ((saved (getenv "GF_CONFIG_DIR")) (saved-pi (getenv "PI_CODING_AGENT_DIR")))
    (putenv "GF_CONFIG_DIR" dir)
    (unsetenv "PI_CODING_AGENT_DIR")
    (let ((result (thunk)))
      (if saved (putenv "GF_CONFIG_DIR" saved) (unsetenv "GF_CONFIG_DIR"))
      (when saved-pi
        (putenv "PI_CODING_AGENT_DIR" saved-pi)
      ) ;when
      result
    ) ;let
  ) ;let
) ;define

;; 1. 执行 pi-app-submit-config! 会清空编辑器内容，并在按 q 退出配置后恢复渲染
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm :handler (lambda (a msg) #t)))
      ) ;
  (virtual-terminal-push-key! vterm 'q)
  (editor-set-text! (pi-app-editor app) "/config")
  (check (editor-text (pi-app-editor app)) => "/config")
  (pi-app-submit-config! app)
  (check (editor-text (pi-app-editor app)) => "")
) ;let*

;; 2. /config 退出后刷新 app 的 models 与 default-model 字段，并向 jobs-chan 投递 reload_models
(with-config-dir FIXTURE-DIR
  (lambda ()
    (let* ((vterm (make-virtual-terminal :width 80 :height 24))
           (dummy-models '(((provider . "dummy") (id . "d1"))))
           (dummy-dm '((provider . "dummy") (id . "d1")))
           (app (make-pi-app
                  :terminal      vterm
                  :handler       (lambda (a msg) #t)
                  :models        dummy-models
                  :default-model dummy-dm
                ) ;make-pi-app
           ) ;app
          ) ;
      (virtual-terminal-push-key! vterm 'q)
      (pi-app-submit-config! app)
      ;; 验证 models 已刷新为 fixture 目录下的模型列表（包含 kimi-coding / custom-kimi 等）
      (let ((refreshed-models (pi-app-models app)))
        (check-true (pair? refreshed-models))
        (check-false (equal? refreshed-models dummy-models))
        (let ((providers
                (delete-duplicates
                  (map (lambda (m) (cdr (assoc 'provider m))) refreshed-models)
                ) ;delete-duplicates
              ) ;providers
             ) ;
          (check providers => '("custom-kimi" "google" "kimi-coding"))
        ) ;let
      ) ;let
      ;; 验证 default-model 已刷新为 settings.json 显式指定的 kimi-coding / k3.1-pro
      (check (%pi-app-default-model app)
        =>
        '((provider . "kimi-coding") (id . "k3.1-pro"))
      ) ;check
      ;; 验证 jobs-chan 收到了 reload_models 任务
      (let ((job (chan-try-recv! (pi-app-jobs-chan app) #f)))
        (check-true (pair? job))
        (check (car job) => 'reload_models)
        (check (cadr job) => FIXTURE-DIR)
      ) ;let
    ) ;let*
  ) ;lambda
) ;with-config-dir

(check-report)
