;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; /config：在 `gf pi` 交互界面里就地打开 `gf pi config`（三列配置 TUI）。
;; 复用同一个终端与备用屏：config 以 nested 模式运行，退出后清屏并把控制权交回，
;; 由外层重新整屏渲染（避免两套 screen 缓冲的 diff 残影）。
(define-library (liii pi code modes interactive app-config)
  (export pi-app-config-command? pi-app-submit-config!)
  (import (scheme base)
    (liii string)
    (liii path)
    (liii go)
    (liii tui)
    (liii pi code config)
    (liii pi code modes config app)
    (liii pi code modes interactive state)
    (liii pi code modes interactive render)
    (liii pi code modes interactive views)
    (liii pi code modes interactive app-model)
  ) ;import
  (begin

    (define (pi-app-config-command? text)
      (string=? text "/config")
    ) ;define

    (define (pi-app-submit-config! app)
      (editor-set-text! (pi-app-editor app) "")
      (let* ((config-dir (gf-get-config-dir))
             (config-dir-str (path->string config-dir))
             (config-ok? #f)
            ) ;
        (catch #t
          (lambda ()
            (pi-config-run config-dir-str :terminal (pi-app-term app) :nested #t)
            (set! config-ok? #t)
          ) ;lambda
          (lambda (tag . info)
            (container-add! (%pi-app-chat app)
              (make-tool-card "config"
                '()
                'error
                (string-append "Failed to open config: "
                  (if (pair? info) (object->string (car info)) "")
                ) ;string-append
              ) ;make-tool-card
            ) ;container-add!
          ) ;lambda
        ) ;catch
        (when config-ok?
          (catch #t
            (lambda ()
              (let-values (((models default-model) (pi-app-model-catalog config-dir)))
                (%pi-app-models-set! app models)
                (%pi-app-default-model-set! app default-model)
                (when (pi-app-jobs-chan app)
                  (catch #t
                    (lambda ()
                      (chan-send! (pi-app-jobs-chan app) (list 'reload_models config-dir-str) 1000)
                    ) ;lambda
                    (lambda args #f)
                  ) ;catch
                ) ;when
              ) ;let-values
            ) ;lambda
            (lambda (tag . info)
              (container-add! (%pi-app-chat app)
                (make-tool-card "config"
                  '()
                  'error
                  (string-append "Failed to refresh models: "
                    (if (pair? info) (object->string (car info)) "")
                  ) ;string-append
                ) ;make-tool-card
              ) ;container-add!
            ) ;lambda
          ) ;catch
        ) ;when
        ;; 返回外层：整屏失效后重画，清掉 config 留下的内容
        (catch #t
          (lambda () (alt-screen-invalidate! (pi-app-screen app)))
          (lambda args #f)
        ) ;catch
        (pi-app-render! app)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
