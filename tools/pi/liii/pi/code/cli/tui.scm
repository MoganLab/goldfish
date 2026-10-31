;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; TUI 启动信息提取：页脚显示与 make-pi-app 初始化共用的配置解析
(define-library (liii pi code cli tui)
  (export pi-tui-footer-info)
  (import (scheme base)
    (scheme process-context)
    (liii pi code cli args)
    (liii pi code config)
    (liii pi code session-state)
    (liii pi ai models)
    (liii pi ai config record)
  ) ;import
  (begin

    ;; TUI footer 右下角显示实际生效的供应商与模型：与后台 worker 内的解析同源同输入
    ;; （纯本地文件读取，无网络开销），配置解析失败时回退到 CLI 原始参数；
    ;; 返回 (provider model-id thinking-level supports-thinking default-thinking-level
    ;;         config-dir models default-model hide-thinking model-thinking-levels)
    ;; default-thinking-level 为 settings.json 里的原始值（未配置时为 #f，
    ;; 由默认级别 medium 兜底）；model-thinking-levels 为 per-model 覆盖表
    ;; 供 make-pi-app 使用（models 为有凭证 provider 的模型列表，供 /model 选择器）
    (define (pi-tui-footer-info parsed)
      (catch #t
        (lambda ()
          (let* ((cfg (resolve-pi-config parsed (get-environment-variables)))
                 (session-state (pi-load-session-state parsed cfg))
                ) ;
            ;; 恢复会话时页脚与后台请求使用同一份合并结果（警告由 flow 在首轮请求前输出）
            (let-values (((cfg _warnings) (pi-resolve-session-state cfg session-state parsed)))
              (let* ((provider (cdr (or (assoc 'provider cfg) (cons #f "google"))))
                     (model-id (cdr (or (assoc 'model-id cfg) (cons #f "gemini-3.8-flash"))))
                     (tl (cdr (or (assoc 'thinking-level cfg) (cons #f "off"))))
                     (supports (cdr (or (assoc 'supports-thinking cfg) (cons #f #t))))
                     (settings (cdr (or (assoc 'settings cfg) (cons #f #f))))
                     (default-tl (if settings (pi-settings-default-thinking-level settings) #f))
                     (model-thinking-levels (if settings (pi-settings-model-thinking-levels settings) '())
                     ) ;model-thinking-levels
                     (config-dir (pi-cfg-config-dir cfg))
                     (models-store (cdr (or (assoc 'models-store cfg) (cons #f '()))))
                     (auth (pi-load-auth config-dir))
                     (models (pi-usable-models models-store auth))
                     (default-model
                       (and settings
                         (list (cons 'provider (pi-settings-default-provider settings))
                           (cons 'id (pi-settings-default-model settings))
                         ) ;list
                       ) ;and
                     ) ;default-model
                    ) ;
                (list provider
                  model-id
                  tl
                  (and supports #t)
                  default-tl
                  config-dir
                  models
                  default-model
                  ;; Ctrl+T 思考块隐藏态：从与上游共用的 settings.json 恢复
                  (and settings (pi-settings-hide-thinking-block settings) #t)
                  model-thinking-levels
                ) ;list
              ) ;let*
            ) ;let-values
          ) ;let*
        ) ;lambda
        (lambda args
          ;; 配置解析失败时的兜底：思考级别用默认级别 medium（对齐上游
          ;; DEFAULT_THINKING_LEVEL），而不是 off；default-tl 留 #f 表示未配置
          (list (or (pi-args-ref parsed 'provider) "google")
            (or (pi-args-ref parsed 'model) "gemini-3.8-flash")
            (or (pi-args-ref parsed 'thinking-level) "medium")
            #t
            #f
            (gf-get-config-dir)
            '()
            #f
            #f
            '()
          ) ;list
        ) ;lambda
      ) ;catch
    ) ;define

  ) ;begin
) ;define-library
