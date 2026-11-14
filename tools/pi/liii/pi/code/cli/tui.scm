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

    ;; 当前模型的上下文窗口：页脚左下角上下文占比的容量分母
    (define (%pi-tui-context-window cfg)
      (let ((meta (pi-cfg-ref cfg 'model-meta)))
        (if (pi-model-metadata? meta) (pi-model-metadata-context-window meta) #f)
      ) ;let
    ) ;define

    ;; 从已合并的会话配置中提取页脚展示字段
    ;; 返回 (provider model-id thinking-level supports-thinking default-thinking-level
    ;;         config-dir models default-model hide-thinking model-thinking-levels
    ;;         context-window)
    ;; default-thinking-level 为 settings.json 里的原始值（未配置时为 #f，
    ;; 由默认级别 medium 兜底）；model-thinking-levels 为 per-model 覆盖表
    ;; 供 make-pi-app 使用（models 为已激活模型列表，供 /model 选择器）
    (define (%pi-tui-footer-fields cfg)
      (let* ((provider (pi-cfg-ref cfg 'provider "google"))
             (model-id (pi-cfg-ref cfg 'model-id "gemini-3.8-flash"))
             (tl (pi-cfg-ref cfg 'thinking-level "off"))
             (supports (pi-cfg-ref cfg 'supports-thinking #t))
             (settings (pi-cfg-ref cfg 'settings))
             (default-tl (if settings (pi-settings-default-thinking-level settings) #f))
             (model-thinking-levels (if settings (pi-settings-model-thinking-levels settings) '())
             ) ;model-thinking-levels
             (config-dir (pi-cfg-config-dir cfg))
             (models-store (pi-cfg-ref cfg 'models-store '()))
             (models (pi-usable-models models-store))
             (default-model
               (and settings
                 (list (cons 'provider (pi-settings-default-provider settings))
                   (cons 'id (pi-settings-default-model settings))
                 ) ;list
               ) ;and
             ) ;default-model
             (context-window (%pi-tui-context-window cfg))
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
          context-window
          ;; Agent Skills：/skill: 命令开关（默认开启）
          (if settings (pi-settings-enable-skill-commands settings) #t)
          (and (pi-cfg-ref cfg 'thinking-level-explicit?) #t)
        ) ;list
      ) ;let*
    ) ;define

    ;; TUI footer 右下角显示实际生效的供应商与模型：与后台 worker 内的解析同源同输入
    ;; （纯本地文件读取，无网络开销），配置解析失败时回退到 CLI 原始参数
    (define (pi-tui-footer-info parsed)
      (catch #t
        (lambda ()
          (let* ((cfg (resolve-pi-config parsed (get-environment-variables)))
                 (session-state (pi-load-session-state parsed cfg))
                ) ;
            ;; 恢复会话时页脚与后台请求使用同一份合并结果（警告由 flow 在首轮请求前输出）
            (let-values (((cfg _warnings) (pi-resolve-session-state cfg session-state parsed)))
              (%pi-tui-footer-fields cfg)
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
            #f
            #t
            (and (string? (pi-args-ref parsed 'thinking-level))
              (not (string-null? (string-trim-both (pi-args-ref parsed 'thinking-level))))
            ) ;and
          ) ;list
        ) ;lambda
      ) ;catch
    ) ;define

  ) ;begin
) ;define-library
