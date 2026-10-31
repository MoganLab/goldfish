;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;
;; 会话状态（model_change / thinking_level_change）的读取与合并：
;; - pi-load-session-state：启动恢复（--continue / --session-file / --resume）时
;;   从会话投影结果取状态；无恢复来源或 --no-session 时返回 #f
;; - pi-resolve-session-state：按「CLI 显式参数 > 投影状态 > cfg」合并到请求配置，
;;   返回合并后的 cfg 与警告列表；缺失或不可用时保留 cfg
(define-library (liii pi code session-state)
  (import (scheme base)
    (scheme file)
    (liii string)
    (liii list)
    (liii path)
    (liii pi code cli args)
    (liii pi code config)
    (liii pi ai models)
    (liii pi ai config record)
    (liii pi code core session-manager)
    (liii pi code core compaction session)
    (liii pi code core session-projection)
  ) ;import
  (export pi-load-session-state pi-resolve-session-state pi-session-state-of)
  (begin
    ;; 恢复会话时的投影状态合并（1201-12）：
    ;; 优先级为「CLI 显式参数 > 投影状态 > cfg」，state 结构对齐 1201-09：
    ;; (... (thinkingLevel . 字符串|#f) (model . ((provider . str) (modelId . str)))|#f ...)
    ;; 返回 (values 合并后的 cfg 警告列表)；警告只用于 stderr 提示，不影响退出码。

    (define (%non-empty-string? v)
      (and (string? v) (not (string-null? (string-trim-both v))))
    ) ;define
    ;; 字段访问兼容字符串键与符号键；非 alist 时返回 #f
    (define (%state-ref alist key)
      (let* ((str-key (if (symbol? key) (symbol->string key) key))
             (sym-key (if (string? key) (string->symbol key) key))
             (pair (and (pair? alist) (or (assoc str-key alist) (assoc sym-key alist))))
            ) ;
        (and pair (cdr pair))
      ) ;let*
    ) ;define

    (define (%cfg-ref cfg key)
      (cdr (or (assoc key cfg) (cons #f #f)))
    ) ;define
    ;; alist 覆盖：pairs 中的键优先，其余沿用原 cfg
    (define (%cfg-override cfg pairs)
      (let ((keys (map car pairs)))
        (append pairs (filter (lambda (p) (not (memq (car p) keys))) cfg))
      ) ;let
    ) ;define

    (define (%cfg-with-thinking-level cfg level)
      (%cfg-override cfg (list (cons 'thinking-level level)))
    ) ;define
    ;; 级别全集与 interactive 层 pi-thinking-levels 一致（flow 不能反向依赖 interactive）
    (define %projection-thinking-levels
      '("off" "minimal" "low" "medium" "high" "xhigh" "max")
    ) ;define
    ;; 应用状态里的模型：重新解析凭证与元数据；
    ;; 无凭证、模型不在目录或元数据缺失时保留 cfg 的模型并返回警告
    (define (%apply-state-model cfg provider model-id)
      (let* ((config-dir (pi-cfg-config-dir cfg))
             (models-store (or (%cfg-ref cfg 'models-store) '()))
             (auth (pi-load-auth config-dir))
             (api-key (pi-resolve-provider-key provider auth))
             (meta (pi-ensure-catalog-meta (pi-resolve-model-metadata provider model-id models-store)
                     provider
                     model-id
                     models-store
                   ) ;pi-ensure-catalog-meta
             ) ;meta
            ) ;
        (cond
         ((not (%non-empty-string? api-key))
          (values cfg
            (list (string-append "Session state model skipped: no credentials for provider \""
                    provider
                    "\"; using \""
                    (%cfg-ref cfg 'provider)
                    "/"
                    (%cfg-ref cfg 'model-id)
                    "\""
                  ) ;string-append
            ) ;list
          ) ;values
         ) ;
         ((not (and (pi-provider-has-model? provider model-id models-store)
                 (pi-model-metadata-matched? meta)
               ) ;and
          ) ;not
          (values cfg
            (list (string-append "Session state model skipped: \""
                    provider
                    "/"
                    model-id
                    "\" is unavailable; using \""
                    (%cfg-ref cfg 'provider)
                    "/"
                    (%cfg-ref cfg 'model-id)
                    "\""
                  ) ;string-append
            ) ;list
          ) ;values
         ) ;
         (else
           (values
             (%cfg-override cfg
               (list (cons 'provider provider)
                 (cons 'model-id model-id)
                 (cons 'model-meta meta)
                 (cons 'api-key api-key)
                 (cons 'supports-thinking
                   (pi-model-supports-thinking? models-store provider model-id)
                 ) ;cons
                 ;; provider 级代理随 provider 变化而重算
                 (cons 'proxy-url (pi-cfg-provider-proxy cfg provider))
               ) ;list
             ) ;%cfg-override
             '()
           ) ;values
         ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    ;; 应用状态里的思考等级：非法值或模型不支持思考时收敛为 off 并返回警告（off 恒合法）
    (define (%apply-state-thinking cfg state)
      (let ((level (%state-ref state 'thinkingLevel)))
        (cond ((not (%non-empty-string? level)) (values cfg '()))
              ((not (member level %projection-thinking-levels))
               (values (%cfg-with-thinking-level cfg "off")
                 (list (string-append "Session state thinking level \""
                         level
                         "\" is not recognized; using \"off\""
                       ) ;string-append
                 ) ;list
               ) ;values
              ) ;
              ((not (%cfg-ref cfg 'supports-thinking))
               (values (%cfg-with-thinking-level cfg "off")
                 (list (string-append "Session state thinking level \""
                         level
                         "\" is not supported by \""
                         (%cfg-ref cfg 'provider)
                         "/"
                         (%cfg-ref cfg 'model-id)
                         "\"; using \"off\""
                       ) ;string-append
                 ) ;list
               ) ;values
              ) ;
              (else (values (%cfg-with-thinking-level cfg level) '()))
        ) ;cond
      ) ;let
    ) ;define

    ;; 合并投影状态：CLI 显式 --provider / --model 时完全不应用状态模型（避免混搭），
    ;; 显式 --thinking 时不应用状态思考等级；state 为 #f 或字段缺失时 cfg 原样返回
    (define (pi-resolve-session-state cfg state parsed)
      (if (not (pair? state))
        (values cfg '())
        (let* ((cli-provider? (%non-empty-string? (pi-args-ref parsed 'provider)))
               (cli-model? (%non-empty-string? (pi-args-ref parsed 'model)))
               (state-model (%state-ref state 'model))
               (state-provider (%state-ref state-model 'provider))
               ;; 1201-09 用 modelId；兼容 TUI 状态条目写入的 model-id
               (state-model-id (or (%state-ref state-model 'modelId) (%state-ref state-model 'model-id))
               ) ;state-model-id
               (apply-model? (and (not cli-provider?)
                               (not cli-model?)
                               (or (%non-empty-string? state-provider) (%non-empty-string? state-model-id))
                             ) ;and
               ) ;apply-model?
              ) ;
          (let*-values (((cfg model-warnings)
                         (if apply-model?
                           (%apply-state-model cfg
                             (if (%non-empty-string? state-provider) state-provider (%cfg-ref cfg 'provider))
                             (if (%non-empty-string? state-model-id) state-model-id (%cfg-ref cfg 'model-id))
                           ) ;%apply-state-model
                           (values cfg '())
                         ) ;if
                        ) ;
                        ((cfg thinking-warnings)
                         (if (%non-empty-string? (pi-args-ref parsed 'thinking-level))
                           (values cfg '())
                           (%apply-state-thinking cfg state)
                         ) ;if
                        ) ;
                       ) ;
            (values cfg (append model-warnings thinking-warnings))
          ) ;let*-values
        ) ;let*
      ) ;if
    ) ;define

    ;; 从启动时恢复的会话文件读取投影状态：只处理 --continue / --session-file / --resume
    ;; 命中的会话；--no-session 或无恢复来源时返回 #f
    (define (pi-load-session-state parsed cfg)
      (if (pi-args-ref parsed 'no-session)
        #f
        (let* ((cwd (path->string (path-cwd)))
               (sdir (resolve-session-dir parsed cfg cwd))
               (sfile (resolve-resume-session-file parsed sdir))
              ) ;
          (if sfile
            (let-values (((msgs state diagnostics)
                          (pi-build-session-projection (pi-read-session-entries sfile))
                         ) ;
                        ) ;
              state
            ) ;let-values
            #f
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    ;; 按会话文件路径读取投影状态（/resume 切换到目标会话后回传其模型与思考等级）；
    ;; 状态缺失、文件不存在或读取失败时返回 #f，不影响会话切换本身
    (define (pi-session-state-of sfile)
      (if (and sfile (file-exists? sfile))
        (catch #t
          (lambda ()
            (let-values (((msgs state diagnostics)
                          (pi-build-session-projection (pi-read-session-entries sfile))
                         ) ;
                        ) ;
              state
            ) ;let-values
          ) ;lambda
          (lambda (tag . args) #f)
        ) ;catch
        #f
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
