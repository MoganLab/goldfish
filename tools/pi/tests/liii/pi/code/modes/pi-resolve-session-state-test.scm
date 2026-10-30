(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii shutil)
  (liii pi ai config record)
  (liii pi code cli args)
  (liii pi code config)
  (liii pi code session-state))

(check-set-mode! 'report-failed)

;; pi-resolve-session-state
;; 恢复会话时的投影状态合并：优先级为「CLI 显式参数 > 投影状态 > cfg」。
;;
;; 语法
;; ----
;; (pi-resolve-session-state cfg state parsed)
;;
;; 参数
;; ----
;; cfg : alist，resolve-pi-config 的结果
;; state : 投影状态 alist 或 #f，结构对齐 1201-09：
;;   ((thinkingLevel . 字符串|#f) (model . ((provider . str) (modelId . str)))|#f ...)
;; parsed : CLI 参数解析结果
;;
;; 返回值
;; ----
;; (values 合并后的 cfg 警告列表)

(define TMP-AGENT-DIR
  (path->string (path-join (path-temp-dir) "pi-resolve-session-state-test")))

;; 清理旧运行残留，保证测试可重复
(when (file-exists? TMP-AGENT-DIR)
  (rmtree TMP-AGENT-DIR))

(path-mkdir TMP-AGENT-DIR :parents #t :exist-ok #t)
(path-write-text (path->string (path-join (path TMP-AGENT-DIR) (path "auth.json")))
  "{\"google\":{\"type\":\"api_key\",\"key\":\"fixture-google-key\"},\"anthropic\":{\"type\":\"api_key\",\"key\":\"fixture-anthropic-key\"}}")

(define ENV (list (cons "PI_CODING_AGENT_DIR" TMP-AGENT-DIR)))

(define (base-cfg)
  (resolve-pi-config (parse-pi-args '()) ENV)
) ;define

(define (state-model provider model-id)
  (list (cons 'provider provider) (cons 'modelId model-id))
) ;define

;; ------------------------------------------------------------
;; 一、无显式参数：采用状态里的 provider/model/thinking
;; ------------------------------------------------------------
(let* ((cfg (base-cfg))
       (state (list (cons 'thinkingLevel "high")
                (cons 'model (state-model "anthropic" "claude-haiku-4-5"))
              ) ;list
       ) ;
      ) ;
  (let-values (((merged warnings) (pi-resolve-session-state cfg state (parse-pi-args '()))))
    (check (cdr (assoc 'provider merged)) => "anthropic")
    (check (cdr (assoc 'model-id merged)) => "claude-haiku-4-5")
    (check (cdr (assoc 'api-key merged)) => "fixture-anthropic-key")
    (check (cdr (assoc 'thinking-level merged)) => "high")
    (check warnings => '())
    ;; 原 cfg 不被破坏
    (check (cdr (assoc 'provider cfg)) => "google")
  ) ;let-values
) ;let*

;; ------------------------------------------------------------
;; 二、显式 --provider/--model 覆盖状态模型；显式 --thinking 覆盖状态等级
;; ------------------------------------------------------------
;; cfg 由同一份 parsed 解析而来，显式参数已体现在 cfg 中，状态不得再改写
(let* ((parsed (parse-pi-args '("--provider" "anthropic" "--model" "claude-haiku-4-5")))
       (cfg (resolve-pi-config parsed ENV))
       (state (list (cons 'thinkingLevel "high")
                (cons 'model (state-model "google" "gemini-2.5-pro"))
              ) ;list
       ) ;
      ) ;
  (let-values (((merged warnings) (pi-resolve-session-state cfg state parsed)))
    (check (cdr (assoc 'provider merged)) => "anthropic")
    (check (cdr (assoc 'model-id merged)) => "claude-haiku-4-5")
    ;; 模型被显式参数锁定，但 thinking 仍按状态应用
    (check (cdr (assoc 'thinking-level merged)) => "high")
    (check warnings => '())
  ) ;let-values
) ;let*

;; 只显式 --provider 也视为锁定模型维度（不混搭 provider/model）
(let* ((parsed (parse-pi-args '("--provider" "anthropic")))
       (cfg (resolve-pi-config parsed ENV))
       (state (list (cons 'model (state-model "google" "gemini-2.5-pro"))))
      ) ;
  (let-values (((merged warnings) (pi-resolve-session-state cfg state parsed)))
    (check (cdr (assoc 'provider merged)) => "anthropic")
    (check (cdr (assoc 'model-id merged)) => (cdr (assoc 'model-id cfg)))
    (check warnings => '())
  ) ;let-values
) ;let*

(let* ((parsed (parse-pi-args '("--thinking" "low")))
       (cfg (resolve-pi-config parsed ENV))
       (state (list (cons 'thinkingLevel "high")))
      ) ;
  (let-values (((merged warnings) (pi-resolve-session-state cfg state parsed)))
    (check (cdr (assoc 'thinking-level merged)) => "low")
    (check warnings => '())
  ) ;let-values
) ;let*

;; ------------------------------------------------------------
;; 三、状态缺失或字段缺失：cfg 原样返回且无警告
;; ------------------------------------------------------------
(let* ((cfg (base-cfg)))
  (let-values (((merged warnings) (pi-resolve-session-state cfg #f (parse-pi-args '()))))
    (check (equal? merged cfg) => #t)
    (check warnings => '())
  ) ;let-values
  ;; 只有 thinkingLevel，模型维度保持 cfg
  (let-values (((merged warnings)
                (pi-resolve-session-state cfg (list (cons 'thinkingLevel "medium")) (parse-pi-args '()))
               ) ;
              ) ;
    (check (cdr (assoc 'provider merged)) => "google")
    (check (cdr (assoc 'model-id merged)) => "gemini-3.8-flash")
    (check (cdr (assoc 'thinking-level merged)) => "medium")
    (check warnings => '())
  ) ;let-values
  ;; 只有 model，思考等级保持 cfg
  (let-values (((merged warnings)
                (pi-resolve-session-state cfg
                  (list (cons 'model (state-model "anthropic" "claude-haiku-4-5")))
                  (parse-pi-args '())
                ) ;pi-resolve-session-state
               ) ;
              ) ;
    (check (cdr (assoc 'provider merged)) => "anthropic")
    (check (cdr (assoc 'thinking-level merged)) => (cdr (assoc 'thinking-level cfg)))
    (check warnings => '())
  ) ;let-values
) ;let*

;; ------------------------------------------------------------
;; 四、无凭证 / 模型不可用：保留 cfg 并给出警告
;; ------------------------------------------------------------
(let* ((cfg (base-cfg))
       (state (list (cons 'model (state-model "openai" "gpt-5"))))
      ) ;
  (let-values (((merged warnings) (pi-resolve-session-state cfg state (parse-pi-args '()))))
    (check (cdr (assoc 'provider merged)) => "google")
    (check (cdr (assoc 'model-id merged)) => "gemini-3.8-flash")
    (check (length warnings) => 1)
  ) ;let-values
) ;let*

(let* ((cfg (base-cfg))
       (state (list (cons 'model (state-model "google" "no-such-model"))))
      ) ;
  (let-values (((merged warnings) (pi-resolve-session-state cfg state (parse-pi-args '()))))
    (check (cdr (assoc 'provider merged)) => "google")
    (check (cdr (assoc 'model-id merged)) => "gemini-3.8-flash")
    (check (length warnings) => 1)
  ) ;let-values
) ;let*

;; ------------------------------------------------------------
;; 五、思考等级非法或模型不支持思考：收敛为 off 并警告
;; ------------------------------------------------------------
(let* ((cfg (base-cfg))
       (state (list (cons 'thinkingLevel "ludicrous")))
      ) ;
  (let-values (((merged warnings) (pi-resolve-session-state cfg state (parse-pi-args '()))))
    (check (cdr (assoc 'thinking-level merged)) => "off")
    (check (length warnings) => 1)
  ) ;let-values
) ;let*

;; 构造一个 reasoning 为 #f 的模型条目，验证不支持思考时收敛为 off
(let* ((cfg (base-cfg))
       (plain-models-store
         (list (cons "google"
                 (list (cons "models"
                         (list (list (cons "id" "plain-1") (cons "reasoning" #f)))
                       ) ;cons
                 ) ;list
               ) ;cons
         ) ;list
       ) ;
       (cfg (cons (cons 'models-store plain-models-store) cfg))
       (state (list (cons 'thinkingLevel "high")
                (cons 'model (state-model "google" "plain-1"))
              ) ;list
       ) ;
      ) ;
  (let-values (((merged warnings) (pi-resolve-session-state cfg state (parse-pi-args '()))))
    (check (cdr (assoc 'model-id merged)) => "plain-1")
    (check (cdr (assoc 'supports-thinking merged)) => #f)
    (check (cdr (assoc 'thinking-level merged)) => "off")
    (check (length warnings) => 1)
  ) ;let-values
) ;let*

(rmtree TMP-AGENT-DIR)

(check-report)
