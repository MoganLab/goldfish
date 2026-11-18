;; pi-resolve-session-state
;; 将恢复的投影状态合并进 Agent 配置。
;;
;; 语法
;; ----
;; (pi-resolve-session-state cfg state parsed)
;;
;; 参数
;; ----
;; cfg : list?
;; Agent 配置 alist。
;;
;; state : (or alist #f)
;; 恢复的投影状态。
;;
;; parsed : list?
;; 命令行解析后的参数 alist。
;;
;; 返回值
;; ----
;; 两个值
;; 合并后的配置与警告文本列表。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii list)
  (liii os)
  (liii path)
  (liii shutil)
  (liii pi code session-state)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "pi-session-state-test-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(path-mkdir (path TMP-DIR) :parents #t :exist-ok #t)
(path-write-text (path->string (path-join TMP-DIR "auth.json"))
  "{\"google\":{\"type\":\"api_key\",\"key\":\"fixture-key\"}}"
) ;path-write-text

(define GOOGLE-STORE
  (list (cons "google"
          (list (cons "models"
                  (list (list (cons "id" "gemini-3.8-flash")
                              (cons "name" "Gemini 3.8 Flash")
                              (cons "contextWindow" 1000000)
                              (cons "maxTokens" 65536)
                              (cons "reasoning" #t)
                            ) ;list
                  ) ;list
                ) ;cons
          ) ;list
    ) ;cons
  ) ;list
) ;define

(define AIO-CFG
  (list (cons 'provider "all-in-one")
    (cons 'model-id "coding-fast")
    (cons 'config-dir (path TMP-DIR))
    (cons 'models-store GOOGLE-STORE)
  ) ;list
) ;define

(define PHYSICAL-STATE
  (list (cons 'model
               (list (cons 'provider "google")
                 (cons 'modelId "gemini-3.8-flash")
               ) ;list
         )
    (cons 'modelSource "assistant")
  ) ;list
) ;define

;; 1. 状态缺失时原样返回配置
(let-values (((cfg warnings) (pi-resolve-session-state '((provider . "kimi")) #f '())))
  (check cfg => '((provider . "kimi")))
  (check warnings => '())
) ;let-values

;; 2. 恢复元供应商模型不检查自身凭据，实际凭据由选中节点解析
(let ((state (list (cons 'model
                     (list (cons 'provider "all-in-one") (cons 'modelId "coding-deep"))
                   ) ;cons
             ) ;list
      ) ;state
     ) ;
  (let-values (((cfg warnings)
                (pi-resolve-session-state
                  (list (cons 'provider "all-in-one")
                    (cons 'model-id "coding-deep")
                    (cons 'config-dir (path "/tmp/nonexistent-aio-session-state"))
                  ) ;list
                  state
                  '()
                ) ;pi-resolve-session-state
               ) ;
              ) ;
    (check (cdr (assoc 'provider cfg)) => "all-in-one")
    (check (cdr (assoc 'model-id cfg)) => "coding-deep")
    (check warnings => '())
  ) ;let-values
) ;let

;; 3. 恢复用户主动设置的 thinking_level_change：保留显式覆盖标记，
;;    自动 clamp 不凭空生成用户意图（CLI 显式 --thinking 优先，不应用状态值）
(let* ((state (list (cons 'thinkingLevel "high")))
       (base (list (cons 'provider "stub")
               (cons 'model-id "one")
               (cons 'models-store '())
               (cons 'thinking-level "medium")
               (cons 'thinking-level-explicit? #f)
             ) ;list
       ) ;base
      ) ;
  (let-values (((cfg warnings) (pi-resolve-session-state base state '())))
    (check (cdr (assoc 'thinking-level cfg)) => "high")
    (check (cdr (assoc 'thinking-level-explicit? cfg)) => #t)
  ) ;let-values
  ;; CLI 显式 --thinking 优先于会话状态，状态值不参与
  (let-values (((cfg warnings)
                (pi-resolve-session-state base state (list (cons 'thinking-level "low")))
               ) ;
              ) ;
    (check (cdr (assoc 'thinking-level cfg)) => "medium")
  ) ;let-values
) ;let*

;; 4. all-in-one 伪模型会话：assistant 消息里记录的是实际服务节点，
;;    不得用它覆盖伪模型，否则链式调度被绕开，节点 429 重试耗尽后不再切换节点
(let-values (((cfg warnings)
              (pi-resolve-session-state AIO-CFG PHYSICAL-STATE '())
             ) ;
            ) ;
  (check (cdr (assoc 'provider cfg)) => "all-in-one")
  (check (cdr (assoc 'model-id cfg)) => "coding-fast")
  (check warnings => '())
) ;let-values

;; 5. all-in-one 会话中用户显式 model_change 到物理节点时，仍然照常切走
(let-values (((cfg warnings)
              (pi-resolve-session-state
                AIO-CFG
                (list (cons 'model
                             (list (cons 'provider "google")
                               (cons 'modelId "gemini-3.8-flash")
                             ) ;list
                       )
                  (cons 'modelSource "model_change")
                ) ;list
                '()
              ) ;pi-resolve-session-state
             ) ;
            ) ;
  (check (cdr (assoc 'provider cfg)) => "google")
  (check (cdr (assoc 'model-id cfg)) => "gemini-3.8-flash")
  (check warnings => '())
) ;let-values

;; 6. 用户先显式切到物理节点、之后的 assistant 消息记录同一节点：
;;    用户意图已不是伪模型，因此按用户意图照常应用物理节点
(let-values (((cfg warnings)
              (pi-resolve-session-state
                AIO-CFG
                (list (cons 'model
                       (list (cons 'provider "google")
                         (cons 'modelId "gemini-3.8-flash")
                       )
                     )
                  (cons 'modelSource "assistant")
                  (cons 'modelIntent
                    (list (cons 'provider "google")
                      (cons 'modelId "gemini-3.8-flash")
                    )
                  )
                ) ;list
                '()
              ) ;pi-resolve-session-state
             ) ;
            ) ;
  (check (cdr (assoc 'provider cfg)) => "google")
  (check (cdr (assoc 'model-id cfg)) => "gemini-3.8-flash")
  (check warnings => '())
) ;let-values

;; 7. 用户最后一次显式切回 all-in-one 伪模型：assistant 来源的物理节点不得覆盖
(let-values (((cfg warnings)
              (pi-resolve-session-state
                AIO-CFG
                (list (cons 'model
                       (list (cons 'provider "google")
                         (cons 'modelId "gemini-3.8-flash")
                       )
                     )
                  (cons 'modelSource "assistant")
                  (cons 'modelIntent
                    (list (cons 'provider "all-in-one") (cons 'modelId "coding-fast"))
                  )
                ) ;list
                '()
              ) ;pi-resolve-session-state
             ) ;
            ) ;
  (check (cdr (assoc 'provider cfg)) => "all-in-one")
  (check (cdr (assoc 'model-id cfg)) => "coding-fast")
  (check warnings => '())
) ;let-values

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)
