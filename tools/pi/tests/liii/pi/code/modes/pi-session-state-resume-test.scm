;; pi-session-state-resume
;; 验证会话状态恢复流程的端到端行为。
;;
;; 语法
;; ----
;; (pi-resolve-session-state parsed cfg cwd)
;;
;; 参数
;; ----
;; parsed : list?
;; 命令行解析后的参数 alist。
;;
;; cfg : list?
;; Agent 配置 alist。
;;
;; cwd : string
;; 当前工作目录。
;;
;; 返回值
;; ----
;; list?
;; 恢复后的会话状态字典。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii shutil)
  (liii json)
  (liii pi code cli args)
  (liii pi code cli args query)
  (liii pi code config)
  (liii pi code core compaction session)
  (liii pi code core session-manager)
  (liii pi code core session-projection)
  (liii pi code session-state)
) ;import

(check-set-mode! 'report-failed)

;; 恢复会话路径：会话文件（含 message 与状态条目）经投影入口读取，
;; 再交给 pi-resolve-session-state 合并。
;; 状态条目经 1201-09 的投影导出为 thinkingLevel 与 model；
;; fixture 补 auth.json，保证状态里的模型能通过凭证校验被真正应用。
;; session-file 是内部键（由 CLI 的恢复选择写入），这里用 pi-args-set 注入。

(define TMP-DIR
  (path->string (path-join (path-temp-dir) "pi-session-state-resume-test"))
) ;define

(define SESSION-FILE (path->string (path-join (path TMP-DIR) "session.jsonl")))

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR)
) ;when

(path-mkdir TMP-DIR :parents #t :exist-ok #t)
;; agent dir 的凭证：状态里的 minimax-cn 模型需要它才会被应用
(path-write-text (path->string (path-join (path TMP-DIR) (path "auth.json")))
  "{\"minimax-cn\":{\"type\":\"api_key\",\"key\":\"fixture-minimax-key\"}}"
) ;path-write-text
(save-session-header! SESSION-FILE "sess-resume" "2026-10-30T08:00:00.000Z"
  TMP-DIR
) ;save-session-header!
(append-session-entry! SESSION-FILE
  (json->string
    (list (cons "type" "message")
      (cons "id" "m1")
      (cons "parentId" "sess-resume")
      (cons "message" (list (cons "role" "user") (cons "content" "hello")))
    ) ;list
  ) ;json->string
) ;append-session-entry!
(append-session-state-entry! SESSION-FILE
  "model_change"
  (list (cons "provider" "minimax-cn") (cons "model-id" "MiniMax-M3.1-Flash"))
) ;append-session-state-entry!
(append-session-state-entry! SESSION-FILE
  "thinking_level_change"
  (list (cons "level" "high"))
) ;append-session-state-entry!

(let* ((parsed (pi-args-set (parse-pi-args '()) 'session-file SESSION-FILE))
       (cfg (resolve-pi-config parsed (list (cons "PI_CODING_AGENT_DIR" TMP-DIR))))
       (entries (pi-read-session-entries SESSION-FILE))
      ) ;
  (check (length entries) => 4)
  (let-values (((msgs state diagnostics) (pi-build-session-projection entries)))
    ;; 只有 message 条目进入上下文；状态条目不产生消息
    (check (length msgs) => 1)
    ;; 投影从状态条目导出思考等级与模型
    (check (cdr (assoc 'thinkingLevel state)) => "high")
    (let ((model (cdr (assoc 'model state))))
      (check (cdr (assoc 'provider model)) => "minimax-cn")
      (check (cdr (assoc 'modelId model)) => "MiniMax-M3.1-Flash")
    ) ;let
    (let-values (((merged warnings) (pi-resolve-session-state cfg state parsed)))
      ;; 未显式指定 --provider/--model/--thinking 时，状态覆盖 cfg 默认值
      (check (pi-cfg-ref merged 'provider) => "minimax-cn")
      (check (pi-cfg-ref merged 'model-id) => "MiniMax-M3.1-Flash")
      (check (pi-cfg-ref merged 'thinking-level) => "high")
      (check (pi-cfg-ref merged 'api-key) => "fixture-minimax-key")
      (check warnings => '())
    ) ;let-values
  ) ;let-values
  ;; pi-load-session-state：--session-file 命中的会话直接返回投影状态
  (let ((state (pi-load-session-state parsed cfg)))
    (check (if state #t #f) => #t)
    (check (cdr (assoc 'thinkingLevel state)) => "high")
    (check
      (cdr (assoc 'provider (cdr (assoc 'model state))))
      =>
      "minimax-cn"
    ) ;check
  ) ;let
  ;; --no-session 时不做状态读取
  (check (pi-load-session-state (parse-pi-args '("--no-session")) cfg) => #f)
) ;let*

(rmtree TMP-DIR)

(check-report)
