(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii shutil)
  (liii string)
  (liii list)
  (liii json)
  (srfi srfi-13)
  (liii pi ai message)
  (liii pi code core session-manager)
  (liii pi code core session-projection)
) ;import

(check-set-mode! 'report-failed)

;; 投影状态导出：thinkingLevel 与 model
;; 状态条目形如：
;; - {type:"thinking_level_change", thinkingLevel:"high"}（本地写入也用 level）
;; - {type:"model_change", provider:"openai", modelId:"gpt-4o"}（本地写入也用 model-id）
;; 同类条目多次出现时，路径上最后一条（最靠近叶子）为准；
;; 状态扫描作用于 pi-session-path 的完整路径，压缩不会裁掉压缩点之前的设置。

;; 建一个 user message 条目

(define (make-user id parent-id text)
  (list (cons "type" "message")
    (cons "id" id)
    (cons "parentId" parent-id)
    (cons "message" (list (cons "role" "user") (cons "content" text)))
  ) ;list
) ;define

;; 建一个 assistant message 条目：provider / model 也是模型状态的来源

(define (make-assistant id parent-id provider model)
  (list (cons "type" "message")
    (cons "id" id)
    (cons "parentId" parent-id)
    (cons "message"
      (list (cons "role" "assistant")
        (cons "content" (list (list (cons "type" "text") (cons "text" "ok"))))
        (cons "provider" provider)
        (cons "model" model)
      ) ;list
    ) ;cons
  ) ;list
) ;define

;; 建一个 thinking_level_change 条目；key 用于覆盖 thinkingLevel / level 两种拼写

(define (make-thinking id parent-id key level)
  (list (cons "type" "thinking_level_change")
    (cons "id" id)
    (cons "parentId" parent-id)
    (cons key level)
  ) ;list
) ;define

;; 建一个 model_change 条目；key 用于覆盖 modelId / model-id 两种拼写

(define (make-model id parent-id provider key model-id)
  (list (cons "type" "model_change")
    (cons "id" id)
    (cons "parentId" parent-id)
    (cons "provider" provider)
    (cons key model-id)
  ) ;list
) ;define

;; 建一个 compaction 条目

(define (make-compaction id parent-id summary kept-id)
  (list (cons "type" "compaction")
    (cons "id" id)
    (cons "parentId" parent-id)
    (cons "summary" summary)
    (cons "firstKeptEntryId" kept-id)
  ) ;list
) ;define

(define (state-ref state key)
  (cdr (assoc key state))
) ;define

(define (diag-ref diag key fallback)
  (cdr (or (assoc key diag) (cons #f fallback)))
) ;define

;; 1. 路径上没有状态条目：state 为 #f（沿用命令行与配置文件）
(let*-values (((entries) (list (make-user "m1" 'null "one") (make-user "m2" "m1" "two")))
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check state => #f)
  (check (length msgs) => 2)
  (check (diag-ref diag 'skipped-unknown-types '()) => '())
) ;let*-values

;; 2. 单条 model_change 生效：thinking 部分为 #f，state 整体不为 #f
(let*-values (((entries)
               (list (make-user "m1" 'null "one")
                 (make-model "d1" "m1" "openai" "modelId" "gpt-4o")
                 (make-user "m2" "d1" "two")
               ) ;list
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check-true (if state #t #f))
  (check (state-ref state 'thinkingLevel) => #f)
  (check (state-ref state 'thinkingLevelEntryId) => #f)
  (check (state-ref state 'modelEntryId) => "d1")
  (let ((model (state-ref state 'model)))
    (check (cdr (assoc 'provider model)) => "openai")
    (check (cdr (assoc 'modelId model)) => "gpt-4o")
  ) ;let
  (check (diag-ref diag 'skipped-unknown-types '()) => '())
) ;let*-values

;; 3. 单条 thinking_level_change 生效：model 部分为 #f，state 整体不为 #f
(let*-values (((entries)
               (list (make-user "m1" 'null "one")
                 (make-thinking "t1" "m1" "thinkingLevel" "high")
                 (make-user "m2" "t1" "two")
               ) ;list
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check-true (if state #t #f))
  (check (state-ref state 'thinkingLevel) => "high")
  (check (state-ref state 'thinkingLevelEntryId) => "t1")
  (check (state-ref state 'model) => #f)
  (check (state-ref state 'modelEntryId) => #f)
  (check (diag-ref diag 'skipped-unknown-types '()) => '())
) ;let*-values

;; 4. 同类条目多次出现：model 与 thinking 都取路径上最后一条，来源 id 同步
(let*-values (((entries)
               (list (make-user "m1" 'null "one")
                 (make-model "d1" "m1" "openai" "modelId" "gpt-4o")
                 (make-thinking "t1" "d1" "thinkingLevel" "low")
                 (make-model "d2" "t1" "anthropic" "modelId" "claude-sonnet")
                 (make-thinking "t2" "d2" "thinkingLevel" "high")
                 (make-user "m2" "t2" "two")
               ) ;list
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check (state-ref state 'thinkingLevel) => "high")
  (check (state-ref state 'thinkingLevelEntryId) => "t2")
  (check (state-ref state 'modelEntryId) => "d2")
  (let ((model (state-ref state 'model)))
    (check (cdr (assoc 'provider model)) => "anthropic")
    (check (cdr (assoc 'modelId model)) => "claude-sonnet")
  ) ;let
) ;let*-values

;; 5. 压缩点之前的状态条目仍生效：状态扫描基于完整路径，不受裁剪影响
;;    路径 m1 -> d1(model) -> m2 -> c1(compaction, firstKeptEntryId=m2) -> m3，
;;    裁剪结果为 [c1, m2, m3]，d1 已被裁掉，但 state 仍取 d1 的设置。
(let*-values (((entries)
               (list (make-user "m1" 'null "one")
                 (make-model "d1" "m1" "openai" "modelId" "gpt-4o-mini")
                 (make-user "m2" "d1" "two")
                 (make-compaction "c1" "m2" "sum" "m2")
                 (make-user "m3" "c1" "three")
               ) ;list
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  ;; 首条摘要 + 保留的 m2、m3：d1 不在上下文消息里
  (check (length msgs) => 3)
  (check (state-ref state 'modelEntryId) => "d1")
  (let ((model (state-ref state 'model)))
    (check (cdr (assoc 'provider model)) => "openai")
    (check (cdr (assoc 'modelId model)) => "gpt-4o-mini")
  ) ;let
  (check (diag-ref diag 'trim-reason #f) => #f)
) ;let*-values

;; 6. 本地写入拼写 level：与 thinkingLevel 等价读取
(let*-values (((entries)
               (list (make-user "m1" 'null "one") (make-thinking "t1" "m1" "level" "medium"))
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check (state-ref state 'thinkingLevel) => "medium")
  (check (state-ref state 'thinkingLevelEntryId) => "t1")
) ;let*-values

;; 7. 本地写入拼写 model-id：与 modelId 等价读取
(let*-values (((entries)
               (list (make-user "m1" 'null "one")
                 (make-model "d1" "m1" "google" "model-id" "gemini-pro")
               ) ;list
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check (state-ref state 'modelEntryId) => "d1")
  (let ((model (state-ref state 'model)))
    (check (cdr (assoc 'provider model)) => "google")
    (check (cdr (assoc 'modelId model)) => "gemini-pro")
  ) ;let
) ;let*-values

;; 8. 状态条目的值类型异常：modelId 为数字按缺失处理，只保留可用键，不伪造字符串
(let*-values (((entries)
               (list (make-user "m1" 'null "one") (make-model "d1" "m1" "openai" "modelId" 42))
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check (state-ref state 'modelEntryId) => "d1")
  (let ((model (state-ref state 'model)))
    (check (cdr (assoc 'provider model)) => "openai")
    (check (assoc 'modelId model) => #f)
  ) ;let
) ;let*-values

;; 9. model_change 与 thinking_level_change 属已知类型：不计入 skipped-unknown-types
(let*-values (((entries)
               (list (make-user "m1" 'null "one")
                 (make-model "d1" "m1" "openai" "modelId" "gpt-4o")
                 (make-thinking "t1" "d1" "thinkingLevel" "high")
                 (make-user "m2" "t1" "two")
               ) ;list
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check (diag-ref diag 'skipped-unknown-types '()) => '())
) ;let*-values

;; 10. 没有 model_change 时回退到最后一条 assistant 消息的 provider / model
;;     （对齐上游 getSessionContextSettings：assistant 消息也参与模型状态）
(let*-values (((entries)
               (list (make-user "m1" 'null "one")
                 (make-assistant "a1" "m1" "anthropic" "claude-sonnet")
                 (make-user "m2" "a1" "two")
               ) ;list
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check (state-ref state 'modelEntryId) => "a1")
  (let ((model (state-ref state 'model)))
    (check (cdr (assoc 'provider model)) => "anthropic")
    (check (cdr (assoc 'modelId model)) => "claude-sonnet")
  ) ;let
  (check (state-ref state 'thinkingLevel) => #f)
) ;let*-values

;; 11. model_change 之后还有 assistant 消息：assistant 更靠近叶子，覆盖 model_change
(let*-values (((entries)
               (list (make-user "m1" 'null "one")
                 (make-model "d1" "m1" "openai" "modelId" "gpt-4o")
                 (make-assistant "a1" "d1" "anthropic" "claude-opus")
               ) ;list
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check (state-ref state 'modelEntryId) => "a1")
  (let ((model (state-ref state 'model)))
    (check (cdr (assoc 'provider model)) => "anthropic")
    (check (cdr (assoc 'modelId model)) => "claude-opus")
  ) ;let
) ;let*-values

;; 12. model_change 在 assistant 消息之后：model_change 胜出，assistant 的 model 不参与
(let*-values (((entries)
               (list (make-assistant "a1" 'null "anthropic" "claude-opus")
                 (make-model "d1" "a1" "openai" "modelId" "gpt-4o")
                 (make-user "m1" "d1" "two")
               ) ;list
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check (state-ref state 'modelEntryId) => "d1")
  (let ((model (state-ref state 'model)))
    (check (cdr (assoc 'provider model)) => "openai")
    (check (cdr (assoc 'modelId model)) => "gpt-4o")
  ) ;let
) ;let*-values

;; 13. JSONL 真实落盘往返：model_change / thinking_level_change 行解析后导出状态
(let* ((tmp-dir (path->string (path-join (path-temp-dir) "test-pi-projection-state-1209"))
       ) ;tmp-dir
       (session-file (path->string (path-join tmp-dir "roundtrip.jsonl")))
      ) ;
  (when (file-exists? tmp-dir)
    (rmtree tmp-dir)
  ) ;when
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (save-session-header! session-file "sess-1209" "2026-10-29T10:00:00.000Z"
    tmp-dir
  ) ;save-session-header!
  (save-session-message! session-file
    "{\"role\":\"user\",\"content\":\"first\"}" "m1" #f
    "2026-10-29T10:00:01.000Z"
  ) ;save-session-message!
  (append-session-entry! session-file
    "{\"type\":\"model_change\",\"id\":\"d1\",\"parentId\":\"m1\",\"timestamp\":\"2026-10-29T10:00:02.000Z\",\"provider\":\"openai\",\"modelId\":\"gpt-4o\"}"
  ) ;append-session-entry!
  (append-session-entry! session-file
    "{\"type\":\"thinking_level_change\",\"id\":\"t1\",\"parentId\":\"d1\",\"timestamp\":\"2026-10-29T10:00:03.000Z\",\"thinkingLevel\":\"high\"}"
  ) ;append-session-entry!
  (save-session-message! session-file
    "{\"role\":\"user\",\"content\":\"second\"}" "m2" "t1"
    "2026-10-29T10:00:04.000Z"
  ) ;save-session-message!
  (let*-values (((lines) (path-read-lines session-file))
                ((entries)
                 (filter (lambda (e) e)
                   (map
                     (lambda (line)
                       (catch #t (lambda () (string->json (string-trim line))) (lambda _ #f))
                     ) ;lambda
                     lines
                   ) ;map
                 ) ;filter
                ) ;
                ((msgs state diag) (pi-build-session-projection entries))
               ) ;
    (check (length msgs) => 2)
    (check (state-ref state 'thinkingLevel) => "high")
    (check (state-ref state 'thinkingLevelEntryId) => "t1")
    (check (state-ref state 'modelEntryId) => "d1")
    (let ((model (state-ref state 'model)))
      (check (cdr (assoc 'provider model)) => "openai")
      (check (cdr (assoc 'modelId model)) => "gpt-4o")
    ) ;let
    (check (diag-ref diag 'skipped-unknown-types '()) => '())
    (check (diag-ref diag 'path-reason #f) => #f)
  ) ;let*-values
  (rmtree tmp-dir)
) ;let*

(check-report)
