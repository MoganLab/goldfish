;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;;; 投影状态导出：从会话路径提取 thinkingLevel 与 model。
;; 从投影入口拆出，避免投影主库超过单文件行数限制；纯函数，不读文件、不写日志。
(define-library (liii pi code core session-projection state)
  (import (scheme base) (liii string))
  (export pi-collect-projection-state)
  (begin
    ;; 字段访问兼容字符串键与符号键；与投影主库的 %ref 同构（子库不共享内部定义）
    (define (%ref obj key)
      (let* ((str-key (if (symbol? key) (symbol->string key) key))
             (sym-key (if (string? key) (string->symbol key) key))
             (pair (or (assoc str-key obj) (assoc sym-key obj)))
            ) ;
        (if pair (cdr pair) #f)
      ) ;let*
    ) ;define

    ;; 条目 id：仅接受非空字符串，其余按缺失处理
    (define (%entry-id entry)
      (let ((id (%ref entry "id")))
        (and (string? id) (not (string-null? id)) id)
      ) ;let
    ) ;define

    ;; 路径上最后一个指定 type 的条目（根到叶顺序中最靠近叶子）；无匹配返回 #f
    (define (%last-path-entry-of-type path type)
      (let loop
        ((rest (reverse path)))
        (cond ((null? rest) #f)
              ((equal? (%ref (car rest) "type") type) (car rest))
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    ;; 状态字段的字符串值：按 keys 顺序取第一个字符串值。
    ;; 上游会话条目的键是 "thinkingLevel" / "modelId"，本仓库描述读写时
    ;; 使用 "level" / "model-id"；此处兼容读取上游会话与本地写入的两种拼写，
    ;; 优先上游键名。值缺失或非字符串时继续尝试后一个键，全部不可用返回 #f。
    (define (%state-field entry keys)
      (let loop
        ((rest keys))
        (cond ((null? rest) #f)
              (else
                (let ((v (%ref entry (car rest))))
                  (if (string? v) v (loop (cdr rest)))
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    ;; 模型状态的最后一条来源：model_change 或 assistant 消息（对齐上游
    ;; getSessionContextSettings：两者都参与模型状态，后者在路径上更靠后时覆盖前者）
    (define (%last-model-source path)
      (let loop
        ((rest (reverse path)))
        (cond ((null? rest) #f)
              ((equal? (%ref (car rest) "type") "model_change") (car rest))
              ((and (equal? (%ref (car rest) "type") "message")
                 (equal? (%ref (%ref (car rest) "message") "role") "assistant")
               ) ;and
               (car rest)
              ) ;
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    ;; 模型来源条目的字段：model_change 自带 provider / modelId，
    ;; assistant 消息把 provider / model 放在 message 里；缺失返回 #f
    (define (%model-field entry type-keys message-keys)
      (let ((msg (%ref entry "message")))
        (if (and (pair? msg) (equal? (%ref entry "type") "message"))
          (%state-field msg message-keys)
          (%state-field entry type-keys)
        ) ;if
      ) ;let
    ) ;define

    ;; pi-collect-projection-state：从完整路径提取会话设置（对齐上游 buildSessionContext 的第二步）：
    ;; - thinking_level_change 取思考等级；model 取路径上最后的模型来源：
    ;;   model_change 的 provider / modelId，或更靠后的 assistant 消息的 provider / model
    ;; - 同类条目多次出现时，路径上最后一条（最靠近叶子）为准
    ;; - 值缺失或非字符串时该字段按缺失处理，不产出字段也不记来源 id
    ;; - provider / modelId 任一可用即产出 model 子 alist，只放入可用键
    ;; 返回 #f（路径上没有可用的状态条目，沿用命令行与配置文件），
    ;; 或 alist：thinkingLevel / thinkingLevelEntryId / model / modelEntryId，
    ;; 来源 id 为该字段实际取值的那条状态条目的 id
    (define (pi-collect-projection-state path)
      (let* ((thinking-entry (%last-path-entry-of-type path "thinking_level_change"))
             (model-entry (%last-model-source path))
             (thinking-level (and thinking-entry (%state-field thinking-entry '("thinkingLevel"
                                                                                "level")))
             ) ;thinking-level
             (thinking-entry-id (and thinking-level (%entry-id thinking-entry)))
             (provider (and model-entry (%model-field model-entry '("provider") '("provider")))
             ) ;provider
             (model-id (and model-entry (%model-field model-entry '("modelId"
                                                                    "model-id") '("model")))
             ) ;model-id
             (model
               (and (or provider model-id)
                 (append (if provider (list (cons 'provider provider)) '())
                   (if model-id (list (cons 'modelId model-id)) '())
                 ) ;append
               ) ;and
             ) ;model
             (model-entry-id (and model model-entry (%entry-id model-entry)))
            ) ;
        (if (and (not thinking-level) (not model))
          #f
          (list (cons 'thinkingLevel thinking-level)
            (cons 'thinkingLevelEntryId thinking-entry-id)
            (cons 'model model)
            (cons 'modelEntryId model-entry-id)
          ) ;list
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
