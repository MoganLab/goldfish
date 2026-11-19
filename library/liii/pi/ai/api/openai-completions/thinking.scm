;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; OpenAI Chat Completions 请求侧思考参数：
;; - 从 models-store 取原始模型条目（compat/thinkingLevelMap 不在 model-metadata 中）
;; - 按 thinkingFormat 生成 thinking / reasoning_effort 字段
;; 目前覆盖 openai 与 deepseek 两种格式；其他格式保持不注入，避免猜错字段。
(define-library (liii pi ai api openai-completions thinking)
  (import (scheme base) (liii string) (liii list))
  (export openai-resolve-model-entry openai-thinking-request-params)
  (begin

    (define (%val key obj default-val)
      (if (not (pair? obj))
        default-val
        (let ((p
                (or (assoc key obj)
                  (assoc (if (symbol? key) (symbol->string key) (string->symbol key)) obj)
                ) ;or
              ) ;p
             ) ;
          (if (pair? p) (cdr p) default-val)
        ) ;let
      ) ;if
    ) ;define

    ;; JSON 布尔归一：string->json 解析为符号 true/false，内置目录可能直接写 #t/#f。
    (define (%json-true? v)
      (or (eq? v #t) (eq? v 'true))
    ) ;define

    (define (%model-supports-reasoning? model)
      (and (pair? model) (%json-true? (%val "reasoning" model #f)))
    ) ;define

    (define (%compat-ref model key default-val)
      (let ((compat (%val "compat" model #f)))
        (if (pair? compat) (%val key compat default-val) default-val)
      ) ;let
    ) ;define

    (define (%thinking-off? level)
      (or (not (string? level))
        (string=? (string-trim-both level) "off")
      ) ;or
    ) ;define

    ;; 在模型条目数组中按 id 查找
    (define (%find-model-entry models model-id)
      (let loop
        ((rest (cond ((vector? models) (vector->list models))
                     ((list? models) models)
                     (else '())
               ) ;cond
         ) ;rest
        ) ;
        (cond ((null? rest) #f)
              ((equal? (%val "id" (car rest) #f) model-id) (car rest))
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    ;; 从 models-store 中取 provider/model 原始条目
    (define (openai-resolve-model-entry models-store provider model-id)
      (let ((p-entry (and provider (%val provider models-store #f))))
        (and (pair? p-entry)
          (string? model-id)
          (%find-model-entry (%val "models" p-entry '()) model-id)
        ) ;and
      ) ;let
    ) ;define

    ;; thinkingLevelMap 映射：缺失/null 时回退原始级别（对齐上游 ?? 语义）
    (define (%mapped-effort model level)
      (let* ((map (%val "thinkingLevelMap" model '()))
             (mapped (and (pair? map) (%val level map #f)))
            ) ;
        (if (string? mapped) mapped level)
      ) ;let*
    ) ;define

    ;; deepseek：显式 thinking.type + 可选 reasoning_effort
    (define (%deepseek-thinking-params model level)
      (let ((map (%val "thinkingLevelMap" model '())))
        (if (%thinking-off? level)
          (if (eq? (and (pair? map) (%val "off" map #f)) 'null)
            '()
            (list (cons "thinking" (list (cons "type" "disabled"))))
          ) ;if
          (append
            (list (cons "thinking" (list (cons "type" "enabled"))))
            (if (%compat-ref model "supportsReasoningEffort" #t)
              (list (cons "reasoning_effort" (%mapped-effort model level)))
              '()
            ) ;if
          ) ;append
        ) ;if
      ) ;let
    ) ;define

    ;; openai 风格：reasoning_effort
    (define (%openai-thinking-params model level)
      (if (%thinking-off? level)
        (let ((off-effort (%val (%val "thinkingLevelMap" model '()) "off" #f)))
          (if (string? off-effort)
            (list (cons "reasoning_effort" off-effort))
            '()
          ) ;if
        ) ;let
        (if (%compat-ref model "supportsReasoningEffort" #t)
          (list (cons "reasoning_effort" (%mapped-effort model level)))
          '()
        ) ;if
      ) ;if
    ) ;define

    ;; 仅注入已实现且显式声明的格式；未知格式保持原行为（不猜字段）。
    (define (openai-thinking-request-params model level)
      (if (not (%model-supports-reasoning? model))
        '()
        (let ((format (%compat-ref model "thinkingFormat" "openai")))
          (cond ((equal? format "deepseek") (%deepseek-thinking-params model level))
                ((equal? format "openai") (%openai-thinking-params model level))
                (else '())
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
