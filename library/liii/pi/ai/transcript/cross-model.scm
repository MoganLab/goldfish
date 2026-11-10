;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 跨模型转录判定与内容块净化（对应上游 pi-ai 的 api/transform-messages.ts）。
;; 本库只做协议无关的判定与块级净化：
;;   - 同模型判定：provider / model / api 三者必须完全一致；
;;   - 异构思维链（thinking 块）直接丢弃；
;;   - 工具调用块上的 thoughtSignature 与文本块私有签名在跨模型时剥离。
;; 仅作用于请求构建链路，不改会话持久化。
(define-library (liii pi ai transcript cross-model)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii list)
    (liii pi ai message)
  ) ;import
  (export pi-transcript-same-model? pi-transcript-normalize-block)
  (begin

    ;; 关联表取值：兼容字符串键与符号键，缺失时返回默认值
    (define (%val key alist default-val)
      (if (not (pair? alist))
        default-val
        (let* ((str-k (if (symbol? key) (symbol->string key) key))
               (sym-k (if (string? key) (string->symbol key) key))
               (p (or (assoc key alist) (assoc str-k alist) (assoc sym-k alist)))
              ) ;
          (if p (cdr p) default-val)
        ) ;let*
      ) ;if
    ) ;define

    ;; 可选元数据比对：目标侧缺省时要求消息侧同样缺省，否则要求严格相等
    ;; 可选元数据比对：两侧都有值时要求严格相等；任一侧缺省时不做约束
    ;; （缺失元数据的历史消息由 model 维度的严格比对兜底）
    (define (%match-optional target actual)
      (if (and target actual) (equal? target actual) #t)
    ) ;define

    ;; 消息是否与当前请求目标属于同一模型（对标上游 isSameModel）
    ;; model 必须完全匹配；provider 与 api 两侧都有值时同样要求严格相等
    (define (pi-transcript-same-model? msg config)
      (let ((target-model
              (%val "model-id"
                config
                (%val 'model-id config (%val "model" config (%val 'model config #f)))
              ) ;%val
            ) ;target-model
            (target-provider (%val "provider" config (%val 'provider config #f)))
            (target-api
              (%val "provider-api"
                config
                (%val 'provider-api config (%val "api" config (%val 'api config #f)))
              ) ;%val
            ) ;target-api
            (msg-model
              (%val "model"
                msg
                (%val 'model msg (%val "model-id" msg (%val 'model-id msg #f)))
              ) ;%val
            ) ;msg-model
            (msg-provider (%val "provider" msg (%val 'provider msg #f)))
            (msg-api
              (%val "api"
                msg
                (%val 'api msg (%val "provider-api" msg (%val 'provider-api msg #f)))
              ) ;%val
            ) ;msg-api
           ) ;
        (and (string? target-model)
          (string? msg-model)
          (equal? target-model msg-model)
          (%match-optional target-provider msg-provider)
          (%match-optional target-api msg-api)
        ) ;and
      ) ;let
    ) ;define

    ;; 从块关联表中剔除指定签名字段（同时兼容字符串键与符号键）
    (define (%strip-keys alist keys)
      (filter
        (lambda (p) (not (member (car p) keys)))
        alist
      ) ;filter
    ) ;define

    ;; 剥离工具调用块的 thoughtSignature
    (define (%strip-thought-signature blk)
      (%strip-keys blk '("thoughtSignature" thoughtSignature))
    ) ;define

    ;; 剥离文本块的私有签名元数据
    (define (%strip-text-signature blk)
      (%strip-keys blk '("textSignature" textSignature "signature" signature))
    ) ;define

    ;; 同模型下思维链是否值得保留：含签名（加密推理）或思维正文非空
    (define (%keep-thinking? blk)
      (let ((thinking (%val "thinking" blk ""))
            (sig (or (%val "thinkingSignature" blk #f) (%val "thoughtSignature" blk #f)))
           ) ;
        (or sig
          (and (string? thinking) (not (string-null? (string-trim-both thinking))))
        ) ;or
      ) ;let
    ) ;define

    ;; 内容块净化：跨模型时丢弃思维链并剥离私有签名
    ;; 返回新的块 alist；丢弃时返回 #f
    (define (pi-transcript-normalize-block blk same-model?)
      (let ((b (if (pi-block? blk) (pi-block->alist blk) blk)))
        (if (not (pair? b))
          b
          (let ((btype (%val "type" b "")))
            (cond
             ((equal? btype "thinking")
              (if (not same-model?) #f (if (%keep-thinking? b) b #f))
             ) ;
             ((equal? btype "text") (if same-model? b (%strip-text-signature b)))
             ((equal? btype "toolCall") (if same-model? b (%strip-thought-signature b)))
             (else b)
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
