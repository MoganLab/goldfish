;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api google-generative-ai thought-signature)
  (import (scheme base) (scheme char) (liii string) (liii pi ai message))
  (export gemini-valid-thought-signature?
    gemini-assistant-block->part
    gemini-requires-signature-sentinel?
  ) ;export
  (begin

    (define* (%val key obj (default-val #f))
      (let ((p
              (or (assoc key obj)
                (assoc (if (symbol? key) (symbol->string key) (string->symbol key)) obj)
              ) ;or
            ) ;p
           ) ;
        (if (pair? p) (cdr p) default-val)
      ) ;let
    ) ;define*

    ;; Gemini 3（含 3.x）在开启 thinking 时要求历史 functionCall 全部携带
    ;; thoughtSignature；跨模型切换（如 all-in-one 链在 gemini 与 glm/minimax
    ;; 之间回溯）会把无签名的外来 toolCall 原样回放，触发 HTTP 400:
    ;; "Function call is missing a thought_signature in functionCall parts"。
    ;; 对齐上游 pi 的 SKIP_THOUGHT_SIGNATURE 方案（badlogic/pi-mono a0d839ce8），
    ;; 为外来无签名 toolCall 注入官方哨兵值跳过校验，保留结构化上下文。
    ;; 注意不能误伤合法场景：同模型但签名非法时依然剔除（Corrupted 签名回传
    ;; 同样报 400），仅当本来就没有有效签名时才补哨兵。
    (define GEMINI-SKIP-THOUGHT-SIGNATURE "skip_thought_signature_validator")

    ;; 模型主版本号：gemini-3* / gemini-3.8-flash / gemini-live-3* → 3
    (define (%gemini-major-version model-id)
      (let* ((s (if (string? model-id) (string-downcase model-id) ""))
             (prefices '("gemini-live-" "gemini-"))
            ) ;
        (let loop
          ((ps prefices))
          (cond ((null? ps) #f)
                ((string-starts? s (car ps))
                 (let* ((rest (substring s (string-length (car ps)) (string-length s)))
                        (c0 (and (not (string-null? rest)) (string-ref rest 0)))
                       ) ;
                   (and c0
                     (char-numeric? c0)
                     (let dl
                       ((i 0))
                       (cond
                        ((and (< i (string-length rest)) (char-numeric? (string-ref rest i)))
                         (dl (+ i 1))
                        ) ;
                        ((> i 0) (string->number (substring rest 0 i)))
                        (else #f)
                       ) ;cond
                     ) ;let
                   ) ;and
                 ) ;let*
                ) ;
                (else (loop (cdr ps)))
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

    ;; 当前模型是否为 Gemini 3+（需要为无签名外来 toolCall 补哨兵）
    (define (gemini-requires-signature-sentinel? model-id)
      (let ((v (%gemini-major-version model-id)))
        (and (number? v) (>= v 3))
      ) ;let
    ) ;define

    ;; thought signature 须为 base64（TYPE_BYTES）：长度 4 倍数且仅含
    ;; [A-Za-z0-9+/] 与至多两个末尾 =（对齐上游 isValidThoughtSignature）
    (define (gemini-valid-thought-signature? sig)
      (and (string? sig)
        (not (string-null? sig))
        (zero? (modulo (string-length sig) 4))
        (let loop
          ((i 0))
          (cond ((>= i (string-length sig)) #t)
                (else
                  (let ((c (string-ref sig i)))
                    (cond ((or (char-alphabetic? c) (char-numeric? c) (char=? c #\+) (char=? c #\/))
                           (loop (+ i 1))
                          ) ;
                          ((char=? c #\=)
                           ;; = 只允许出现在最后两位
                           (and (>= i (- (string-length sig) 2)) (loop (+ i 1)))
                          ) ;
                          (else #f)
                    ) ;cond
                  ) ;let
                ) ;else
          ) ;cond
        ) ;let
      ) ;and
    ) ;define

    ;; 同 provider+model 且签名合法时才回放（对齐上游 resolveThoughtSignature）；
    ;; 跨模型的有效签名不可回传（异构签名回传报 400: Corrupted），返回 #f
    (define (%resolve-thought-signature same-model? sig)
      (if (and same-model? (gemini-valid-thought-signature? sig)) sig #f)
    ) ;define

    ;; assistant 内容块转换为 Gemini part；blk 可以是内部 record 块或 alist。
    ;; same-model? 为 #f 时表示外来模型的历史：toolCall 无签名回放会被
    ;; Gemini 3+ 拒绝，由 sentinel? 决定是否补哨兵（Gemini 3+ 时应补）。
    (define* (gemini-assistant-block->part blk same-model? (model-id #f))
      (let* ((blk (if (pi-block? blk) (pi-block->alist blk) blk))
             (btype (%val 'type blk ""))
            ) ;
        (cond
         ((equal? btype "text")
          (let* ((t (%val 'text blk ""))
                 (sig (%resolve-thought-signature same-model?
                        (%val 'textSignature blk (%val 'text-signature blk #f))
                      ) ;%resolve-thought-signature
                 ) ;sig
                ) ;
            ;; 空文本但带签名的块保留（Gemini 3 要求签名回显）
            (if
              (or (not (string? t)) (and (string-null? (string-trim-both t)) (not sig)))
              #f
              (if sig
                (list (cons "thoughtSignature" sig) (cons "text" t))
                (list (cons "text" t))
              ) ;if
            ) ;if
          ) ;let*
         ) ;
         ((equal? btype "thinking")
          (let* ((th (%val 'thinking blk ""))
                 (sig (%resolve-thought-signature same-model?
                        (%val 'thinkingSignature blk (%val 'thinking-signature blk #f))
                      ) ;%resolve-thought-signature
                 ) ;sig
                 (empty? (or (not (string? th)) (string-null? (string-trim-both th))))
                ) ;
            (if same-model?
              (if (and empty? (not sig))
                #f
                (let ((base (list (cons "thought" #t) (cons "text" th))))
                  (if sig (cons (cons "thoughtSignature" sig) base) base)
                ) ;let
              ) ;if
              ;; 跨模型：thinking 块被过滤，不发射为文本，避免污染模型上下文
              #f
            ) ;if
          ) ;let*
         ) ;
         ((equal? btype "toolCall")
          (let* ((ts (%resolve-thought-signature same-model?
                       (%val 'thoughtSignature blk (%val 'thought-signature blk #f))
                     ) ;%resolve-thought-signature
                 ) ;ts
                 (fc-obj (list (cons "name" (%val 'name blk ""))
                           (cons "args" (%val 'arguments blk '()))
                           (cons "id" (%val 'id blk ""))
                         ) ;list
                 ) ;fc-obj
                 (part (list (cons "functionCall" fc-obj)))
                 ;; 外来无签名 toolCall：Gemini 3+ 补哨兵跳过签名校验；
                 ;; 同模型但签名缺失（如流式中断）同样需要补，否则同样 400
                 (eff-sig (or ts
                            (and (gemini-requires-signature-sentinel? model-id)
                              GEMINI-SKIP-THOUGHT-SIGNATURE
                            ) ;and
                          ) ;or
                 ) ;eff-sig
                ) ;
            (if eff-sig (cons (cons "thoughtSignature" eff-sig) part) part)
          ) ;let*
         ) ;
         (else #f)
        ) ;cond
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
