;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api google-generative-ai thought-signature)
  (import (scheme base)
    (scheme char)
    (liii list)
    (liii string)
    (liii pi ai message)
  ) ;import
  (export gemini-valid-thought-signature? gemini-assistant-block->part
    gemini-dummy-thought-signature gemini-requires-thought-signature?
    gemini-ensure-thought-signature
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

    ;; 同 provider+model 且签名合法时才回放（对齐上游 resolveThoughtSignature）
    (define (%resolve-thought-signature same-model? sig)
      (if (and same-model? (gemini-valid-thought-signature? sig)) sig #f)
    ) ;define

    ;; Google 文档给出的兼容哨兵：调用方确实无法保留真实签名（跨模型交接、
    ;; 自造 functionCall、上游丢弃签名）时，用它跳过 Gemini 3+ 对 functionCall
    ;; 的签名强校验，避免 400 INVALID_ARGUMENT。
    ;; 见 https://ai.google.dev/gemini-api/docs/thought-signatures 的 FAQ 第 1 条。
    (define gemini-dummy-thought-signature "skip_thought_signature_validator")

    (define (%leading-major-version model-id prefix-len)
      (let loop
        ((i prefix-len) (digits '()))
        (cond ((and (< i (string-length model-id))
                 (char-numeric? (string-ref model-id i))
               ) ;and
               (loop (+ i 1) (cons (string-ref model-id i) digits))
              ) ;
              ((pair? digits) (string->number (list->string (reverse digits))))
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    ;; Gemini 主版本号：gemini-3-* / gemini-4-* / gemini-live-3-* 等
    (define (%gemini-major-version model-id)
      (let ((s (if (string? model-id) (string-downcase model-id) "")))
        (cond ((string-starts? s "gemini-live-") (%leading-major-version s 12))
              ((string-starts? s "gemini-") (%leading-major-version s 7))
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    ;; Gemini 3+ 强制要求 functionCall 回传 thought signature（2.5 为可选）
    (define (gemini-requires-thought-signature? model-id)
      (let ((v (%gemini-major-version model-id)))
        (and (number? v) (>= v 3))
      ) ;let
    ) ;define

    ;; 为缺失签名的首个 functionCall part 补兼容哨兵：Gemini 3+ 要求
    ;; 每个 model turn 中的第一个 functionCall 必须携带签名。
    ;; 首个 functionCall 已有签名（含并行调用只签首块）时保持原样。
    (define (gemini-ensure-thought-signature parts model-id)
      (if (not (gemini-requires-thought-signature? model-id))
        parts
        (let loop
          ((rest parts) (acc '()) (done? #f))
          (cond ((null? rest) (reverse acc))
                (else
                  (let ((part (car rest)))
                    (if (and (not done?) (pair? part) (%val "functionCall" part #f))
                      (let ((signed? (or (%val "thoughtSignature" part #f)
                                       (%val "thought-signature" part #f)
                                     ) ;or
                           ) ;signed?
                           ) ;bindings
                        (loop (cdr rest)
                          (cons (if signed?
                                  part
                                  (cons (cons "thoughtSignature" gemini-dummy-thought-signature
                                        ) ;cons
                                    part
                                  ) ;cons
                                ) ;if
                            acc
                          ) ;cons
                          #t
                        ) ;loop
                      ) ;let
                      (loop (cdr rest) (cons part acc) done?)
                    ) ;if
                  ) ;let
                ) ;else
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    ;; assistant 内容块转换为 Gemini part；blk 可以是内部 record 块或 alist
    (define (gemini-assistant-block->part blk same-model?)
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
                ) ;
            (if ts (cons (cons "thoughtSignature" ts) part) part)
          ) ;let*
         ) ;
         (else #f)
        ) ;cond
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
