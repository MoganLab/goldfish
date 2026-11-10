;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api google-generative-ai thought-signature)
  (import (scheme base) (scheme char) (liii string) (liii pi ai message))
  (export gemini-valid-thought-signature? gemini-assistant-block->part)
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
