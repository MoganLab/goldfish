;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api anthropic-messages thinking)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii pi ai message)
    (liii pi ai api anthropic-messages stream-state)
  ) ;import
  (export anthropic-thinking-level->budget anthropic-thinking-level->effort
    anthropic-adaptive-thinking-model? anthropic-inject-thinking
    anthropic-start-thinking-block anthropic-handle-thinking-delta
    anthropic-finish-thinking-block
  ) ;export
  (begin

    (define-record-type <anthropic-thinking-state>
      (%make-anthropic-thinking-state thinking signature index)
      %anthropic-thinking-state?
      (thinking %anthropic-thinking-state-record-thinking)
      (signature %anthropic-thinking-state-record-signature)
      (index %anthropic-thinking-state-record-index)
    ) ;define-record-type

    (define (%anthropic-thinking-state-thinking state)
      (if (%anthropic-thinking-state? state)
        (%anthropic-thinking-state-record-thinking state)
        (cdr (assoc 'thinking state))
      ) ;if
    ) ;define

    (define (%anthropic-thinking-state-signature state)
      (if (%anthropic-thinking-state? state)
        (%anthropic-thinking-state-record-signature state)
        (cdr (assoc 'signature state))
      ) ;if
    ) ;define

    (define (%anthropic-thinking-state-index state)
      (if (%anthropic-thinking-state? state)
        (%anthropic-thinking-state-record-index state)
        (cdr (assoc 'index state))
      ) ;if
    ) ;define

    (define (anthropic-thinking-level->budget level)
      (cond ((equal? level "minimal") 1024)
            ((equal? level "low") 2048)
            ((equal? level "medium") 8192)
            ((equal? level "high") 16384)
            ((or (equal? level "xhigh") (equal? level "max")) 16384)
            (else #f)
      ) ;cond
    ) ;define

    ;; 自适应思考的 effort 档位；off 或未知级别返回 #f
    (define (anthropic-thinking-level->effort level)
      (cond ((or (equal? level "minimal") (equal? level "low")) "low")
            ((equal? level "medium") "medium")
            ((equal? level "high") "high")
            ((equal? level "xhigh") "xhigh")
            ((equal? level "max") "max")
            (else #f)
      ) ;cond
    ) ;define

    (define (%digits? s)
      (and (string? s) (not (string-null? s)) (string-every char-numeric? s))
    ) ;define

    ;; 解析 Claude 模型 id，返回 (family major minor)；无法识别时返回 #f。
    ;; 例："claude-opus-5-5" -> ("opus" 5 5)，"claude-haiku-4-5-20251001" -> ("haiku" 4 5)，
    ;; "claude-fable-5" -> ("fable" 5 0)；允许 "anthropic/claude-..." 这类前缀。
    ;; 旧命名（如 "claude-3-7-sonnet"）的第二段不是系列名，返回 #f。
    (define (%claude-version model-id)
      (let loop
        ((parts (if (string? model-id) (string-split (string-downcase model-id) #\-) '())
         ) ;parts
        ) ;
        (cond ((null? parts) #f)
              ((and (string-ends? (car parts) "claude")
                 (pair? (cdr parts))
                 (pair? (cddr parts))
                 (member (cadr parts) '("opus" "sonnet" "haiku" "fable" "mythos"))
                 (%digits? (caddr parts))
               ) ;and
               (let ((minor-part (and (pair? (cdddr parts)) (cadddr parts))))
                 (list (cadr parts)
                   (string->number (caddr parts))
                   (if (and (%digits? minor-part) (<= (string-length minor-part) 2))
                     (string->number minor-part)
                     0
                   ) ;if
                 ) ;list
               ) ;let
              ) ;
              (else (loop (cdr parts)))
        ) ;cond
      ) ;let
    ) ;define

    ;; Claude 4.6 起（Fable / Mythos 全系，Opus / Sonnet 4.6+，Haiku 5+）改用自适应思考：
    ;; thinking.type = "adaptive" + output_config.effort；这些模型对
    ;; {"type": "enabled", "budget_tokens": N} 返回 HTTP 400。其余模型（含各家
    ;; Anthropic 兼容端点）仍走 budget_tokens。
    (define (anthropic-adaptive-thinking-model? model-id)
      (let ((v (%claude-version model-id)))
        (and v
          (let ((family (car v)) (major (cadr v)) (minor (caddr v)))
            (cond ((member family '("fable" "mythos")) #t)
                  ((equal? family "haiku") (>= major 5))
                  (else (or (> major 4) (and (= major 4) (>= minor 6))))
            ) ;cond
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    ;; Opus / Sonnet 4.6 没有 xhigh 档（Opus 4.7 起才有），降为 high
    (define (%effort-for-model model-id effort)
      (let ((v (%claude-version model-id)))
        (if (and (equal? effort "xhigh") v (= (cadr v) 4) (= (caddr v) 6))
          "high"
          effort
        ) ;if
      ) ;let
    ) ;define

    (define (%body-model-id req-body)
      (let ((p (or (assoc "model" req-body) (assoc 'model req-body))))
        (and p (cdr p))
      ) ;let
    ) ;define

    ;; 自适应思考：display 显式设为 summarized，否则新模型默认 omitted，
    ;; thinking 块只有签名、没有文本，界面上看不到思考过程。
    ;; off 时不发 thinking 字段，由模型按默认行为处理（Opus 5.5 等无法关闭思考）。
    (define (%inject-adaptive-thinking req-body level)
      (let ((effort (anthropic-thinking-level->effort level)))
        (if effort
          (cons (cons "thinking" (list (cons "type" "adaptive") (cons "display" "summarized")))
            (cons
              (cons "output_config"
                (list (cons "effort" (%effort-for-model (%body-model-id req-body) effort)))
              ) ;cons
              req-body
            ) ;cons
          ) ;cons
          req-body
        ) ;if
      ) ;let
    ) ;define

    (define (anthropic-inject-thinking req-body level)
      (if (anthropic-adaptive-thinking-model? (%body-model-id req-body))
        (%inject-adaptive-thinking req-body level)
        (%inject-budget-thinking req-body level)
      ) ;if
    ) ;define

    (define (%inject-budget-thinking req-body level)
      (let ((budget (anthropic-thinking-level->budget level)))
        (if budget
          (let* ((th-val (list (cons "type" "enabled") (cons "budget_tokens" budget)))
                 (with-th (cons (cons "thinking" th-val) req-body))
                 (cur-max (or (assoc "max_tokens" req-body) (assoc 'max_tokens req-body)))
                 (eff-max (if cur-max (cdr cur-max) 8192))
                ) ;
            (if (<= eff-max budget)
              (map
                (lambda (pair)
                  (if (or (equal? (car pair) "max_tokens") (eq? (car pair) 'max_tokens))
                    (cons (car pair) (+ budget 4096))
                    pair
                  ) ;if
                ) ;lambda
                with-th
              ) ;map
              with-th
            ) ;if
          ) ;let*
          req-body
        ) ;if
      ) ;let
    ) ;define

    (define (anthropic-start-thinking-block stream-state idx on-ev)
      (anthropic-stream-state-current-thinking-set! stream-state
        (%make-anthropic-thinking-state "" "" idx)
      ) ;anthropic-stream-state-current-thinking-set!
      (anthropic-stream-state-last-text-set! stream-state #f)
      (when (procedure? on-ev)
        (on-ev (list (cons 'type "thinking_start")
                 (cons 'contentIndex idx)
                 (cons 'usage (anthropic-stream-state-usage stream-state))
               ) ;list
        ) ;on-ev
      ) ;when
    ) ;define

    (define (anthropic-handle-thinking-delta delta idx stream-state on-ev)
      (let* ((th (cdr (or (assoc "thinking" delta) '(#f . ""))))
             (sig (cdr (or (assoc "signature" delta) '(#f . ""))))
             (cur-th (anthropic-stream-state-current-thinking stream-state))
            ) ;
        (when cur-th
          (let* ((prev-text (%anthropic-thinking-state-thinking cur-th))
                 (prev-sig (%anthropic-thinking-state-signature cur-th))
                 (new-text (string-append prev-text th))
                 (new-sig (string-append prev-sig sig))
                 (upd (%make-anthropic-thinking-state new-text new-sig idx))
                ) ;
            (anthropic-stream-state-current-thinking-set! stream-state upd)
            (when (and (procedure? on-ev) (not (string-null? th)))
              (on-ev (list (cons 'type "thinking_delta")
                       (cons 'contentIndex idx)
                       (cons 'delta th)
                       (cons 'usage (anthropic-stream-state-usage stream-state))
                     ) ;list
              ) ;on-ev
            ) ;when
          ) ;let*
        ) ;when
      ) ;let*
    ) ;define

    (define (anthropic-finish-thinking-block stream-state on-ev)
      (let* ((cur-th (anthropic-stream-state-current-thinking stream-state))
             (blocks (anthropic-stream-state-blocks stream-state))
             (usage (anthropic-stream-state-usage stream-state))
            ) ;
        (when cur-th
          (let* ((th (%anthropic-thinking-state-thinking cur-th))
                 (sig (%anthropic-thinking-state-signature cur-th))
                 (eff-sig (if (string-null? sig) #f sig))
                 (blk (pi-thinking-block th eff-sig))
                ) ;
            (anthropic-stream-state-blocks-set! stream-state (append blocks (list blk)))
            (anthropic-stream-state-current-thinking-set! stream-state #f)
            (when (procedure? on-ev)
              (on-ev (list (cons 'type "thinking_end")
                       (cons 'contentIndex (%anthropic-thinking-state-index cur-th))
                       (cons 'content th)
                       (cons 'usage usage)
                     ) ;list
              ) ;on-ev
            ) ;when
          ) ;let*
        ) ;when
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
