;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api google-generative-ai usage)
  (import (scheme base) (scheme char) (liii string))
  (export gemini-map-finish-reason
    gemini-retriable-finish-reason?
    gemini-map-usage
  ) ;export
  (begin

    (define (gemini-map-finish-reason finish-reason has-function-call?)
      (cond ((or (equal? finish-reason "STOP") (eq? finish-reason 'STOP))
             (if has-function-call? "toolUse" "stop")
            ) ;
            ((or (equal? finish-reason "MAX_TOKENS") (eq? finish-reason 'MAX_TOKENS))
             "length"
            ) ;
            (else "error")
      ) ;cond
    ) ;define

    ;; HTTP 200 但 finishReason 表示生成失败时，是否值得原样重发请求。
    ;; MALFORMED_RESPONSE 是服务端生成中途的偶发内部校验失败，重发通常即可
    ;; 成功（实测同一会话手动继续即恢复），因此仅将这一项纳入重试；
    ;; SAFETY/RECITATION 等属于内容判定类终态，重发同样结果，不重试。
    (define (gemini-retriable-finish-reason? finish-reason)
      (and (string? finish-reason)
        (string=? (string-upcase finish-reason) "MALFORMED_RESPONSE")
      ) ;and
    ) ;define

    (define (%usage-field usage-meta key sym-key)
      (if (not (pair? usage-meta))
        0
        (let ((pair (or (assoc key usage-meta) (assoc sym-key usage-meta))))
          (if pair (cdr pair) 0)
        ) ;let
      ) ;if
    ) ;define

    (define (gemini-map-usage usage-meta)
      (let* ((prompt-tokens (%usage-field usage-meta "promptTokenCount" 'promptTokenCount))
             (cand-tokens (%usage-field usage-meta "candidatesTokenCount" 'candidatesTokenCount)
             ) ;cand-tokens
             (thought-tokens (%usage-field usage-meta "thoughtsTokenCount" 'thoughtsTokenCount)
             ) ;thought-tokens
             (cached-tokens (%usage-field usage-meta "cachedContentTokenCount" 'cachedContentTokenCount)
             ) ;cached-tokens
             (total-tokens (%usage-field usage-meta "totalTokenCount" 'totalTokenCount))
             (in-tokens (- prompt-tokens cached-tokens))
             (out-tokens (+ cand-tokens thought-tokens))
            ) ;
        (list (cons 'input in-tokens)
          (cons 'output out-tokens)
          (cons 'cacheRead cached-tokens)
          (cons 'cacheWrite 0)
          (cons 'reasoning thought-tokens)
          (cons 'totalTokens total-tokens)
        ) ;list
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
