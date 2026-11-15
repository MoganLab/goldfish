;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai message json)
  (import (scheme base) (liii list) (liii pi ai message record))
  (export json->pi-block json->pi-message)
  (begin

    (define (%json-dict-pair obj str-key sym-key)
      (let loop
        ((rest obj))
        (cond ((not (pair? rest)) #f)
              ((and (pair? (car rest))
                 (let ((k (car (car rest))))
                   (or (equal? k str-key) (equal? k sym-key))
                 ) ;let
               ) ;and
               (car rest)
              ) ;
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%json-dict-ref obj key default-val)
      (if (not (pair? obj))
        default-val
        (let* ((str-key (if (symbol? key) (symbol->string key) key))
               (sym-key (if (string? key) (string->symbol key) key))
               (pair (%json-dict-pair obj str-key sym-key))
              ) ;
          (if pair (let ((v (cdr pair))) (if (eq? v 'null) default-val v)) default-val)
        ) ;let*
      ) ;if
    ) ;define

    (define (%normalize-json-bool val)
      (case val
       ((true) #t)
       ((false) #f)
       (else (if val #t #f))
      ) ;case
    ) ;define

    (define (%normalize-tool-args args)
      (if (or (not args) (null? args) (eq? args 'null) (equal? args '(())))
        '(())
        args
      ) ;if
    ) ;define

    (define (%json-key->string k)
      (if (symbol? k) (symbol->string k) k)
    ) ;define

    (define (json->pi-block obj)
      (cond ((pi-block? obj) obj)
            ((not (pair? obj)) #f)
            (else
              (let ((btype (%json-key->string (%json-dict-ref obj "type" #f))))
                (cond ((equal? btype "text")
                       (pi-text-block (%json-dict-ref obj "text" "")
                         (%json-dict-ref obj "textSignature" #f)
                       ) ;pi-text-block
                      ) ;
                      ((equal? btype "thinking")
                       (pi-thinking-block (%json-dict-ref obj "thinking" "")
                         (%json-dict-ref obj "thinkingSignature" #f)
                       ) ;pi-thinking-block
                      ) ;
                      ((equal? btype "toolCall")
                       (pi-tool-call-block (%json-dict-ref obj "id" "")
                         (%json-dict-ref obj "name" "")
                         (%normalize-tool-args (%json-dict-ref obj "arguments" '(())))
                         (%json-dict-ref obj "thoughtSignature" #f)
                       ) ;pi-tool-call-block
                      ) ;
                      ((equal? btype "image")
                       (let ((mime (or (%json-dict-ref obj "mimeType" #f)
                                     (%json-dict-ref obj "media_type" "image/png")
                                   ) ;or
                             ) ;mime
                            ) ;
                         (pi-image-block (%json-dict-ref obj "data" "") mime)
                       ) ;let
                      ) ;
                      (else #f)
                ) ;cond
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    (define (%convert-block-elem x)
      (or (json->pi-block x) x)
    ) ;define

    (define (%convert-content c)
      (cond ((vector? c) (vector-map %convert-block-elem c))
            ((and (pair? c) (not (pair? (car c)))) c)
            ((pair? c) (map %convert-block-elem c))
            (else c)
      ) ;cond
    ) ;define

    (define (json->pi-message obj)
      (cond ((pi-message? obj) obj)
            ((not (pair? obj)) #f)
            (else
              (let ((role (%json-key->string (%json-dict-ref obj "role" #f))))
                (cond
                 ((equal? role "user")
                  (pi-user-message (%convert-content (%json-dict-ref obj "content" ""))
                    (%json-dict-ref obj "timestamp" #f)
                  ) ;pi-user-message
                 ) ;
                 ((equal? role "assistant")
                  (pi-assistant-message (%convert-content (%json-dict-ref obj "content" #()))
                    :stop-reason   (%json-dict-ref obj "stopReason" "stop")
                    :error-message (%json-dict-ref obj "errorMessage" #f)
                    :usage         (%json-dict-ref obj "usage" #f)
                    :timestamp     (%json-dict-ref obj "timestamp" #f)
                    :provider      (%json-dict-ref obj "provider" #f)
                    :model         (%json-dict-ref obj "model" #f)
                    :api           (%json-dict-ref obj "api" #f)
                  ) ;pi-assistant-message
                 ) ;
                 ((equal? role "toolResult")
                  (pi-tool-result-message (%json-dict-ref obj "toolCallId" "")
                    (%json-dict-ref obj "toolName" "")
                    (%convert-content (%json-dict-ref obj "content" #()))
                    (%normalize-json-bool (%json-dict-ref obj "isError" #f))
                    (%json-dict-ref obj "timestamp" #f)
                  ) ;pi-tool-result-message
                 ) ;
                 (else #f)
                ) ;cond
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
