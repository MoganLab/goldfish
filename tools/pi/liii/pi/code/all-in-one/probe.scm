;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 进程内轻量预检：使用 pi-request-provider 发起单次非流式探测请求。
;; 避免子进程开销，不读写主会话，不加载工具与技能，失败统一追加到按天日志。
(define-library (liii pi code all-in-one probe)
  (import (scheme base)
    (scheme char)
    (liii list)
    (liii path)
    (liii string)
    (liii pi ai dispatch)
    (liii pi ai message)
    (liii pi ai config record)
    (liii pi agent tools)
    (liii pi code all-in-one alarm)
    (liii pi code all-in-one types)
    (liii pi code config)
  ) ;import
  (export pi-probe-node pi-probe-prompt pi-probe-error-code)
  (begin

    (define pi-probe-prompt "ping")

    (define (%http-code message start)
      (let loop
        ((i start) (digits '()))
        (if (and (< i (string-length message)) (char-numeric? (string-ref message i)))
          (loop (+ i 1) (cons (string-ref message i) digits))
          (if (pair? digits) (string->number (list->string (reverse digits))) 'error)
        ) ;if
      ) ;let
    ) ;define

    ;; 从错误文本或响应中提取 HTTP 状态码；无状态码时保留超时类别。
    (define (pi-probe-error-code message)
      (cond ((number? message) message)
            ((symbol? message) message)
            ((string? message)
             (let ((pos (string-position "HTTP " message)))
               (cond (pos (%http-code message (+ pos 5)))
                     ((or (string-contains? (string-downcase message) "timeout")
                        (string-contains? (string-downcase message) "timed out")
                      ) ;or
                      'timeout
                     ) ;
                     (else 'error)
               ) ;cond
             ) ;let
            ) ;
            (else 'error)
      ) ;cond
    ) ;define

    (define (%failure code message)
      (list code message)
    ) ;define

    ;; 探活不给思考预算：只保留极小的输出上限，避免模型把时间花在长思考上，
    ;; 同时让响应只返回结构化状态（stop/length 也算链路健康）。
    (define %probe-max-tokens 16)

    (define (%probe-node-config node dir-str)
      (catch #t
        (lambda ()
          (let* ((parsed (list (cons 'provider (chain-node-provider node))
                           (cons 'model (chain-node-model node))
                           (cons 'thinking-level (chain-node-thinking-level node))
                           (cons 'no-tools #t)
                         ) ;list
                 ) ;parsed
                 (cfg (resolve-pi-config parsed (list (cons "PI_CODING_AGENT_DIR" dir-str))))
                 (meta (cdr (or (assoc 'model-meta cfg) '(#f . #f))))
                 (base-url
                   (cond ((pi-model-metadata? meta) (pi-model-metadata-base-url meta))
                         ((pair? meta) (cdr (or (assoc 'base-url meta) '(#f
                                                                         . #f))))
                         (else #f)
                   ) ;cond
                 ) ;base-url
                 (probe-meta
                   (pi-model-metadata-from-alist
                     (append (list (cons 'base-url (or base-url "")) (cons 'max-tokens %probe-max-tokens))
                       (if (pi-model-metadata? meta)
                         (list (cons 'id (pi-model-metadata-id meta))
                           (cons 'name (pi-model-metadata-name meta))
                           (cons 'provider (pi-model-metadata-provider meta))
                           (cons 'context-window (pi-model-metadata-context-window meta))
                           (cons 'matched? (pi-model-metadata-matched? meta))
                         ) ;list
                         (if (pair? meta) meta '())
                       ) ;if
                     ) ;append
                   ) ;pi-model-metadata-from-alist
                 ) ;probe-meta
                ) ;
            (cons (cons 'model-meta probe-meta)
              (filter
                (lambda (p) (not (eq? (car p) 'model-meta)))
                cfg
              ) ;filter
            ) ;cons
          ) ;let*
        ) ;lambda
        (lambda _
          (list (cons 'provider (chain-node-provider node))
            (cons 'model-id (chain-node-model node))
            (cons 'thinking-level (chain-node-thinking-level node))
            (cons 'model-meta
              (pi-model-metadata-from-alist (list (cons 'id (chain-node-model node))
                                              (cons 'name (chain-node-model node))
                                              (cons 'provider (chain-node-provider node))
                                              (cons 'base-url "")
                                              (cons 'max-tokens %probe-max-tokens)
                                            ) ;list
              ) ;pi-model-metadata-from-alist
            ) ;cons
            (cons 'config-dir (path dir-str))
          ) ;list
        ) ;lambda
      ) ;catch
    ) ;define

    (define (%nonempty-response-text resp)
      (let ((text (extract-assistant-text resp)))
        (and (string? text) (not (string-null? (string-trim-both text))))
      ) ;let
    ) ;define

    (define (%network-error-code message)
      (if
        (and (string? message)
          (or (string-contains? (string-downcase message) "request failed:")
            (string-contains? (string-downcase message) "network error")
          ) ;or
        ) ;and
        (if (or (string-contains? (string-downcase message) "timeout")
              (string-contains? (string-downcase message) "timed out")
            ) ;or
          'timeout
          'network
        ) ;if
        'error
      ) ;if
    ) ;define

    (define (%assistant-probe-failure resp)
      (let ((stop (pi-assistant-message-stop-reason resp))
            (err-msg (pi-assistant-message-error-message resp))
           ) ;
        (cond
         ((equal? stop "error")
          (%failure
            (let ((code (pi-probe-error-code (or err-msg "error"))))
              (if (and (or (eqv? code 0) (eq? code 'error)) (string? err-msg))
                (%network-error-code err-msg)
                code
              ) ;if
            ) ;let
            (or err-msg "probe request error")
          ) ;%failure
         ) ;
         ((equal? stop "aborted")
          (%failure 'timeout
            (if (and (string? err-msg) (not (string-null? err-msg)))
              err-msg
              "probe request timeout"
            ) ;if
          ) ;%failure
         ) ;
         ;; 探活只验证链路可用：非 error/aborted 的结构化 assistant 即健康，
         ;; 即使 max_tokens 截断导致正文为空（stop=length）。
         (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (%probe-response-failure resp)
      (cond ((pi-assistant-message? resp) (%assistant-probe-failure resp))
            ((and (string? resp) (not (string-null? (string-trim-both resp)))) #f)
            ((string? resp) (%failure 'empty-response "probe returned no text"))
            ((boolean? resp) (if resp #f (%failure 'error "probe returned false")))
            ((and (pair? resp) (%nonempty-response-text resp)) #f)
            ((pair? resp) (%failure 'empty-response "probe returned no text"))
            (else (%failure 'error "probe returned unexpected response"))
      ) ;cond
    ) ;define

    (define (%request-exception-failure tag args)
      (let* ((detail
               (if (pair? args)
                 (let ((d (car args)))
                   (if (string? d) d (object->string d))
                 ) ;let
                 (object->string tag)
               ) ;if
             ) ;detail
             (code (pi-probe-error-code detail))
            ) ;
        (%failure (if (eq? code 'error) 'exception code) detail)
      ) ;let*
    ) ;define

    ;; request-fn 接收 provider、config、messages、sys-prompt、tools、timeout、retry，
    ;; 缺省为 (liii pi ai dispatch) 的 pi-request-provider。
    ;; 正常返回非空文本时返回 #t；任何错误统一写 FAIL 日志并返回 #f。
    (define* (pi-probe-node node (timeout-ms 8000) (config-dir #f) (request-fn #f))
      (let* ((provider (chain-node-provider node))
             (model (chain-node-model node))
             (dir
               (if (string? config-dir)
                 config-dir
                 (path->string (or config-dir (gf-get-config-dir)))
               ) ;if
             ) ;dir
             (req (or request-fn pi-request-provider))
             (failure
               (catch #t
                 (lambda ()
                   (let* ((node-cfg (%probe-node-config node dir))
                          (msgs (list (pi-user-message pi-probe-prompt)))
                          (timeout-sec (/ timeout-ms 1000))
                          ;; 探活必须单次、快速失败，不在同一节点内自动重试。
                          (resp (req provider node-cfg msgs #f '() timeout-sec '((enabled
                                                                                   . #f))))
                         ) ;
                     (%probe-response-failure resp)
                   ) ;let*
                 ) ;lambda
                 (lambda (tag . args) (%request-exception-failure tag args))
               ) ;catch
             ) ;failure
            ) ;
        (when failure
          (pi-alarm-log! "FAIL" provider model (car failure) (cadr failure) dir)
        ) ;when
        (not failure)
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
