;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; Agent 主循环的 all-in-one 请求适配：伪模型 -> 健康节点 -> 真实协议。
(define-library (liii pi code all-in-one request)
  (import (scheme base)
    (liii list)
    (liii path)
    (liii pi ai dispatch)
    (liii pi ai message)
    (liii pi code config)
    (liii pi code all-in-one chain)
    (liii pi code all-in-one loader)
    (liii pi code all-in-one types)
  ) ;import
  (export pi-all-in-one-request-stream pi-all-in-one-exhausted-message)
  (begin

    (define (pi-all-in-one-exhausted-message)
      (pi-assistant-message '()
        "error"
        "All nodes in the all-in-one chain failed probe"
      ) ;pi-assistant-message
    ) ;define

    (define (%node-config config provider model-id thinking-level)
      (let* ((parsed (list (cons 'provider provider)
                       (cons 'model model-id)
                       (cons 'thinking-level thinking-level)
                       (cons 'no-tools #t)
                     ) ;list
             ) ;parsed
             (dir-str (path->string (pi-cfg-config-dir config)))
             (node-cfg (resolve-pi-config parsed (list (cons "PI_CODING_AGENT_DIR" dir-str)))
             ) ;node-cfg
             (keys '(provider provider-api model-id api-key model-meta
                      models-store thinking-level proxy-url config-dir)
             ) ;keys
             (pairs
               (map
                 (lambda (key) (cons key (cdr (assoc key node-cfg))))
                 keys
               ) ;map
             ) ;pairs
            ) ;
        (append pairs (filter (lambda (p) (not (memq (car p) keys))) config))
      ) ;let*
    ) ;define

    (define (%execute-node config system-prompt tools on-event timeout request-fn)
      (lambda (provider model-id thinking-level messages)
        (let ((node-config (%node-config config provider model-id thinking-level))
              (request (or request-fn pi-request-provider-stream))
             ) ;
          (request provider node-config messages system-prompt tools on-event timeout)
        ) ;let
      ) ;lambda
    ) ;define

    ;; 预检后解析实际节点的凭据、协议与模型元数据；普通供应商不经过此入口。
    (define* (pi-all-in-one-request-stream provider
               config
               messages
               system-prompt
               tools
               on-event
               timeout
               (probe-fn #f)
               (request-fn #f)
             ) ;pi-all-in-one-request-stream
      (let* ((dir (pi-cfg-config-dir config))
             (aio-cfg (all-in-one-load-config dir))
             (chain-id (cdr (assoc 'model-id config)))
             (chain
               (and aio-cfg
                 chain-id
                 (find (lambda (c) (equal? (chain-def-id c) chain-id))
                   (all-in-one-cfg-chains aio-cfg)
                 ) ;find
               ) ;and
             ) ;chain
            ) ;
        (if (not chain)
          (pi-all-in-one-exhausted-message)
          (let ((result (pi-chain-execute chain
                          messages
                          (%execute-node config system-prompt tools on-event timeout request-fn)
                          aio-cfg
                          dir
                          #f
                          probe-fn
                        ) ;pi-chain-execute
                ) ;result
               ) ;
            (if (eq? result 'all-nodes-exhausted) (pi-all-in-one-exhausted-message) result)
          ) ;let
        ) ;if
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
