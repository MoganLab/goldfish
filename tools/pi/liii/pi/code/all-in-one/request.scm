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
    (liii pi ai config record)
    (liii pi code config)
    (liii pi code all-in-one chain)
    (liii pi code all-in-one loader)
    (liii pi code all-in-one probe)
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

    ;; 当前实际节点上报给 TUI：emit 为 agent 事件回调（来自 cfg），
    ;; 与请求层只接单参数的流式 on-event 区分开。
    (define (%emit-active-node cfg provider model-id thinking-level node-config)
      (let* ((emit (and (pair? cfg) (assoc 'emit cfg)))
             (meta (cdr (or (assoc 'model-meta node-config) '(#f . #f))))
             (window
               (cond ((pi-model-metadata? meta) (pi-model-metadata-context-window meta))
                     ((pair? meta) (cdr (or (assoc 'context-window meta) '(#f
                                                                           . #f))))
                     (else #f)
               ) ;cond
             ) ;window
            ) ;
        (when (and emit (procedure? (cdr emit)))
          ((cdr emit) 'active_node (list provider model-id thinking-level window))
        ) ;when
      ) ;let*
    ) ;define

    (define (%request-failed? response)
      (and (pi-assistant-message? response)
        (equal? (pi-assistant-message-stop-reason response) "error")
      ) ;and
    ) ;define

    (define (%execute-node config system-prompt tools on-event timeout request-fn)
      (lambda (provider model-id thinking-level messages)
        (let ((node-config (%node-config config provider model-id thinking-level))
              (request (or request-fn pi-request-provider-stream))
             ) ;
          ;; 交接前把实际节点告知前台，供页脚展示
          (%emit-active-node config provider model-id thinking-level node-config)
          (request provider node-config messages system-prompt tools on-event timeout)
        ) ;let
      ) ;lambda
    ) ;define

    (define (%probe-node config-dir)
      (lambda (node timeout-ms dir request-fn)
        (let ((dir-str (if (string? dir) dir (path->string dir))))
          (pi-probe-node node
            timeout-ms
            dir-str
            (lambda (provider node-cfg msgs sys-prompt tools probe-timeout retry)
              (let ((config (%node-config config-dir
                              provider
                              (chain-node-model node)
                              (chain-node-thinking-level node)
                            ) ;%node-config
                    ) ;config
                   ) ;
                (pi-request-provider provider config msgs sys-prompt tools probe-timeout retry)
              ) ;let
            ) ;lambda
          ) ;pi-probe-node
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
          (let ((execute (%execute-node config system-prompt tools on-event timeout request-fn))
               ) ;
            (let loop
              ((attempts 0) (skip-index #f))
              (let ((result (pi-chain-execute chain
                              messages
                              execute
                              aio-cfg
                              dir
                              #f
                              (or probe-fn (%probe-node config))
                              skip-index
                            ) ;pi-chain-execute
                    ) ;result
                   ) ;
                ;; 探活成功后真实请求仍可能失败：清掉粘性并从链头重选下一节点。
                (if
                  (and (%request-failed? result)
                    (< attempts (- (length (chain-def-nodes chain)) 1))
                  ) ;and
                  (let ((failed-index (pi-chain-current-sticky-index chain-id)))
                    (pi-chain-clear-sticky-node! chain-id)
                    (loop (+ attempts 1) failed-index)
                  ) ;let
                  (if (eq? result 'all-nodes-exhausted) (pi-all-in-one-exhausted-message) result)
                ) ;if
              ) ;let
            ) ;let
          ) ;let
        ) ;if
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
