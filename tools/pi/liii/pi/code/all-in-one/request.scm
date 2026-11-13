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
    (liii string)
    (liii pi ai dispatch)
    (liii pi ai message)
    (liii pi ai config record)
    (liii pi ai thinking)
    (liii pi code config)
    (liii pi code modes output)
    (liii pi code all-in-one chain)
    (liii pi code all-in-one loader)
    (liii pi code all-in-one probe)
    (liii pi code all-in-one provider)
    (liii pi code all-in-one recovery)
    (liii pi code all-in-one thinking)
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

    ;; 从已解析的节点配置中挑出请求层需要的键，节点等级一律以节点默认值为准；
    ;; 主 cfg 的额外键（emit、get-steering-messages、now-fn 等）追加在末尾
    ;; supports-thinking 必须取节点真实模型的能力，否则会被主 cfg 中伪模型的值
    ;; 覆盖，导致页脚按伪模型判断是否渲染等级后缀
    (define (%node-request-keys)
      '(provider provider-api model-id api-key model-meta models-store
         thinking-level supports-thinking proxy-url config-dir
         thinking-level-explicit?)
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
             (keys (%node-request-keys))
             (pairs (map (lambda (key) (%node-config-pair key node-cfg thinking-level)) keys)
             ) ;pairs
            ) ;
        (append pairs (filter (lambda (p) (not (memq (car p) keys))) config))
      ) ;let*
    ) ;define

    ;; 节点级请求的等级与显式标记不被节点解析结果覆盖
    (define (%node-config-pair key node-cfg thinking-level)
      (cond ((eq? key 'thinking-level) (cons 'thinking-level thinking-level))
            ((eq? key 'thinking-level-explicit?) (cons key #f))
            (else (cons key (cdr (assoc key node-cfg))))
      ) ;cond
    ) ;define

    ;; 当前实际节点上报给 TUI：emit 为 agent 事件回调（来自 cfg），
    ;; 与请求层只接单参数的流式 on-event 区分开。
    (define (%node-context-window node-config)
      (let ((meta (cdr (or (assoc 'model-meta node-config) '(#f . #f)))))
        (cond ((pi-model-metadata? meta) (pi-model-metadata-context-window meta))
              ((pair? meta) (cdr (or (assoc 'context-window meta) '(#f . #f))))
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (%emit-active-node cfg provider model-id thinking-level node-config)
      (let* ((emit (and (pair? cfg) (assoc 'emit cfg)))
             (window (%node-context-window node-config))
             (supports? (cdr (or (assoc 'supports-thinking node-config) '(#f
                                                                          . #t))))
            ) ;
        (when (and emit (procedure? (cdr emit)))
          ((cdr emit)
           'active_node
           (list provider model-id thinking-level window supports?)
          ) ;
        ) ;when
      ) ;let*
    ) ;define

    (define (%request-failed? response)
      (and (pi-assistant-message? response)
        (equal? (pi-assistant-message-stop-reason response) "error")
      ) ;and
    ) ;define

    ;; 用户是否本次显式指定了思考等级（CLI /thinking / Shift+Tab）
    (define (%cfg-ref cfg key default-val)
      (if (not (pair? cfg))
        default-val
        (let ((p (assoc key cfg)))
          (if p (cdr p) default-val)
        ) ;let
      ) ;if
    ) ;define

    (define (%node-effective-thinking-level config node-default-level provider model-id)
      (let* ((explicit? (%cfg-ref config 'thinking-level-explicit? #f))
             (explicit-tl (if explicit? (%cfg-ref config 'thinking-level #f) #f))
             (models-store (%cfg-ref config 'models-store #f))
            ) ;
        (resolve-node-thinking-level explicit-tl node-default-level models-store
          provider model-id
        ) ;resolve-node-thinking-level
      ) ;let*
    ) ;define

    (define (%execute-node config system-prompt tools on-event timeout
              request-fn chain-id last-exec-box
            ) ;%execute-node
      (lambda (provider model-id thinking-level messages)
        (let* ((eff-tl (%node-effective-thinking-level config thinking-level provider model-id)
               ) ;eff-tl
               (_ (when (vector? last-exec-box)
                    (vector-set! last-exec-box 0 (list provider model-id))
                  ) ;when
               ) ;_
               (explicit? (%cfg-ref config 'thinking-level-explicit? #f))
               (_ (when explicit?
                    (pi-log-thinking-clamp! config
                      chain-id
                      provider
                      model-id
                      (%cfg-ref config 'thinking-level #f)
                      eff-tl
                    ) ;pi-log-thinking-clamp!
                  ) ;when
               ) ;_
               (node-config (%node-config config provider model-id eff-tl))
               (request (or request-fn pi-request-provider-stream))
              ) ;
          ;; 交接前把实际节点告知前台，供页脚展示
          (%emit-active-node config provider model-id eff-tl node-config)
          (request provider node-config messages system-prompt tools on-event timeout)
        ) ;let*
      ) ;lambda
    ) ;define

    (define (%find-node-index nodes provider model-id)
      (let loop
        ((rest nodes) (i 0))
        (cond ((null? rest) #f)
              ((and (equal? (chain-node-provider (car rest)) provider)
                 (equal? (chain-node-model (car rest)) model-id)
               ) ;and
               i
              ) ;
              (else (loop (cdr rest) (+ i 1)))
        ) ;cond
      ) ;let
    ) ;define

    ;; 探活配置：继承节点配置但覆盖 model-meta 的 max-tokens
    (define (%probe-config base-cfg)
      (let* ((raw-meta (cdr (or (assoc 'model-meta base-cfg) '(#f . #f))))
             (probe-meta (pi-probe-max-tokens-metadata raw-meta))
            ) ;
        (cons (cons 'model-meta probe-meta)
          (filter
            (lambda (pair) (not (eq? (car pair) 'model-meta)))
            base-cfg
          ) ;filter
        ) ;cons
      ) ;let*
    ) ;define

    ;; 探活与业务请求共用同一套节点等级收敛，避免探活通过而正式请求 HTTP 400
    (define (%probe-node cfg)
      (lambda (node timeout-ms dir request-fn)
        (let* ((dir-str (if (string? dir) dir (path->string dir)))
               (provider (chain-node-provider node))
               (model-id (chain-node-model node))
               (eff-tl (%node-effective-thinking-level cfg
                         (chain-node-thinking-level node)
                         provider
                         model-id
                       ) ;%node-effective-thinking-level
               ) ;eff-tl
              ) ;
          (pi-probe-node node
            timeout-ms
            dir-str
            (lambda (p node-cfg msgs sys-prompt tools probe-timeout retry)
              (pi-request-provider p
                (%probe-config (%node-config cfg p model-id eff-tl))
                msgs
                sys-prompt
                tools
                probe-timeout
                (if retry retry (pi-probe-retry-config (%cfg-ref cfg 'settings #f)))
              ) ;pi-request-provider
            ) ;lambda
          ) ;pi-probe-node
        ) ;let*
      ) ;lambda
    ) ;define

    ;; 粘性节点下的一次完整链式尝试；真实请求失败时记录故障并顺延到下一节点
    (define (%run-chain-turn chain chain-id messages skip-idx execute aio-cfg
              dir probe-adapter config last-exec-box
            ) ;%run-chain-turn
      (let loop
        ((attempts 0) (skip skip-idx))
        (let ((result (pi-chain-execute chain
                        messages
                        execute
                        aio-cfg
                        dir
                        #f
                        probe-adapter
                        skip
                        (%cfg-ref config 'now-fn #f)
                      ) ;pi-chain-execute
              ) ;result
             ) ;
          (if
            (and (%request-failed? result)
              (< attempts (- (length (chain-def-nodes chain)) 1))
            ) ;and
            (let* ((last-exec (and (vector? last-exec-box) (vector-ref last-exec-box 0)))
                   (nodes (chain-def-nodes chain))
                   (exec-idx (and (pair? last-exec)
                               (%find-node-index nodes (car last-exec) (cadr last-exec))
                             ) ;and
                   ) ;exec-idx
                   (failed-index (or exec-idx
                                   (pi-chain-current-sticky-index chain-id)
                                   (and (integer? skip) skip)
                                   0
                                 ) ;or
                   ) ;failed-index
                  ) ;
              (%record-and-skip chain chain-id failed-index)
              (when (vector? last-exec-box)
                (vector-set! last-exec-box 0 #f)
              ) ;when
              (loop (+ attempts 1) failed-index)
            ) ;let*
            result
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 记录失败节点的时间戳并清掉粘性，使下一轮能从链头重新选择
    (define (%record-and-skip chain chain-id failed-index)
      (let* ((nodes (chain-def-nodes chain))
             (failed-node (and (integer? failed-index)
                            (>= failed-index 0)
                            (< failed-index (length nodes))
                            (list-ref nodes failed-index)
                          ) ;and
             ) ;failed-node
            ) ;
        (when failed-node
          (pi-chain-record-node-failure! failed-node)
        ) ;when
        (pi-chain-clear-sticky-node! chain-id)
      ) ;let*
    ) ;define

    ;; 全链枯竭时构造 recovery 选项：只传递时钟、取消、轮询与事件回调
    (define (%recovery-options config chain-id dir)
      (let* ((poll-steering (%cfg-ref config 'get-steering-messages (lambda () '())))
             (emit (%cfg-ref config 'emit (lambda args #f)))
             (base (list (cons 'chain-id chain-id)
                     (cons 'config-dir dir)
                     (cons 'poll-steering poll-steering)
                     (cons 'emit emit)
                   ) ;list
             ) ;base
            ) ;
        (fold
          (lambda (k acc)
            (let ((entry (assoc k config)))
              (if entry (cons entry acc) acc)
            ) ;let
          ) ;lambda
          base
          '(now-fn sleep-fn aborted? step-ms)
        ) ;fold
      ) ;let*
    ) ;define

    (define (%resolve-chain aio-cfg chain-id)
      (let* ((user-chains (if (all-in-one-cfg? aio-cfg) (all-in-one-cfg-chains aio-cfg) '()))
             (default-chains (all-in-one-default-chains))
             (all-chains (append user-chains default-chains))
            ) ;
        (and chain-id
          (or
            (find (lambda (c) (equal? (chain-def-id c) chain-id)) all-chains)
            (and (member chain-id '("coding-deep" "coding-high-level"))
              (find (lambda (c) (equal? (chain-def-id c) "coding-high-quality")) all-chains)
            ) ;and
            (and (equal? chain-id "coding-fast-cheap")
              (find (lambda (c) (equal? (chain-def-id c) "coding-fast")) all-chains)
            ) ;and
          ) ;or
        ) ;and
      ) ;let*
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
             (aio-cfg (or (all-in-one-load-config dir)
                        (make-all-in-one-cfg #t 3600000 60000 8000 (all-in-one-default-chains))
                      ) ;or
             ) ;aio-cfg
             (raw-model-id (cdr (assoc 'model-id config)))
             (chain (%resolve-chain aio-cfg raw-model-id))
            ) ;
        (if (not chain)
          (pi-all-in-one-exhausted-message)
          (let* ((canonical-id (chain-def-id chain))
                 (last-exec-box (vector #f))
                 (execute (%execute-node config system-prompt tools on-event
                            timeout request-fn canonical-id last-exec-box
                          ) ;%execute-node
                 ) ;execute
                 (probe-adapter (or probe-fn (%probe-node config)))
                 (run-turn (lambda (msgs skip)
                             (%run-chain-turn chain canonical-id msgs skip
                               execute aio-cfg dir probe-adapter config
                               last-exec-box
                             ) ;%run-chain-turn
                           ) ;lambda
                 ) ;run-turn
                 (initial (run-turn messages #f))
                ) ;
            (if (not (eq? initial 'all-nodes-exhausted))
              initial
              ;; 全链枯竭：接入 recovery 守护与到期重试（原上下文续跑）
              (pi-recovery-run (lambda () (run-turn messages #f))
                aio-cfg
                (%recovery-options config canonical-id dir)
              ) ;pi-recovery-run
            ) ;if
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
