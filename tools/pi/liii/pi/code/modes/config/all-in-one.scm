;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; `gf pi config` 的 all-in-one 子标签页数据与详情装配：
;;   读取 all-in-one.json（缺失或无效时回退内置默认链），
;;   结合 pi-config-state 中的 provider 启用状态、key 来源与代理配置，
;;   生成链条列表、节点列表、右列详情行与只读文件视图。
(define-library (liii pi code modes config all-in-one)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii list)
    (liii string)
    (liii path)
    (liii pi code all-in-one types)
    (liii pi code all-in-one loader)
    (liii pi code all-in-one defaults)
    (liii pi code modes config state)
    (liii pi code modes config paths)
    (liii pi code modes config aio-writes)
  ) ;import
  (export pi-config-aio-state pi-config-aio-chains pi-config-aio-chain-nodes
    pi-config-aio-chain-detail-rows pi-config-aio-node-detail-rows
    pi-config-aio-file-view pi-config-aio-status-text
    pi-config-aio-enabled-targets pi-config-aio-add-chain pi-config-aio-add-slot
  ) ;export
  (begin

    (define (%ref entry key)
      (if (pair? entry) (cdr (or (assoc key entry) (cons #f #f))) #f)
    ) ;define

    (define (%format-duration ms)
      (cond ((not (number? ms)) "—")
            ((and (>= ms 1000) (= (modulo ms 1000) 0))
             (string-append (number->string (quotient ms 1000)) "s")
            ) ;
            (else (string-append (number->string ms) "ms"))
      ) ;cond
    ) ;define

    (define (%build-node st idx node)
      (let* ((p-id (chain-node-provider node)) (p-entry (pi-config-provider st p-id)))
        (list (cons "index" idx)
          (cons "provider" p-id)
          (cons "model" (chain-node-model node))
          (cons "thinking" (chain-node-thinking-level node))
          (cons "enabled" (if p-entry (pi-config-provider-enabled? p-entry) #f))
          (cons "keySource" (and p-entry (pi-config-provider-key-source p-entry)))
          (cons "proxy" (and p-entry (pi-config-provider-proxy p-entry)))
        ) ;list
      ) ;let*
    ) ;define

    (define (%build-chain st chain)
      (let* ((c-id (chain-def-id chain))
             (def-p (pi-config-state-default-provider st))
             (def-m (pi-config-state-default-model st))
             (default? (and (string? def-p)
                         (string? def-m)
                         (string=? def-p "all-in-one")
                         (string=? def-m c-id)
                       ) ;and
             ) ;default?
             (nodes (chain-def-nodes chain))
            ) ;
        (list (cons "id" c-id)
          (cons "name" (chain-def-name chain))
          (cons "default" default?)
          (cons "nodes"
            (let loop
              ((rest nodes) (i 1) (acc '()))
              (if (null? rest)
                (reverse acc)
                (loop (cdr rest) (+ i 1) (cons (%build-node st i (car rest)) acc))
              ) ;if
            ) ;let
          ) ;cons
        ) ;list
      ) ;let*
    ) ;define

    (define (pi-config-aio-state st)
      (let* ((dir (pi-config-state-dir st))
             (cfg-file (path-join dir "all-in-one.json"))
             (raw-cfg (and (file-exists? (path->string cfg-file)) (all-in-one-load-config dir))
             ) ;raw-cfg
             (valid? (and raw-cfg (all-in-one-cfg? raw-cfg) (pair? (all-in-one-cfg-chains raw-cfg)))
             ) ;valid?
             (cfg (if valid? raw-cfg (make-default-all-in-one-config)))
             (source (if valid? "all-in-one.json" "(builtin default)"))
            ) ;
        (list (cons "source" source)
          (cons "continuous" (all-in-one-cfg-continuous? cfg))
          (cons "maxWaitMs" (all-in-one-cfg-max-wait-ms cfg))
          (cons "retryIntervalMs" (all-in-one-cfg-retry-interval-ms cfg))
          (cons "probeTimeoutMs" (all-in-one-cfg-probe-timeout-ms cfg))
          (cons "chains"
            (map (lambda (c) (%build-chain st c)) (all-in-one-cfg-chains cfg))
          ) ;cons
        ) ;list
      ) ;let*
    ) ;define

    (define (pi-config-aio-chains aio)
      (or (%ref aio "chains") '())
    ) ;define

    (define (pi-config-aio-chain-nodes c)
      (or (%ref c "nodes") '())
    ) ;define

    ;; 链条右列详情：显示链条名称、默认状态、有序节点链路与全局调度参数
    (define (pi-config-aio-chain-detail-rows aio chain)
      (let* ((c-id (%ref chain "id"))
             (c-name (%ref chain "name"))
             (nodes (pi-config-aio-chain-nodes chain))
             (node-rows
               (map
                 (lambda (n)
                   (list (string-append "node #" (number->string (%ref n "index")))
                     (string-append (%ref n "provider")
                       "/"
                       (%ref n "model")
                       " • "
                       (or (%ref n "thinking") "off")
                     ) ;string-append
                   ) ;list
                 ) ;lambda
                 nodes
               ) ;map
             ) ;node-rows
            ) ;
        (append
          (list
            (list "chain"
              (if (and c-name (not (string=? c-name c-id)))
                (string-append c-id " (" c-name ")")
                c-id
              ) ;if
            ) ;list
            (list "default" (if (%ref chain "default") "yes" "no"))
            (list "nodes" (number->string (length nodes)))
          ) ;list
          node-rows
          (list (list "continuous" (if (%ref aio "continuous") "yes" "no"))
            (list "maxWait" (%format-duration (%ref aio "maxWaitMs")))
            (list "retry" (%format-duration (%ref aio "retryIntervalMs")))
            (list "probe" (%format-duration (%ref aio "probeTimeoutMs")))
            (list "source" (or (%ref aio "source") "(builtin default)"))
          ) ;list
        ) ;append
      ) ;let*
    ) ;define

    ;; 节点右列详情：显示当前节点的顺序、provider 状态、模型、思考等级、密钥来源与代理
    (define (pi-config-aio-node-detail-rows aio chain node)
      (list (list "chain" (%ref chain "id"))
        (list "order" (string-append "#" (number->string (%ref node "index"))))
        (list "provider"
          (string-append (%ref node "provider")
            (if (%ref node "enabled") " (enabled)" " (disabled)")
          ) ;string-append
        ) ;list
        (list "model" (%ref node "model"))
        (list "thinking" (or (%ref node "thinking") "off"))
        (list "key" (or (%ref node "keySource") "missing"))
        (list "proxy" (or (%ref node "proxy") "—"))
        (list "source" (or (%ref aio "source") "(builtin default)"))
      ) ;list
    ) ;define

    ;; all-in-one.json 的只读覆盖层视图
    (define (pi-config-aio-file-view st)
      (let ((content (pi-config-read-text st "all-in-one.json")))
        (list (cons "kind" "all-in-one")
          (cons "id" "all-in-one")
          (cons "path" "all-in-one.json")
          (cons "exists" (and content #t))
          (cons "content" content)
        ) ;list
      ) ;let
    ) ;define

    (define (pi-config-aio-status-text st aio)
      (string-append " "
        (pi-config-state-dir st)
        " · "
        (number->string (length (pi-config-aio-chains aio)))
        " chains · "
        (or (%ref aio "source") "(builtin default)")
        " · continuous "
        (if (%ref aio "continuous") "on" "off")
        " · default "
        (or (pi-config-state-default-provider st) "—")
        "/"
        (or (pi-config-state-default-model st) "—")
      ) ;string-append
    ) ;define

    ;; ---------- 已启用的供应商与模型 ----------

    (define (pi-config-aio-enabled-targets st)
      (let ((enabled-providers
              (filter
                (lambda (p)
                  (and (pi-config-provider-enabled? p)
                    (not (string=? (pi-config-provider-id p) "all-in-one"))
                  ) ;and
                ) ;lambda
                (pi-config-state-providers st)
              ) ;filter
            ) ;enabled-providers
           ) ;
        (filter (lambda (entry) (pair? (cdr entry)))
          (map
            (lambda (p)
              (cons (pi-config-provider-id p)
                (filter pi-config-model-enabled? (pi-config-provider-models p))
              ) ;cons
            ) ;lambda
            enabled-providers
          ) ;map
        ) ;filter
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
