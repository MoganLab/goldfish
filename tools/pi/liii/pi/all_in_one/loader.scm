;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi all_in_one loader)
  (import (scheme base)
    (scheme file)
    (liii list)
    (liii path)
    (liii string)
    (liii json)
    (liii pi all_in_one types)
    (liii pi code config user-fs)
  ) ;import
  (export resolve-all-in-one-config-path all-in-one-load-config)
  (begin

    ;; 解析 all-in-one.json 配置文件路径。
    ;; 缺省或传入 #f 时返回默认路径 ~/.gf/agent/all-in-one.json，
    ;; 显式指定非空配置目录时返回该目录下的 all-in-one.json。
    (define (resolve-all-in-one-config-path . opt-config-dir)
      (let ((base-dir (if (and (pair? opt-config-dir) (car opt-config-dir))
                        (car opt-config-dir)
                        (path-join (path-home) ".gf" "agent")
                      ) ;if
            ) ;base-dir
           ) ;
        (path->string (path-join base-dir "all-in-one.json"))
      ) ;let
    ) ;define

    ;; 单节点解析：严格校验必填字段 provider 与 model，可选 thinkingLevel（缺省为 "off"）
    ;; 缺失或类型非法时发出警告并返回 #f，合法时返回 <chain-node> 记录。
    (define (%parse-node chain-id node-data)
      (if (not (pair? node-data))
        (begin
          (pi-user-warn (string-append "all-in-one: chain \"" chain-id "\" 的 node 必须是对象")
          ) ;pi-user-warn
          #f
        ) ;begin
        (let* ((p (pi-user-dict-ref node-data "provider" #f))
               (m (pi-user-dict-ref node-data "model" #f))
               (tl-raw (pi-user-dict-ref node-data "thinkingLevel" "off"))
               (tl
                 (if
                   (and (string? tl-raw) (not (string-null? (string-trim-both tl-raw))))
                   tl-raw
                   "off"
                 ) ;if
               ) ;tl
              ) ;
          (cond
           ((not
              (and (string? p) (not (string-null? (string-trim-both p))))
            ) ;not
            (pi-user-warn (string-append "all-in-one: chain \""
                            chain-id
                            "\" 节点缺少必填字段 provider，已跳过"
                          ) ;string-append
            ) ;pi-user-warn
            #f
           ) ;
           ((not
              (and (string? m) (not (string-null? (string-trim-both m))))
            ) ;not
            (pi-user-warn (string-append "all-in-one: chain \""
                            chain-id
                            "\" 节点缺少必填字段 model，已跳过"
                          ) ;string-append
            ) ;pi-user-warn
            #f
           ) ;
           (else (make-chain-node p m tl))
          ) ;cond
        ) ;let*
      ) ;if
    ) ;define

    ;; 单条链解析：校验 id，提取 name，逐个解析 nodes 并过滤无效节点
    (define (%parse-chain chain-data)
      (if (not (pair? chain-data))
        (begin
          (pi-user-warn "all-in-one: chain 必须是对象")
          #f
        ) ;begin
        (let* ((id (pi-user-dict-ref chain-data "id" #f))
               (name (pi-user-dict-ref chain-data "name" #f))
               (raw-nodes (pi-user-dict-ref chain-data "nodes" '()))
               (nodes-list (pi-user-vector->list raw-nodes))
              ) ;
          (if
            (not
              (and (string? id) (not (string-null? (string-trim-both id))))
            ) ;not
            (begin
              (pi-user-warn "all-in-one: chain 缺少必填字段 id")
              #f
            ) ;begin
            (let* ((eff-name
                     (if
                       (and (string? name) (not (string-null? (string-trim-both name))))
                       name
                       id
                     ) ;if
                   ) ;eff-name
                   (valid-nodes
                     (if (not (list? nodes-list))
                       (begin
                         (pi-user-warn (string-append "all-in-one: chain \"" id "\" 的 nodes 必须是数组")
                         ) ;pi-user-warn
                         '()
                       ) ;begin
                       (filter (lambda (x) x) (map (lambda (n) (%parse-node id n)) nodes-list))
                     ) ;if
                   ) ;valid-nodes
                  ) ;
              (make-chain-def id eff-name valid-nodes)
            ) ;let*
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    ;; 解析 chains 数组，返回有效 <chain-def> 列表；格式非法时返回 #f
    (define (%parse-chains cfg-path raw-chains)
      (let ((chains-list (pi-user-vector->list raw-chains)))
        (if (not (list? chains-list))
          (begin
            (pi-user-warn (string-append cfg-path ": chains 必须是数组"))
            #f
          ) ;begin
          (filter (lambda (x) x) (map %parse-chain chains-list))
        ) ;if
      ) ;let
    ) ;define

    ;; 解析 JSON 布尔值（支持 #t/#f 与符号 'true/'false）
    (define (%json-boolean raw default-val)
      (cond ((boolean? raw) raw)
            ((eq? raw 'true) #t)
            ((eq? raw 'false) #f)
            (else default-val)
      ) ;cond
    ) ;define

    ;; 从合法的根 JSON 对象构建 <all-in-one-cfg> 记录
    (define (%build-cfg cfg-path data)
      (let* ((cont-raw (pi-user-dict-ref data "continuous" #t))
             (cont (%json-boolean cont-raw #t))
             (max-wait-raw (pi-user-dict-ref data "maxWaitMs" 3600000))
             (max-wait (if (and (number? max-wait-raw) (> max-wait-raw 0)) max-wait-raw 3600000)
             ) ;max-wait
             (retry-raw (pi-user-dict-ref data "retryIntervalMs" 60000))
             (retry-interval (if (and (number? retry-raw) (> retry-raw 0)) retry-raw 60000))
             (probe-raw (pi-user-dict-ref data "probeTimeoutMs" 8000))
             (probe-timeout (if (and (number? probe-raw) (> probe-raw 0)) probe-raw 8000))
             (raw-chains (pi-user-dict-ref data "chains" '()))
             (chains (%parse-chains cfg-path raw-chains))
            ) ;
        (and chains
          (make-all-in-one-cfg cont max-wait retry-interval probe-timeout chains)
        ) ;and
      ) ;let*
    ) ;define

    ;; all-in-one-load-config
    ;; 加载并校验 all-in-one.json 配置文件。
    ;; 若文件不存在，安全返回 #f。
    ;; 若文件语法错误或根 provider 不是 "all-in-one"，告警后返回 #f。
    ;; 成功时返回 <all-in-one-cfg> 记录。
    (define (all-in-one-load-config . opt-config-dir)
      (let* ((cfg-path (apply resolve-all-in-one-config-path opt-config-dir)))
        (if (not (file-exists? cfg-path))
          #f
          (let ((data (pi-user-read-json cfg-path)))
            (cond ((not (pair? data))
                   (pi-user-warn (string-append cfg-path ": JSON 解析失败或根节点非对象")
                   ) ;pi-user-warn
                   #f
                  ) ;
                  ((not (equal? (pi-user-dict-ref data "provider" #f) "all-in-one"))
                   (pi-user-warn (string-append cfg-path ": 根节点 provider 必须是 \"all-in-one\"")
                   ) ;pi-user-warn
                   #f
                  ) ;
                  (else (%build-cfg cfg-path data))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
