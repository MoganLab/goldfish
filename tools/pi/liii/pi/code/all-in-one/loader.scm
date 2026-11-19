;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;; all-in-one 配置加载入口。

(define-library (liii pi code all-in-one loader)
  (import (scheme base)
    (scheme file)
    (liii list)
    (liii path)
    (liii string)
    (liii json)
    (liii json schema)
    (liii json schema error)
    (liii pi ai log)
    (liii pi code all-in-one types)
    (liii pi code all-in-one config)
    (liii pi code all-in-one defaults)
    (liii pi code config json-write)
    (liii pi code config user-fs)
  ) ;import

  ;; 路径
  (export resolve-all-in-one-config-path)
  ;; 读取：读到用户配置才返回 record，#f 表示未读到
  (export all-in-one-load-config)
  ;; 缺省回退：把上一步的 #f 折叠成默认记录
  (export all-in-one-load-or-default-config)
  ;; 初始化
  (export ensure-all-in-one-config!)
  (begin

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

    ;; 默认值都是整秒（3600/60/8），按秒写回是为了与历史默认文件的字段形式
    ;; 保持一致。
    (define (%default-all-in-one-json)
      (let* ((cfg (make-default-all-in-one-config))
             (chains (all-in-one-cfg-chains cfg))
             (pairs
               (list (cons "provider" "all-in-one")
                 (cons "name" all-in-one-default-config-name)
                 (cons "continuous" (if (all-in-one-cfg-continuous? cfg) 'true 'false))
                 (cons "maxWait" (quotient (all-in-one-cfg-max-wait-ms cfg) 1000))
                 (cons "retryInterval" (quotient (all-in-one-cfg-retry-interval-ms cfg) 1000))
                 (cons "probeTimeout" (quotient (all-in-one-cfg-probe-timeout-ms cfg) 1000))
                 (cons "chains" (list->vector (map chain-def->json chains)))
               ) ;list
             ) ;pairs
            ) ;
        (string-append (pi-config-json-pretty pairs) "\n")
      ) ;let*
    ) ;define

    ;; 用户配置文件绝不被覆盖：仅在目标文件不存在时原子写入默认配置。
    (define (ensure-all-in-one-config! . opt-config-dir)
      (let* ((cfg-path (apply resolve-all-in-one-config-path opt-config-dir)))
        (if (file-exists? cfg-path)
          #f
          (let ((dir (path-parent (path cfg-path))))
            (path-mkdir dir :parents #t :exist-ok #t)
            (catch #t
              (lambda () (pi-write-atomic cfg-path (%default-all-in-one-json)) #t)
              (lambda args #f)
            ) ;catch
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define %cached-aio-schema #f)
    (define %cached-aio-schema-path #f)

    (define (%all-in-one-schema)
      (unless %cached-aio-schema
        (let ((schema-path
                (find (lambda (cand) (file-exists? (path->string cand)))
                  (map (lambda (d) (path-join d "liii/pi/code/resources/all-in-one.schema.json"))
                    *load-path*
                  ) ;map
                ) ;find
              ) ;schema-path
             ) ;
          (when schema-path
            (set! %cached-aio-schema-path (path->string schema-path))
            (set! %cached-aio-schema (string->json (path-read-text schema-path)))
          ) ;when
        ) ;let
      ) ;unless
      %cached-aio-schema
    ) ;define

    (define (%log-schema-errors cfg-path errs)
      (for-each
        (lambda (err)
          (pi-log-error! (string-append cfg-path
                           " schema validation error at "
                           (json-schema-error-json-path err)
                           " ("
                           (json-schema-error-keyword err)
                           "): "
                           (json-schema-error-message err)
                           (if %cached-aio-schema-path
                             (string-append " (schema: " %cached-aio-schema-path ")")
                             ""
                           ) ;if
                         ) ;string-append
          ) ;pi-log-error!
        ) ;lambda
        errs
      ) ;for-each
    ) ;define

    (define (%validate-aio-schema cfg-path data)
      (let ((schema (%all-in-one-schema)))
        (when schema
          (let ((errs (json-schema-errors data schema)))
            (unless (null? errs)
              (%log-schema-errors cfg-path errs)
            ) ;unless
          ) ;let
        ) ;when
      ) ;let
    ) ;define

    (define (all-in-one-load-config . opt-config-dir)
      (when (or (null? opt-config-dir) (not (car opt-config-dir)))
        (ensure-all-in-one-config!)
      ) ;when
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
                   (%validate-aio-schema cfg-path data)
                   (pi-user-warn (string-append cfg-path ": 根节点 provider 必须是 \"all-in-one\"")
                   ) ;pi-user-warn
                   #f
                  ) ;
                  (else (%validate-aio-schema cfg-path data) (json->all-in-one-cfg data cfg-path))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    ;; 分层不变量：读取失败（#f）与“配置存在但没有任何链”同样回退默认记录，
    ;; 因此请求、压缩摘要与配置 TUI 不必各自重复构造默认参数与默认链。
    (define (all-in-one-load-or-default-config . opt-config-dir)
      (let ((cfg (apply all-in-one-load-config opt-config-dir)))
        (if (and cfg (all-in-one-cfg? cfg) (pair? (all-in-one-cfg-chains cfg)))
          cfg
          (make-default-all-in-one-config)
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
