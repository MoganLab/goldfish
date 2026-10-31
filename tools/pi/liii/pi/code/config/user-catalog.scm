;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 用户配置目录（~/.gf/agent）的模型目录加载：
;;   providers.json          provider id 白名单（唯一致命文件：语法错/非字符串/重复 id 抛错）
;;   models/<provider>.json  provider 层：name/baseUrl/api/httpProxy/models
;;   models/<provider>/<model>.json  模型条目（文件名即 id）
;;
;; 合并优先级（字段级）：模型文件 > 内置目录。
;; 输出 catalog 结构与 models-store 兼容：
;;   ((provider-name ("baseUrl" . ...) ("api" . ...) ("models" m1 m2 ...)) ...)

(define-library (liii pi code config user-catalog)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii list)
    (liii string)
    (liii path)
    (liii json)
    (liii sort)
    (liii os)
    (liii pi ai models builtin)
    (liii pi ai providers registry)
  ) ;import
  (export pi-load-providers-whitelist
    pi-build-user-catalog
    pi-user-provider-config
  ) ;export
  (begin

    ;; ---------- 基础工具 ----------

    (define (%dict-ref obj key default-val)
      (if (not (pair? obj))
        default-val
        (let* ((str-key (if (symbol? key) (symbol->string key) key))
               (sym-key (if (string? key) (string->symbol key) key))
               (pair (or (assoc str-key obj) (assoc sym-key obj)))
              ) ;
          (if pair (cdr pair) default-val)
        ) ;let*
      ) ;if
    ) ;define

    (define (%read-json-file filepath)
      (and (file-exists? filepath)
        (catch #t
          (lambda () (string->json (path-read-text (path filepath))))
          (lambda args #f)
        ) ;catch
      ) ;and
    ) ;define

    (define (%warn msg)
      (display "[warn] ")
      (display msg)
      (newline)
    ) ;define

    (define (%vector->list v)
      (if (vector? v) (vector->list v) v)
    ) ;define

    ;; ---------- providers.json 白名单 ----------

    ;; 读取白名单；文件缺失返回 #f。
    ;; 语法错（string->json 抛 parse-error）、元素非字符串、重复 id：直接抛错拒启。
    (define (pi-load-providers-whitelist config-dir)
      (let ((path-str (path->string (path-join (path config-dir) (path "providers.json"))))
           ) ;
        (and (file-exists? path-str)
          (let ((data (string->json (path-read-text (path path-str)))))
            (cond ((not (vector? data))
                   (error 'value-error "providers.json: 顶层必须是 json array")
                  ) ;
                  (else
                    (let ((ids
                            (map (lambda (x)
                                   (if (string? x)
                                     x
                                     (error 'type-error "providers.json: 元素必须是字符串")
                                   ) ;if
                                 ) ;lambda
                              (vector->list data)
                            ) ;map
                          ) ;ids
                         ) ;
                      (let loop
                        ((seen '()) (rest ids))
                        (cond ((null? rest) ids)
                              ((member (car rest) seen)
                               (error 'value-error
                                 (string-append "providers.json: 重复 provider id \"" (car rest) "\"")
                               ) ;error
                              ) ;
                              (else (loop (cons (car rest) seen) (cdr rest)))
                        ) ;cond
                      ) ;let
                    ) ;let
                  ) ;else
            ) ;cond
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    ;; ---------- provider 层文件 ----------

    ;; api 简写归一化：anthropic/openai/google -> 全名
    (define (%normalize-api api-str)
      (cond ((string=? api-str "anthropic") "anthropic-messages")
            ((string=? api-str "openai") "openai-completions")
            ((string=? api-str "google") "google-generative-ai")
            (else api-str)
      ) ;cond
    ) ;define

    ;; 读 models/<id>.json，返回字段 alist；缺失或语法错返回 #f（告警）
    (define (%load-provider-file config-dir p-id)
      (let ((path-str
              (path->string (path-join (path config-dir)
                              (path "models")
                              (path (string-append p-id ".json"))
                            ) ;path-join
              ) ;path->string
            ) ;path-str
           ) ;
        (and (file-exists? path-str)
          (let ((data (%read-json-file path-str)))
            (if (pair? data)
              data
              (begin
                (%warn (string-append path-str ": JSON 解析失败，已忽略该文件"))
                #f
              ) ;begin
            ) ;if
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    ;; ---------- 模型目录文件 ----------

    ;; models/<provider>/<model>.json：文件名即 id；字段级覆盖内置
    (define (%load-model-entry config-dir p-id m-id)
      (let ((path-str
              (path->string (path-join (path config-dir)
                              (path "models")
                              (path p-id)
                              (path (string-append m-id ".json"))
                            ) ;path-join
              ) ;path->string
            ) ;path-str
           ) ;
        (and (file-exists? path-str)
          (let ((data (%read-json-file path-str)))
            (if (pair? data)
              data
              (begin
                (%warn (string-append path-str ": JSON 解析失败，已忽略该模型"))
                #f
              ) ;begin
            ) ;if
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    ;; 字段级合并：override 写了的字段生效，其余从 base 取
    (define (%merge-fields base override)
      (let ((keys
              (map
                (lambda (f) (if (symbol? (car f)) (symbol->string (car f)) (car f)))
                override
              ) ;map
            ) ;keys
            (norm
              (lambda (f) (if (symbol? (car f)) (cons (symbol->string (car f)) (cdr f)) f))
            ) ;norm
           ) ;
        (append (map norm override)
          (filter
            (lambda (f)
              (not
                (member (if (symbol? (car f)) (symbol->string (car f)) (car f)) keys)
              ) ;not
            ) ;lambda
            base
          ) ;filter
        ) ;append
      ) ;let
    ) ;define

    ;; ---------- 构造 catalog ----------

    ;; 内置模型条目表：id -> entry
    (define (%builtin-models-table p-id)
      (let ((raw (%dict-ref PI_PROVIDER_BUILTIN_MODELS p-id '())))
        (cond ((vector? raw) (vector->list raw))
              ((list? raw) raw)
              (else '())
        ) ;cond
      ) ;let
    ) ;define

    (define (%builtin-model-ids p-id)
      (map (lambda (m) (%dict-ref m "id" "")) (%builtin-models-table p-id))
    ) ;define

    ;; 单个 provider 的模型列表：
    ;; 有 models 白名单 -> 白名单顺序，逐 id 合并（模型文件 > 内置）
    ;; 无白名单 -> 内置全量 + 目录中不在内置的模型文件
    (define (%provider-models config-dir p-id)
      (let* ((p-file (or (%load-provider-file config-dir p-id) '()))
             (wl-raw (%dict-ref p-file "models" #f))
             (wl (and (vector? wl-raw) (vector->list wl-raw)))
            ) ;
        (cond
          ;; 白名单模式
          (wl
            (filter (lambda (x) x)
              (map
                (lambda (m-id)
                  (if (string? m-id)
                    (let* ((file-entry (%load-model-entry config-dir p-id m-id))
                           (builtin
                             (find (lambda (m) (string=? (%dict-ref m "id" "") m-id))
                               (%builtin-models-table p-id)
                             ) ;find
                           ) ;builtin
                           (base
                             (or builtin
                               file-entry
                               (begin
                                 (%warn (string-append "models/" p-id
                                          ".json: 模型 \"" m-id
                                          "\" 在内置目录与模型文件中均不存在，已忽略"
                                        ) ;string-append
                                 ) ;%warn
                                 #f
                               ) ;begin
                             ) ;or
                           ) ;base
                          ) ;
                      (and base
                        (let ((merged (if file-entry (%merge-fields base file-entry) base)))
                          (cons (cons "id" m-id) (filter (lambda (f) (not (equal? (car f) "id"))) merged))
                        ) ;let
                      ) ;and
                    ) ;let*
                    (begin
                      (%warn (string-append "models/"
                               p-id
                               ".json: models 白名单元素必须是字符串: "
                               (if (symbol? m-id) (symbol->string m-id) "")
                             ) ;string-append
                      ) ;%warn
                      #f
                    ) ;begin
                  ) ;if
                ) ;lambda
                wl
              ) ;map
            ) ;filter
          ) ;wl
          ;; 全量模式：内置 + 目录补充
          (else
            (let* ((builtin (%builtin-models-table p-id))
                   (builtin-ids (%builtin-model-ids p-id))
                   (dir-models (%directory-model-files config-dir p-id))
                   (extra
                     (filter (lambda (m) (not (member m builtin-ids))) dir-models)
                   ) ;extra
                  ) ;
              (append builtin
                (filter (lambda (x) x)
                  (map
                    (lambda (m-id)
                      (let ((file-entry (%load-model-entry config-dir p-id m-id)))
                        (and file-entry
                          (cons (cons "id" m-id)
                            (filter
                              (lambda (f) (not (equal? (car f) "id")))
                              file-entry
                            ) ;filter
                          ) ;cons
                        ) ;and
                      ) ;let
                    ) ;lambda
                    extra
                  ) ;map
                ) ;filter
              ) ;append
            ) ;let*
          ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    ;; 列出 models/<p-id>/ 下的模型文件名（去 .json，排除白名单外的告警由上层处理）
    (define (%directory-model-files config-dir p-id)
      (let ((dir-str (path->string (path-join (path config-dir) (path "models") (path p-id)))
            ) ;dir-str
           ) ;
        (if (not (file-exists? dir-str))
          '()
          (let ((entries
                  (catch #t (lambda () (vector->list (listdir dir-str))) (lambda args '()))
                ) ;entries
               ) ;
            (map (lambda (e) (string-remove-suffix e ".json"))
              (filter
                (lambda (e) (and (string-ends? e ".json") (not (string=? e ".json"))))
                entries
              ) ;filter
            ) ;map
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 构造用户 catalog：
    ;; 返回 ((p-id (name . ..) (baseUrl . ..) (api . ..) (models . (m1 m2 ...))) ...)
    ;; provider 集与顺序 = providers.json 白名单；文件缺失 = 全部内置 provider（字典序）
    (define (pi-build-user-catalog config-dir)
      (let* ((whitelist (pi-load-providers-whitelist config-dir))
             (p-ids (or whitelist (list-sort string<? (map car PI_PROVIDER_BUILTIN_MODELS))))
            ) ;
        (map
          (lambda (p-id)
            (let* ((p-file (or (%load-provider-file config-dir p-id) '()))
                   (builtin-record (pi-provider-record p-id))
                   (name (or (%dict-ref p-file "name" #f) p-id))
                   (base-url
                     (or (%dict-ref p-file "baseUrl" #f)
                       (and builtin-record (let ((v (assq 'baseUrl builtin-record))) (and v (cdr v))))
                     ) ;or
                   ) ;base-url
                   (api-raw
                     (or (%dict-ref p-file "api" #f)
                       (and builtin-record (let ((v (assq 'api builtin-record))) (and v (cdr v))))
                     ) ;or
                   ) ;api-raw
                   (api (and api-raw (%normalize-api api-raw)))
                   (models (%provider-models config-dir p-id))
                  ) ;
              (cons p-id
                (append (if name (list (cons "name" name)) '())
                  (if base-url (list (cons "baseUrl" base-url)) '())
                  (if api (list (cons "api" api)) '())
                  (list (cons "models" models))
                ) ;append
              ) ;cons
            ) ;let*
          ) ;lambda
          p-ids
        ) ;map
      ) ;let*
    ) ;define

    ;; 从 catalog 取 provider 层配置（name/baseUrl/api/httpProxy），无则 #f
    (define (pi-user-provider-config catalog p-id)
      (let ((entry (assoc p-id catalog)))
        (and entry (cdr entry))
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
