;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 模型层合成：内置目录 < provider 文件（models/<p>.json）< 模型文件
;; （models/<p>/<m>.json）。字段级合并，模型文件未写的字段逐层回落。
(define-library (liii pi code config user-models)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii list)
    (liii string)
    (liii os)
    (liii pi ai models builtin)
    (liii pi code all-in-one provider)
    (liii pi code config user-fs)
    (liii pi code config user-providers)
  ) ;import
  (export pi-user-provider-models
    pi-user-provider-model-list
    pi-user-model-exists?
  ) ;export
  (begin

    ;; ---------- 字段级合并 ----------

    (define (%field-key f)
      (if (symbol? (car f)) (symbol->string (car f)) (car f))
    ) ;define

    ;; override 写了的字段生效，其余从 base 取；结果键统一为字符串
    (define (%merge-fields base override)
      (let ((keys (map %field-key override)))
        (append
          (map (lambda (f) (cons (%field-key f) (cdr f))) override)
          (filter
            (lambda (f) (not (member (%field-key f) keys)))
            base
          ) ;filter
        ) ;append
      ) ;let
    ) ;define

    ;; 单模型合成：provider 层字段覆盖内置值，模型文件再覆盖其上
    (define (%compose-model p-fields base file-entry)
      (let* ((with-provider (if (pair? base) (%merge-fields base p-fields) p-fields))
             (merged (if file-entry (%merge-fields with-provider file-entry) with-provider))
            ) ;
        (if (pair? merged) merged #f)
      ) ;let*
    ) ;define

    ;; 合成结果补齐 id 并去掉重复 id 字段（优先保留数据中已声明的 id）
    (define (%with-id m-id merged)
      (let ((actual-id (or (pi-user-dict-ref merged "id" #f) m-id)))
        (cons (cons "id" actual-id)
          (filter
            (lambda (f) (not (equal? (%field-key f) "id")))
            merged
          ) ;filter
        ) ;cons
      ) ;let
    ) ;define

    ;; ---------- 内置目录 ----------

    (define (%builtin-table p-id)
      (let ((raw (pi-user-dict-ref PI_PROVIDER_BUILTIN_MODELS p-id '())))
        (cond ((vector? raw) (vector->list raw))
              ((list? raw) raw)
              (else '())
        ) ;cond
      ) ;let
    ) ;define

    (define (%find-builtin builtin m-id)
      (find (lambda (m) (string=? (pi-user-dict-ref m "id" "") m-id)) builtin)
    ) ;define

    (define (%builtin-ids builtin)
      (map (lambda (m) (pi-user-dict-ref m "id" "")) builtin)
    ) ;define

    ;; ---------- 模型文件 ----------

    ;; 判断模型是否存在于内置目录或模型文件中（ollama 允许本地任意模型）
    (define (pi-user-model-exists? config-dir p-id m-id)
      (and (string? p-id)
        (string? m-id)
        (or (equal? p-id "ollama")
          (and (equal? p-id "all-in-one")
            (let ((models (all-in-one-models config-dir)))
              (and
                (find (lambda (m) (equal? (pi-user-dict-ref m "id" #f) m-id)) models)
                #t
              ) ;and
            ) ;let
          ) ;and
          (and (%find-builtin (%builtin-table p-id) m-id) #t)
          (let* ((path-str (pi-user-config-path config-dir "models" p-id (string-append m-id ".json"))
                 ) ;path-str
                ) ;
            (or (and (file-exists? path-str) (pair? (pi-user-read-json path-str)))
              (and (pair? (%find-model-file-by-id config-dir p-id m-id)) #t)
            ) ;or
          ) ;let*
        ) ;or
      ) ;and
    ) ;define

    ;; 读 models/<p>/<m>.json；缺失返回 #f，语法错告警后返回 #f
    (define (%find-model-file-by-id config-dir p-id m-id)
      (let* ((dir-str (pi-user-config-path config-dir "models" p-id))
             (exists (file-exists? dir-str))
             (entries
               (and exists
                 (catch #t (lambda () (vector->list (listdir dir-str))) (lambda args '()))
               ) ;and
             ) ;entries
            ) ;
        (if (not exists)
          #f
          (let loop
            ((files
               (filter
                 (lambda (e) (and (string-ends? e ".json") (not (string=? e ".json"))))
                 entries
               ) ;filter
             ) ;files
            ) ;
            (if (null? files)
              #f
              (let* ((f (car files))
                     (p (pi-user-config-path config-dir "models" p-id f))
                     (data (pi-user-read-json p))
                    ) ;
                (if (and (pair? data) (equal? (pi-user-dict-ref data "id" #f) m-id))
                  data
                  (loop (cdr files))
                ) ;if
              ) ;let*
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (%model-file config-dir p-id m-id)
      (let* ((path-str (pi-user-config-path config-dir "models" p-id (string-append m-id ".json"))
             ) ;path-str
             (exists (file-exists? path-str))
             (data (and exists (pi-user-read-json path-str)))
            ) ;
        (cond ((pair? data) data)
              (exists (pi-user-warn (string-append path-str ": JSON 解析失败，已忽略该模型")
                      ) ;pi-user-warn
                #f
              ) ;exists
              (else (%find-model-file-by-id config-dir p-id m-id))
        ) ;cond
      ) ;let*
    ) ;define

    ;; 列出 models/<p>/ 下的模型 id（去 .json 后缀）
    (define (%directory-model-ids config-dir p-id)
      (let* ((dir-str (pi-user-config-path config-dir "models" p-id))
             (exists (file-exists? dir-str))
             (entries
               (and exists
                 (catch #t (lambda () (vector->list (listdir dir-str))) (lambda args '()))
               ) ;and
             ) ;entries
            ) ;
        (if (not exists)
          '()
          (map (lambda (e) (string-remove-suffix e ".json"))
            (filter
              (lambda (e) (and (string-ends? e ".json") (not (string=? e ".json"))))
              entries
            ) ;filter
          ) ;map
        ) ;if
      ) ;let*
    ) ;define

    ;; ---------- provider 层字段 ----------

    ;; provider 文件的 baseUrl/api（字段级覆盖内置值）
    (define (pi-user-provider-fields p-file)
      (let ((p-url (pi-user-dict-ref p-file "baseUrl" #f))
            (p-api (pi-user-dict-ref p-file "api" #f))
           ) ;
        (append (if p-url (list (cons "baseUrl" p-url)) '())
          (if p-api (list (cons "api" p-api)) '())
        ) ;append
      ) ;let
    ) ;define

    ;; ---------- 两种模式 ----------

    ;; 白名单模式：按白名单顺序逐个合成；查无此模型时告警忽略（支持字符串 id 或内联模型对象）
    (define (%whitelist-model config-dir p-id p-fields builtin m-spec)
      (cond
       ((pair? m-spec)
        (let* ((m-id (pi-user-dict-ref m-spec "id" ""))
               (base (or (%find-builtin builtin m-id) '()))
               (merged (%compose-model p-fields base m-spec))
              ) ;
          (and merged (%with-id m-id merged))
        ) ;let*
       ) ;
       ((string? m-spec)
        (let* ((m-id m-spec)
               (file-entry (%model-file config-dir p-id m-id))
               (base
                 (or (%find-builtin builtin m-id)
                   (and file-entry '())
                   (if (or (equal? p-id "ollama") (null? builtin)) '() (%warn-missing p-id m-id))
                 ) ;or
               ) ;base
               (merged (and base (%compose-model p-fields base file-entry)))
              ) ;
          (and merged (%with-id m-id merged))
        ) ;let*
       ) ;
       (else #f)
      ) ;cond
    ) ;define

    (define (%warn-missing p-id m-id)
      (pi-user-warn (string-append "models/" p-id ".json: 模型 \"" m-id
                      "\" 在内置目录与模型文件中均不存在，已忽略"
                    ) ;string-append
      ) ;pi-user-warn
      #f
    ) ;define

    (define (%whitelist-models config-dir p-id p-fields builtin wl)
      (let ((valid (filter (lambda (x) (or (string? x) (pair? x))) wl)))
        (for-each
          (lambda (x)
            (unless (or (string? x) (pair? x))
              (pi-user-warn (string-append "models/" p-id ".json: models 元素必须是字符串或对象")
              ) ;pi-user-warn
            ) ;unless
          ) ;lambda
          wl
        ) ;for-each
        (filter (lambda (x) x)
          (map (lambda (m-id) (%whitelist-model config-dir p-id p-fields builtin m-id))
            valid
          ) ;map
        ) ;filter
      ) ;let
    ) ;define

    ;; 全量模式：内置目录逐个套 provider 字段（+ 同名模型文件），
    ;; 再追加目录中不在内置目录里的模型
    (define (%full-models config-dir p-id p-fields builtin)
      (let* ((extra
               (filter
                 (lambda (m) (not (member m (%builtin-ids builtin))))
                 (%directory-model-ids config-dir p-id)
               ) ;filter
             ) ;extra
             (from-builtin
               (map
                 (lambda (m)
                   (let* ((m-id (pi-user-dict-ref m "id" ""))
                          (merged (%compose-model p-fields m (%model-file config-dir p-id m-id)))
                         ) ;
                     (%with-id m-id merged)
                   ) ;let*
                 ) ;lambda
                 builtin
               ) ;map
             ) ;from-builtin
             (from-dir
               (filter (lambda (x) x)
                 (map
                   (lambda (m-id)
                     (let ((merged (%compose-model p-fields '() (%model-file config-dir p-id m-id))))
                       (and merged (%with-id m-id merged))
                     ) ;let
                   ) ;lambda
                   extra
                 ) ;map
               ) ;filter
             ) ;from-dir
            ) ;
        (append from-builtin from-dir)
      ) ;let*
    ) ;define

    ;; 白名单之外但仍存在于内置目录/模型文件中的模型（配置视图里灰显为不可用）
    (define (%non-whitelist-models config-dir p-id p-fields builtin wl)
      (let* ((known (append (%builtin-ids builtin) (%directory-model-ids config-dir p-id)))
             (rest
               (filter (lambda (m-id) (not (member m-id wl))) known)
             ) ;rest
            ) ;
        (filter (lambda (x) x)
          (map
            (lambda (m-id)
              (let* ((base (or (%find-builtin builtin m-id) '()))
                     (merged (%compose-model p-fields base (%model-file config-dir p-id m-id)))
                    ) ;
                (and merged (%with-id m-id merged))
              ) ;let*
            ) ;lambda
            rest
          ) ;map
        ) ;filter
      ) ;let*
    ) ;define

    ;; 给模型条目追加可用标记
    (define (%mark n m)
      (append m (list (cons "enabled" (and n #t))))
    ) ;define

    ;; 配置视图用的完整模型列表：
    ;;   可用（白名单内）在前，不可用（被白名单过滤）在后，各带 "enabled" 标记
    (define (pi-user-provider-model-list config-dir p-id)
      (if (equal? p-id "all-in-one")
        (map (lambda (m) (%mark #t m)) (all-in-one-models config-dir))
        (let* ((p-file (or (pi-user-provider-file config-dir p-id) '()))
               (p-fields (pi-user-provider-fields p-file))
               (wl-raw (pi-user-dict-ref p-file "models" #f))
               (wl (and (vector? wl-raw) (vector->list wl-raw)))
               (builtin (%builtin-table p-id))
              ) ;
          (if wl
            (append (map (lambda (m) (%mark #t m))
                      (%whitelist-models config-dir p-id p-fields builtin wl)
                    ) ;map
              (map (lambda (m) (%mark #f m))
                (%non-whitelist-models config-dir p-id p-fields builtin wl)
              ) ;map
            ) ;append
            (map (lambda (m) (%mark #t m)) (%full-models config-dir p-id p-fields builtin))
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    ;; provider 的模型列表（对外入口）
    (define (pi-user-provider-models config-dir p-id)
      (if (equal? p-id "all-in-one")
        (all-in-one-models config-dir)
        (let* ((p-file (or (pi-user-provider-file config-dir p-id) '()))
               (p-fields (pi-user-provider-fields p-file))
               (wl-raw (pi-user-dict-ref p-file "models" #f))
               (wl (and (vector? wl-raw) (vector->list wl-raw)))
               (builtin (%builtin-table p-id))
              ) ;
          (if wl
            (%whitelist-models config-dir p-id p-fields builtin wl)
            (%full-models config-dir p-id p-fields builtin)
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
