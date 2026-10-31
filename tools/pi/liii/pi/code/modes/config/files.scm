;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; `gf pi config` 的文件层：
;;   pi-config-file-view    只读：光标所在条目对应的配置文件（路径/是否存在/原文）
;;   pi-config-plan-toggle  空格启停：算出要写的文件与新内容（纯函数，不落盘）
;;   pi-config-apply-writes 落盘：原子写（.tmp + rename）
;;
;; 语义（与运行时一致）：provider 是否可用由 models/providers.json 白名单决定；
;; 模型是否可用由 models/<provider>.json 的 models 数组决定，缺该键 = 内置目录全量可用。
(define-library (liii pi code modes config files)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii list)
    (liii string)
    (liii unicode)
    (liii path)
    (liii json)
    (liii pi code config user-fs)
    (liii pi code config user-providers)
    (liii pi code modes config json-write)
    (liii pi code modes config paths)
    (liii pi code modes config state)
  ) ;import
  (export pi-config-file-view pi-config-plan-toggle pi-config-plan-set-default)
  (begin

    (define %provider-schema "https://case-lambda.org/agent/provider.schema.json")

    ;; ---------- 光标定位 ----------

    (define (%clamp* i n)
      (cond ((<= n 0) 0)
            ((< i 0) 0)
            ((>= i n) (- n 1))
            (else i)
      ) ;cond
    ) ;define

    (define (%cursor-ref cursor key)
      (let ((pair (assoc key cursor)))
        (if pair (cdr pair) 0)
      ) ;let
    ) ;define

    (define (%provider-at st cursor)
      (let* ((providers (pi-config-state-providers st))
             (index (%clamp* (%cursor-ref cursor 'provider) (length providers)))
            ) ;
        (and (pair? providers) (list-ref providers index))
      ) ;let*
    ) ;define

    (define (%model-at st cursor)
      (let* ((provider (%provider-at st cursor))
             (models (if provider (pi-config-provider-models provider) '()))
             (index (%clamp* (%cursor-ref cursor 'model) (length models)))
            ) ;
        (and (pair? models) (list-ref models index))
      ) ;let*
    ) ;define

    (define (%model-id-at st cursor index)
      (let* ((provider (%provider-at st cursor))
             (models (if provider (pi-config-provider-models provider) '()))
            ) ;
        (and (> (length models) index) (pi-config-model-id (list-ref models index)))
      ) ;let*
    ) ;define

    ;; ---------- 只读文件视图 ----------

    ;; column: 0 = provider 列（看 models/<p>.json），其它 = 模型列（看模型文件）
    (define* (pi-config-file-view st cursor (column 0))
      (let* ((provider (%provider-at st cursor))
             (model (%model-at st cursor))
             (provider-column? (= column 0))
             (kind (if provider-column? "provider" "model"))
             (id (if provider-column?
                   (pi-config-provider-id provider)
                   (pi-config-model-id model)
                 ) ;if
             ) ;id
             (rel (if provider-column?
                    (pi-config-rel-provider id)
                    (pi-config-rel-model (pi-config-provider-id provider) id)
                  ) ;if
             ) ;rel
             (content (pi-config-read-text st rel))
            ) ;
        (list (cons "kind" kind)
          (cons "id" id)
          (cons "path" rel)
          (cons "exists" (and content #t))
          (cons "content" content)
        ) ;list
      ) ;let*
    ) ;define*

    ;; ---------- 启停计划 ----------

    (define (%result ok? message writes)
      (list (cons "ok" (and ok? #t)) (cons "message" message) (cons "writes" writes))
    ) ;define

    (define (%refused message)
      (%result #f message '())
    ) ;define

    ;; 当前可用 provider id（按 state 顺序）
    (define (%enabled-provider-ids st)
      (map pi-config-provider-id
        (filter pi-config-provider-enabled? (pi-config-state-providers st))
      ) ;map
    ) ;define

    ;; 该 provider 下可用模型 id（可指定排除项）
    (define (%enabled-model-ids provider)
      (map pi-config-model-id
        (filter pi-config-model-enabled? (pi-config-provider-models provider))
      ) ;map
    ) ;define

    ;; 默认模型的替代者：按 state 顺序找第一个仍可用的 provider/model
    ;; drop-provider? 为真时整条 provider 都被排除（禁用 provider 的场景）
    (define (%fallback-default st skip-p-id skip-m-id drop-provider?)
      (let loop
        ((providers (pi-config-state-providers st)))
        (cond ((null? providers) #f)
              (else
                (let* ((p (car providers))
                       (p-id (pi-config-provider-id p))
                       (drop? (and drop-provider? (string=? p-id skip-p-id)))
                       (ids (if drop? '() (%enabled-model-ids p)))
                       (kept
                         (if (string=? p-id skip-p-id)
                           (filter (lambda (m) (not (string=? m skip-m-id))) ids)
                           ids
                         ) ;if
                       ) ;kept
                      ) ;
                  (if (and (pi-config-provider-enabled? p) (pair? kept))
                    (cons p-id (car kept))
                    (loop (cdr providers))
                  ) ;if
                ) ;let*
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    ;; 就地替换 alist 里的某个键（不存在则追加），保持其余键顺序不变
    (define (%set-key alist key value)
      (let loop
        ((rest alist) (acc '()) (seen? #f))
        (cond
         ((null? rest) (reverse (if seen? acc (cons (cons key value) acc))))
         ((equal? (car (car rest)) key) (loop (cdr rest) (cons (cons key value) acc) #t))
         (else (loop (cdr rest) (cons (car rest) acc) seen?))
        ) ;cond
      ) ;let
    ) ;define

    ;; settings.json 的改动：把默认模型设为 provider/model
    (define (%settings-write st provider-id model-id)
      (let* ((rel "settings.json")
             (raw (or (pi-user-read-json (pi-config-abs st rel)) '()))
             (new (%set-key (%set-key raw "defaultProvider" provider-id) "defaultModel" model-id)
             ) ;new
            ) ;
        (cons rel (string-append (pi-config-json-pretty new) "\n"))
      ) ;let*
    ) ;define

    ;; 纯函数：把选中模型设为默认模型（写 settings.json）
    (define* (pi-config-plan-set-default st cursor (column 1))
      (let* ((provider (%provider-at st cursor)) (model (%model-at st cursor)))
        (cond ((or (not provider) (not model)) (%refused "nothing selected"))
              ((= column 0) (%refused "select a model first"))
              ((not (pi-config-provider-enabled? provider))
               (%refused (string-append "provider "
                           (pi-config-provider-id provider)
                           " is disabled — enable it first"
                         ) ;string-append
               ) ;%refused
              ) ;
              ((not (pi-config-model-enabled? model))
               (%refused "model is not enabled — press space to enable it")
              ) ;
              (else
                (let* ((p-id (pi-config-provider-id provider)) (m-id (pi-config-model-id model)))
                  (%result #t
                    (string-append "default model is now " p-id "/" m-id)
                    (list (%settings-write st p-id m-id))
                  ) ;%result
                ) ;let*
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define*

    ;; 白名单文本（每行一个 id 的 json array + 换行）
    (define (%providers-json ids)
      (string-append (pi-config-json-pretty (list->vector ids)) "\n")
    ) ;define

    (define (%providers-write-writes ids)
      (list (cons "models/providers.json" (%providers-json ids)))
    ) ;define

    ;; provider 禁用：从白名单移除，必要时级联默认模型
    (define (%disable-provider st p-id ids)
      (let* ((rest (filter (lambda (id) (not (string=? id p-id))) ids)))
        (if (null? rest)
          (%refused "at least one provider must stay enabled")
          (let* ((writes (%providers-write-writes rest))
                 (fallback (and (equal? (pi-config-state-default-provider st) p-id)
                             (%fallback-default st p-id "" #t)
                           ) ;and
                 ) ;fallback
                 (final
                   (if fallback
                     (append writes (list (%settings-write st (car fallback) (cdr fallback))))
                     writes
                   ) ;if
                 ) ;final
                 (message (if fallback
                            (string-append "disabled "
                              p-id
                              " · default moved to "
                              (car fallback)
                              "/"
                              (cdr fallback)
                            ) ;string-append
                            (string-append "disabled " p-id)
                          ) ;if
                 ) ;message
                ) ;
            (%result #t message final)
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    ;; provider 启用：加入白名单；缺 <provider>.json 时创建骨架
    (define (%enable-provider st provider ids)
      (let* ((p-id (pi-config-provider-id provider))
             (new-ids (append ids (list p-id)))
             (provider-rel (pi-config-rel-provider p-id))
             (skeleton
               (string-append (pi-config-json-pretty (list (cons "$schema" %provider-schema)))
                 "\n"
               ) ;string-append
             ) ;skeleton
             (writes (%providers-write-writes new-ids))
            ) ;
        (if (pi-config-read-text st provider-rel)
          (%result #t (string-append "enabled " p-id) writes)
          (%result #t
            (string-append "enabled " p-id " · created " provider-rel)
            (append writes (list (cons provider-rel skeleton)))
          ) ;%result
        ) ;if
      ) ;let*
    ) ;define

    ;; provider 启停
    (define (%toggle-provider st provider)
      (let* ((p-id (pi-config-provider-id provider)) (ids (%enabled-provider-ids st)))
        (if (pi-config-provider-enabled? provider)
          (%disable-provider st p-id ids)
          (%enable-provider st provider ids)
        ) ;if
      ) ;let*
    ) ;define

    ;; 白名单数组重写：保留其它键的顺序，models 就地替换（原本没有则追加）
    (define (%provider-file-with-models st provider ids)
      (let* ((p-id (pi-config-provider-id provider))
             (rel (pi-config-rel-provider p-id))
             (raw (or (pi-user-read-json (pi-config-abs st rel)) '()))
             (has-models? (and (assoc "models" raw) #t))
             (stripped
               (filter
                 (lambda (e) (not (equal? (car e) "models")))
                 raw
               ) ;filter
             ) ;stripped
             (base (if (pair? stripped) stripped (list (cons "$schema" %provider-schema))))
             (new
               (append base (list (cons "models" (list->vector ids))))
             ) ;new
            ) ;
        (cons rel (string-append (pi-config-json-pretty new) "\n"))
      ) ;let*
    ) ;define

    ;; 模型启停（禁用分支）：从白名单移除，必要时级联默认模型
    (define (%disable-model st provider model wl)
      (let* ((p-id (pi-config-provider-id provider))
             (m-id (pi-config-model-id model))
             (rest
               (filter (lambda (id) (not (string=? id m-id))) wl)
             ) ;rest
             (default? (and (equal? (pi-config-state-default-provider st) p-id)
                         (equal? (pi-config-state-default-model st) m-id)
                       ) ;and
             ) ;default?
             (fallback (and default? (%fallback-default st p-id m-id #f)))
             (write (%provider-file-with-models st provider rest))
             (writes
               (if fallback
                 (list write (%settings-write st (car fallback) (cdr fallback)))
                 (list write)
               ) ;if
             ) ;writes
             (message (if fallback
                        (string-append "disabled "
                          p-id
                          "/"
                          m-id
                          " · default moved to "
                          (car fallback)
                          "/"
                          (cdr fallback)
                        ) ;string-append
                        (string-append "disabled " p-id "/" m-id)
                      ) ;if
             ) ;message
            ) ;
        (%result #t message writes)
      ) ;let*
    ) ;define

    ;; 模型启停：无白名单 = 新建白名单只含该模型（不存在"删除"语义）
    (define (%toggle-model st provider model)
      (let* ((p-id (pi-config-provider-id provider))
             (m-id (pi-config-model-id model))
             (rel (pi-config-rel-provider p-id))
             (raw (or (pi-user-read-json (pi-config-abs st rel)) '()))
             (wl-raw (pi-user-dict-ref raw "models" #f))
             (wl (and (vector? wl-raw) (vector->list wl-raw)))
            ) ;
        (cond ((not (pi-config-provider-enabled? provider))
               (%refused (string-append "provider " p-id " is disabled — enable it first"))
              ) ;
              ((not wl)
               (%result #t
                 (string-append "whitelist created with 1 model · " m-id)
                 (list (%provider-file-with-models st provider (list m-id)))
               ) ;%result
              ) ;
              ((pi-config-model-enabled? model) (%disable-model st provider model wl))
              (else
                (%result #t
                  (string-append "enabled " p-id "/" m-id)
                  (list (%provider-file-with-models st provider (append wl (list m-id))))
                ) ;%result
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    ;; 纯函数：返回 ((ok . #t/#f) (message . ...) (writes . ((rel . text) ...)))
    ;; column: 0 = provider 列，其它 = 模型列
    (define* (pi-config-plan-toggle st cursor (column 0))
      (let ((provider (%provider-at st cursor)))
        (cond ((not provider) (%refused "nothing selected"))
              ((= column 0) (%toggle-provider st provider))
              (else
                (let ((model (%model-at st cursor)))
                  (if model
                    (%toggle-model st provider model)
                    (%refused "this provider has no models")
                  ) ;if
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define*

  ) ;begin
) ;define-library
