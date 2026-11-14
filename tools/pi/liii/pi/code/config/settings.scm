;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code config settings)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii path)
    (liii json)
    (liii pi ai config types)
    (liii pi ai config record)
    (liii pi code config json-write)
    (liii pi code core compaction tokens)
  ) ;import
  (export json-dict-ref pi-read-json-file pi-load-settings
    pi-save-default-thinking-level! pi-save-default-model!
    pi-save-hide-thinking-block! pi-load-compaction-settings
  ) ;export
  (begin

    ;; JSON 对象（alist）取键值：键可为字符串或符号。空 JSON 对象在
    ;; (liii json) 中解析为 (())，故不能直接交给 assoc（要求每项都是 pair），
    ;; 这里自行遍历并跳过非 pair 项。
    (define (%json-dict-pair obj str-key sym-key)
      (let loop
        ((rest obj))
        (cond ((not (pair? rest)) #f)
              ((and (pair? (car rest))
                 (let ((k (car (car rest))))
                   (or (equal? k str-key) (equal? k sym-key))
                 ) ;let
               ) ;and
               (car rest)
              ) ;
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (json-dict-ref obj key default-val)
      (if (not (pair? obj))
        default-val
        (let* ((str-key (if (symbol? key) (symbol->string key) key))
               (sym-key (if (string? key) (string->symbol key) key))
               (pair (%json-dict-pair obj str-key sym-key))
              ) ;
          (if pair (cdr pair) default-val)
        ) ;let*
      ) ;if
    ) ;define

    (define (pi-read-json-file filepath)
      (and (path-exists? filepath)
        (catch #t (lambda () (string->json (path-read-text filepath))) (lambda args #f))
      ) ;and
    ) ;define

    (define (%normalize-bool val)
      (case val
       ((true) #t)
       ((false) #f)
       (else val)
      ) ;case
    ) ;define

    (define (pi-load-settings config-dir)
      (let* ((filepath (path-join config-dir "settings.json"))
             (obj (or (pi-read-json-file filepath) '()))
            ) ;
        (json->pi-settings obj)
      ) ;let*
    ) ;define

    (define (pi-save-default-thinking-level! config-dir level)
      (let* ((filepath (path-join config-dir "settings.json"))
             (settings (or (pi-read-json-file filepath) '()))
             (updated
               (cons (cons "defaultThinkingLevel" level)
                 (filter
                   (lambda (entry)
                     (not (or (equal? (car entry) "defaultThinkingLevel")
                            (equal? (car entry) 'defaultThinkingLevel)
                          ) ;or
                     ) ;not
                   ) ;lambda
                   settings
                 ) ;filter
               ) ;cons
             ) ;updated
            ) ;
        (path-mkdir config-dir :parents #t :exist-ok #t)
        (path-write-text filepath (string-append (pi-config-json-pretty updated) "\n"))
        level
      ) ;let*
    ) ;define

    ;; /model Ctrl+S 持久化：只更新 defaultProvider/defaultModel，保留其他设置
    (define (pi-save-default-model! config-dir provider model-id)
      (let* ((filepath (path-join config-dir "settings.json"))
             (settings (or (pi-read-json-file filepath) '()))
             (updated
               (append (list (cons "defaultProvider" provider) (cons "defaultModel" model-id))
                 (filter
                   (lambda (entry)
                     (not (or (equal? (car entry) "defaultProvider")
                            (equal? (car entry) 'defaultProvider)
                            (equal? (car entry) "defaultModel")
                            (equal? (car entry) 'defaultModel)
                          ) ;or
                     ) ;not
                   ) ;lambda
                   settings
                 ) ;filter
               ) ;append
             ) ;updated
            ) ;
        (path-mkdir config-dir :parents #t :exist-ok #t)
        (path-write-text filepath (string-append (pi-config-json-pretty updated) "\n"))
        (list provider model-id)
      ) ;let*
    ) ;define

    ;; Ctrl+T 切换思考块显隐后持久化：只更新 hideThinkingBlock，保留其他设置
    ;; （对齐上游 setHideThinkingBlock；JSON 布尔值为符号 true/false）
    (define (pi-save-hide-thinking-block! config-dir hide)
      (let* ((filepath (path-join config-dir "settings.json"))
             (settings (or (pi-read-json-file filepath) '()))
             (updated
               (cons (cons "hideThinkingBlock" (if hide 'true 'false))
                 (filter
                   (lambda (entry)
                     (not (or (equal? (car entry) "hideThinkingBlock")
                            (equal? (car entry) 'hideThinkingBlock)
                          ) ;or
                     ) ;not
                   ) ;lambda
                   settings
                 ) ;filter
               ) ;cons
             ) ;updated
            ) ;
        (path-mkdir config-dir :parents #t :exist-ok #t)
        (path-write-text filepath (string-append (pi-config-json-pretty updated) "\n"))
        (and hide #t)
      ) ;let*
    ) ;define

    (define (pi-load-compaction-settings config-dir)
      (let* ((filepath (path-join config-dir "settings.json"))
             (obj (or (pi-read-json-file filepath) '()))
             (defaults (pi-compaction-default-settings))
             (node (json-dict-ref obj "compaction" '()))
             (get (lambda (key default) (json-dict-ref node key default)))
            ) ;
        (make-pi-compaction-settings (%normalize-bool (get "enabled" (pi-compaction-settings-enabled defaults)))
          (get "reserveTokens" (pi-compaction-settings-reserve-tokens defaults))
          (get "keepRecentTokens" (pi-compaction-settings-keep-recent-tokens defaults))
        ) ;make-pi-compaction-settings
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
