;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; provider 层配置读取：
;;   providers.json           provider id 白名单（唯一致命文件）
;;   models/<provider>.json   provider 层：name/baseUrl/api/httpProxy/models
(define-library (liii pi code config user-providers)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii list)
    (liii string)
    (liii path)
    (liii json)
    (liii pi code config user-fs)
  ) ;import
  (export pi-load-providers-whitelist pi-user-provider-file pi-user-normalize-api)
  (begin

    ;; 白名单元素校验：非字符串直接抛错
    (define (%whitelist-entry x)
      (if (string? x)
        x
        (error 'type-error "providers.json: 元素必须是字符串")
      ) ;if
    ) ;define

    ;; 重复 id 检测（保持文件内顺序）
    (define (%ensure-unique ids)
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
    ) ;define

    (define (%parse-whitelist data)
      (cond ((not (vector? data))
             (error 'value-error "providers.json: 顶层必须是 json array")
            ) ;
            (else (%ensure-unique (map %whitelist-entry (vector->list data))))
      ) ;cond
    ) ;define

    ;; 读取白名单；文件缺失返回 #f（回退全部内置 provider）。
    ;; 语法错（string->json 抛 parse-error）、元素非字符串、重复 id 均抛错拒启。
    (define (pi-load-providers-whitelist config-dir)
      (let* ((path-str (pi-user-config-path config-dir "providers.json"))
             (data
               (and (file-exists? path-str) (string->json (path-read-text (path path-str))))
             ) ;data
            ) ;
        (and data (%parse-whitelist data))
      ) ;let*
    ) ;define

    ;; api 简写归一化：anthropic/openai/google -> 全名
    (define (pi-user-normalize-api api-str)
      (cond ((string=? api-str "anthropic") "anthropic-messages")
            ((string=? api-str "openai") "openai-completions")
            ((string=? api-str "google") "google-generative-ai")
            (else api-str)
      ) ;cond
    ) ;define

    ;; 读 models/<id>.json，返回字段 alist；缺失返回 #f，语法错告警后返回 #f
    (define (pi-user-provider-file config-dir p-id)
      (let* ((path-str (pi-user-config-path config-dir "models" (string-append p-id ".json"))
             ) ;path-str
             (exists (file-exists? path-str))
             (data (pi-user-read-json path-str))
            ) ;
        (cond ((pair? data) data)
              (exists (pi-user-warn (string-append path-str ": JSON 解析失败，已忽略该文件")
                      ) ;pi-user-warn
                #f
              ) ;exists
              (else #f)
        ) ;cond
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
