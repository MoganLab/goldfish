;; 对 ~/.gf/agent/ 下的新格式配置文件做 JSON Schema 校验
;;
;; 用法：bin/gf demo/json-schema/check.scm [config-dir]
;; config-dir 缺省 ~/.gf/agent；schema 文件位于 tools/pi/liii/pi/code/resources/
;;
;; 校验对象：
;;   providers.json          -> providers.schema.json
;;   <provider>.json         -> provider.schema.json（排除 providers.json）
;;   <provider>/<model>.json -> model.schema.json
;;   auth.json               -> auth.schema.json
;;
;; 模型文件为可选（无任何 provider/模型文件时仅校验 providers.json 与 auth.json）。

(import (liii base)
  (liii list)
  (liii sort)
  (liii os)
  (liii path)
  (liii path stat)
  (liii string)
  (srfi srfi-13)
  (liii json)
  (liii json schema)
  (scheme process-context)
) ;import

(define REPO-DIR
  (if (file-exists? "tools/pi/liii/pi/code/resources")
    "."
    (string->immutable-string (path->string (path-parent (current-directory))))
  ) ;if
) ;define

(define SCHEMA-DIR
  (path-join REPO-DIR "tools" "pi" "liii" "pi" "code" "resources")
) ;define

;; command-line 形如 ("bin/gf" "demo/json-schema/check.scm" "<config-dir>")

(define CONFIG-DIR
  (let ((arg (and (> (length (command-line)) 2) (caddr (command-line)))))
    (if arg (path arg) (path-join (path-home) ".gf" "agent"))
  ) ;let
) ;define

;; ---------- 工具函数 ----------

(define (%read-json filepath)
  (string->json (path-read-text filepath))
) ;define

(define (%valid? instance schema)
  (json-schema-valid? instance schema)
) ;define

(define (%error-list instance schema)
  (json-schema-errors instance schema)
) ;define

(define (pass msg)
  (display "  [OK]   ")
  (display msg)
  (newline)
) ;define

(define (fail msg errs)
  (display "  [FAIL] ")
  (display msg)
  (newline)
  (for-each (lambda (e) (display "         ") (display e) (newline)) errs)
) ;define

;; 校验单个文件：文件路径、相对显示名、schema

(define (check-one filepath label schema)
  (let ((instance (catch #t (lambda () (%read-json filepath)) (lambda args #f))))
    (if (not instance)
      (fail (string-append label "：JSON 语法错误或不可读") '())
      (let ((errs (%error-list instance schema)))
        (if (null? errs) (pass label) (fail label errs))
      ) ;let
    ) ;if
  ) ;let
) ;define

;; ---------- 加载 schema ----------

(display "schema 目录：")
(display (path->string SCHEMA-DIR))
(newline)

(define providers-schema
  (string->json (path-read-text (path-join SCHEMA-DIR "providers.schema.json")))
) ;define

(define provider-schema
  (string->json (path-read-text (path-join SCHEMA-DIR "provider.schema.json")))
) ;define

(define model-schema
  (string->json (path-read-text (path-join SCHEMA-DIR "model.schema.json")))
) ;define

(define auth-schema
  (string->json (path-read-text (path-join SCHEMA-DIR "auth.schema.json")))
) ;define

;; 对每个 schema 自检（元 schema 校验）
(for-each
  (lambda (pair)
    (let ((name (car pair)) (schema (cdr pair)))
      (catch #t
        (lambda ()
          (json-schema-check-schema schema)
          (pass (string-append name " schema 自检"))
        ) ;lambda
        (lambda args (fail (string-append name " schema 自检") (list (cadr args))))
      ) ;catch
    ) ;let
  ) ;lambda
  (list (cons "providers.schema.json" providers-schema)
    (cons "provider.schema.json" provider-schema)
    (cons "model.schema.json" model-schema)
    (cons "auth.schema.json" auth-schema)
  ) ;list
) ;for-each

;; ---------- 校验配置 ----------

(display "配置目录：")
(display (path->string CONFIG-DIR))
(newline)
(newline)

;; 1. providers.json
(let ((f (path-join CONFIG-DIR "providers.json")))
  (if (path-exists? f)
    (check-one f "providers.json" providers-schema)
    (display "  [SKIP] providers.json 不存在（默认全部内置 provider）\n")
  ) ;if
) ;let

(newline)

;; 2. auth.json
(let ((f (path-join CONFIG-DIR "auth.json")))
  (if (path-exists? f)
    (check-one f "auth.json" auth-schema)
    (display "  [SKIP] auth.json 不存在\n")
  ) ;if
) ;let

(newline)

;; 3. models/ 下的 <provider>.json 与 <provider>/<model>.json
;;    providers.json 留在 config-dir 顶层；provider 与模型文件统一在 models/ 子目录

(define MODELS-DIR (path-join CONFIG-DIR "models"))

(define (dir-files dir)
  (if (path-exists? dir)
    (list-sort string<? (vector->list (listdir (path->string dir))))
    '()
  ) ;if
) ;define

;; models/ 下的 provider 文件
(for-each
  (lambda (name)
    (let ((f (path-join MODELS-DIR name)))
      (when
        (and (string-suffix? ".json" name)
          (path-stat-file? (path-stat (path->string f)))
        ) ;and
        (check-one f (string-append "models/" name) provider-schema)
      ) ;when
    ) ;let
  ) ;lambda
  (dir-files MODELS-DIR)
) ;for-each

(newline)

;; models/<provider>/ 下的模型文件
(for-each
  (lambda (name)
    (let ((sub (path-join MODELS-DIR name)))
      (when (path-stat-dir? (path-stat (path->string sub)))
        (for-each
          (lambda (m)
            (when (string-suffix? ".json" m)
              (check-one (path-join sub m) (string-append "models/" name "/" m) model-schema)
            ) ;when
          ) ;lambda
          (dir-files sub)
        ) ;for-each
      ) ;when
    ) ;let
  ) ;lambda
  (dir-files MODELS-DIR)
) ;for-each

(newline)
(display "校验完成\n")
