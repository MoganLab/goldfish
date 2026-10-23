(import (scheme base) (liii check) (liii json) (liii goldcheck config))

;; exclude-entry->path
;; 将 exclude 配置项统一转换为路径模式字符串。
;;
;; 语法
;; ----
;; (exclude-entry->path entry)
;;
;; 参数
;; ----
;; entry : string | json-object
;; 字符串或包含 "path" 字段的 JSON 对象。
;;
;; 返回值
;; ------
;; string
;; 转换得到的路径字符串。
;;
;; 错误
;; ----
;; type-error: 当 entry 既非字符串也非对象，或对象的 path 字段非字符串时抛出。

(check-set-mode! 'report-failed)

;; 1. 字符串条目直接返回
(check (exclude-entry->path "goldfish/srfi/") => "goldfish/srfi/")

;; 2. 对象条目提取 path 字段
(let ((obj (string->json "{\"path\": \"goldfish/liii/go.scm\", \"reason\": \"协程\"}")
      ) ;obj
     ) ;
  (check (exclude-entry->path obj) => "goldfish/liii/go.scm")
) ;let

;; 3. 非字符串且非对象条目抛出 type-error
(check-catch 'type-error (exclude-entry->path 12345))
(check-catch 'type-error (exclude-entry->path #t))

;; 4. 对象条目的 path 字段非字符串抛出 type-error
(check-catch 'type-error (exclude-entry->path (string->json "{\"path\": 123}")))

(check-report)
