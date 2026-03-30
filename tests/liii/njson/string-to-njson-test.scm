(import (liii check)
        (liii base)
        (liii njson)
) ;import

(check-set-mode! 'report-failed)

;; string->njson
;; 将严格 JSON 字符串解析为 njson 句柄。
;;
;; 语法
;; ----
;; (string->njson json-string)
;;
;; 参数
;; ----
;; json-string : string
;; 严格 JSON 文本。
;;
;; 返回值
;; ----
;; njson-handle
;; 解析成功后返回可继续操作的句柄。
;;
;; 注意
;; ----
;; 解析失败会抛 parse-error，非字符串会抛 type-error。
;;
;; 错误处理
;; ----
;; parse-error
;; JSON 文本非法时抛出。
;; type-error
;; 输入不是字符串时抛出。

(define string-to-njson-sample
  "{\"name\":\"Goldfish\",\"active\":true,\"nums\":[1,2,3]}"
) ;define

(let-njson ((root (string->njson string-to-njson-sample)))
  (check (njson-ref root "name") => "Goldfish")
  (check-true (njson-contains-key? root "active"))
) ;let-njson

(check-catch 'parse-error (string->njson "{name:\"Goldfish\"}"))
(check-catch 'type-error (string->njson 1))

(check-report)
