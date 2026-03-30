(import (liii check)
        (liii base)
        (liii error)
        (rename (liii json)
                (string->json ljson-string->json)
                (json-object? ljson-object?)
                (json-ref ljson-ref)
        ) ;rename
        (liii njson)
) ;import

(check-set-mode! 'report-failed)

;; njson->json
;; 将 njson 句柄或 strict JSON 标量转换为 liii json 值。
;;
;; 语法
;; ----
;; (njson->json value)
;;
;; 参数
;; ----
;; value : njson-handle | string | number | boolean | 'null
;;
;; 返回值
;; ----
;; any?
;; 转换后的 liii json 值。
;;
;; 注意
;; ----
;; 已释放句柄会抛 type-error。
;;
;; 错误处理
;; ----
;; type-error
;; 输入不是支持范围内的 njson 值时抛出。

(let-njson ((bridge-handle (string->njson "{\"name\":\"Goldfish\",\"active\":true,\"nums\":[1,2,3]}")))
  (let ((ljson-val (njson->json bridge-handle)))
    (check-true (ljson-object? ljson-val))
    (check (ljson-ref ljson-val "name") => "Goldfish")
    (check (ljson-ref ljson-val "active") => #t)
    (check (ljson-ref ljson-val "nums" 1) => 2)
  ) ;let
) ;let-njson

(check (njson->json 'null) => 'null)
(check (njson->json 7) => 7)
(check (njson->json "abc") => "abc")
(check (njson->json #f) => #f)

(check-catch 'type-error (njson->json 'foo))

(define njson->json-freed (string->njson "{\"a\":1}"))
(check-true (njson-free njson->json-freed))
(check-catch 'type-error (njson->json njson->json-freed))

(check-report)
