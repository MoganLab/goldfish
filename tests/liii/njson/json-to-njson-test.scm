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


;; json->njson
;; 将 liii json 值或 strict JSON 标量转换为 njson 句柄。
;;
;; 语法
;; ----
;; (json->njson value)
;;
;; 参数
;; ----
;; value : any
;; liii json object/array 或 strict JSON 标量。
;;
;; 返回值
;; ----
;; njson-handle
;; 转换后的句柄。
;;
;; 注意
;; ----
;; 对 object/array 先转字符串再解析，对标量直接直通处理。
;;
;; 错误处理
;; ----
;; type-error
;; 输入不是支持范围内的 liii json 值时抛出。


(define ljson-bridge-sample
  (ljson-string->json "{\"name\":\"Goldfish\",\"nums\":[1,2,3]}"
  ) ;ljson-string->json
) ;define
(define ljson-bridge-array
  (ljson-string->json "[1,2,3]")
) ;define


(let-njson ((bridge-handle (json->njson ljson-bridge-sample)
            ) ;bridge-handle
           ) ;
  (check (njson-ref bridge-handle "name")
    =>
    "Goldfish"
  ) ;check
  (check (njson-ref bridge-handle "nums" 2)
    =>
    3
  ) ;check
) ;let-njson


(let-njson ((bridge-array (json->njson ljson-bridge-array)
            ) ;bridge-array
           ) ;
  (check (njson-ref bridge-array 1) => 2)
) ;let-njson


(let-njson ((null-handle (json->njson 'null))
            (int-handle (json->njson 7))
            (string-handle (json->njson "abc"))
            (bool-handle (json->njson #f))
           ) ;
  (check-true (njson-null? null-handle))
  (check (njson->string int-handle)
    =>
    "7"
  ) ;check
  (check (njson->string string-handle)
    =>
    "\"abc\""
  ) ;check
  (check (njson->string bool-handle)
    =>
    "false"
  ) ;check
) ;let-njson


(check-catch 'type-error
  (json->njson 'foo)
) ;check-catch


(check-report)
