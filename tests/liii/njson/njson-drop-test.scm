(import (liii check)
        (liii base)
        (liii error)
        (liii njson)
) ;import

(check-set-mode! 'report-failed)

;; njson-drop
;; 函数式删除 JSON 路径，返回新句柄。
;;
;; 语法
;; ----
;; (njson-drop json key ...)
;;
;; 参数
;; ----
;; json : njson-handle
;; key ... : string | integer
;;
;; 返回值
;; ----
;; njson-handle
;; 删除后的新句柄。
;;
;; 注意
;; ----
;; 对象字段缺失、数组越界或路径非法会抛 key-error。
;;
;; 错误处理
;; ----
;; type-error
;; json 非句柄或句柄已释放时抛出。
;; key-error
;; 路径不存在、token 类型不匹配或缺少路径参数时抛出。

(define sample-json
  "{\"name\":\"Goldfish\",\"version\":\"17.11.26\",\"active\":true,\"score\":3.14,\"nums\":[1,2,3,4,5],\"meta\":{\"arch\":\"x86_64\",\"os\":\"linux\"}}"
) ;define

(define (capture-key-error-message thunk)
  (catch 'key-error
    thunk
    (lambda args
      (let ((payload (if (and (pair? args) (pair? (cdr args))) (cadr args) '())))
        (if (and (pair? payload) (string? (car payload)))
            (car payload)
            ""
        ) ;if
      ) ;let
    ) ;lambda
  ) ;catch
) ;define

(let-njson ((root (string->njson sample-json))
            (root4 (njson-drop root "active")))
  (check-false (njson-contains-key? root4 "active"))
  (check (njson-ref root "active") => #t)
) ;let-njson

(let-njson ((arr (string->njson "[10,20,30]"))
            (arr2 (njson-drop arr 1)))
  (check (njson-ref arr2 0) => 10)
  (check (njson-ref arr2 1) => 30)
  (check (njson-size arr2) => 2)
  (check (njson-ref arr 1) => 20)
) ;let-njson

(check-catch 'type-error (njson-drop 'foo "active"))
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error (njson-drop root 'active))
  (check-catch 'key-error (njson-drop root "not-found"))
  (check-catch 'key-error (njson-drop root "meta" "not-found"))
  (check-catch 'key-error (njson-drop root "name" "as" "as"))
  (check (capture-key-error-message (lambda () (njson-drop root "not-found")))
         => "g_njson-drop: path not found: missing object key 'not-found'"
  ) ;check
  (check (capture-key-error-message (lambda () (njson-drop root "name" "as" "as")))
         => "g_njson-drop: path not found: missing object key 'as'"
  ) ;check
) ;let-njson
(let-njson ((arr (string->njson "[10,20,30]")))
  (check-catch 'key-error (njson-drop arr 3))
  (check (capture-key-error-message (lambda () (njson-drop arr 3)))
         => "g_njson-drop: path not found: array index out of range (index=3, size=3)"
  ) ;check
) ;let-njson

(check-report)
