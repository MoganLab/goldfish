(import (liii check)
        (liii base)
        (rename (liii json)
                (json-object? ljson-object?)
        ) ;rename
        (liii njson)
) ;import

(check-set-mode! 'report-failed)

;; njson-array->list
;; 把 njson array 递归转换为 list/alist 家族的纯 Scheme 结构。
;;
;; 语法
;; ----
;; (njson-array->list array-json)
;;
;; 参数
;; ----
;; array-json : njson-handle
;; 必须是指向 JSON array 的句柄。
;;
;; 返回值
;; ----
;; list
;; 返回纯 Scheme 的 list/alist 结构。
;;
;; 注意
;; ----
;; 空 object 保持 canonical 表示 `'(())`，避免与空 array `()` 混淆。
;;
;; 错误处理
;; ----
;; type-error
;; 输入不是 array-handle 或句柄已释放时抛出。

(define njson-array->list-json
  "[1,{\"name\":\"Goldfish\",\"tags\":[\"a\",\"b\"]},[2,{\"k\":null}],[]]"
) ;define
(define njson-array->list-expected
  '(1
    (("name" . "Goldfish") ("tags" . ("a" "b")))
    (2 (("k" . null)))
    ())
) ;define

(define array-as-list '())
(let-njson ((root (string->njson njson-array->list-json)))
  (set! array-as-list (njson-array->list root))
  (check array-as-list => njson-array->list-expected)
) ;let-njson
(check array-as-list => njson-array->list-expected)

(let-njson ((root (string->njson "[]")))
  (check (njson-array->list root) => '())
) ;let-njson

(let-njson ((root (string->njson "[{},[]]")))
  (let ((shape-list (njson-array->list root)))
    (check (car shape-list) => '(()))
    (check (cadr shape-list) => '())
    (check-true (ljson-object? (car shape-list)))
    (check (ljson-object? (cadr shape-list)) => #f)
  ) ;let
) ;let-njson

(check-catch 'type-error (njson-array->list 'foo))
(let-njson ((obj (string->njson "{\"a\":1}")))
  (check-catch 'type-error (njson-array->list obj))
) ;let-njson
(define array->list-freed (string->njson "[1]"))
(check-true (njson-free array->list-freed))
(check-catch 'type-error (njson-array->list array->list-freed))

(check-report)
