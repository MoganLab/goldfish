(import (liii check)
  (liii base)
  (rename (liii json)
    (json-object? ljson-object?)
  ) ;rename
  (liii njson)
) ;import


(check-set-mode! 'report-failed)


;; njson-object->alist
;; 把 njson object 递归转换为 alist/list 家族的纯 Scheme 结构。
;;
;; 语法
;; ----
;; (njson-object->alist object-json)
;;
;; 参数
;; ----
;; object-json : njson-handle
;; 必须是指向 JSON object 的句柄。
;;
;; 返回值
;; ----
;; alist
;; 返回纯 Scheme 的 alist/list 结构。
;;
;; 注意
;; ----
;; 空 object 使用 `'(())` 作为 canonical 表示。
;;
;; 错误处理
;; ----
;; type-error
;; 输入不是 object-handle 或句柄已释放时抛出。


(define njson-object->alist-json
  "{\"name\":\"Goldfish\",\"meta\":{\"os\":\"linux\",\"empty\":{}},\"nums\":[1,{\"deep\":true},[]],\"nil\":null}"
) ;define


(define object-as-alist '())
(let-njson ((root (string->njson njson-object->alist-json)
            ) ;root
           ) ;
  (set! object-as-alist
    (njson-object->alist root)
  ) ;set!
  (check (assoc "name" object-as-alist)
    =>
    '("name" . "Goldfish")
  ) ;check
  (let ((meta (cdr (assoc "meta" object-as-alist))
        ) ;meta
        (nums (cdr (assoc "nums" object-as-alist))
        ) ;nums
       ) ;
    (check (assoc "os" meta)
      =>
      '("os" . "linux")
    ) ;check
    (check (assoc "empty" meta)
      =>
      '("empty" ())
    ) ;check
    (check (car nums) => 1)
    (check (assoc "deep" (cadr nums))
      =>
      '("deep" . #t)
    ) ;check
    (check (caddr nums) => '())
  ) ;let
  (check (assoc "nil" object-as-alist)
    =>
    '("nil" . null)
  ) ;check
) ;let-njson
(let ((meta (cdr (assoc "meta" object-as-alist))
      ) ;meta
     ) ;
  (check (assoc "os" meta)
    =>
    '("os" . "linux")
  ) ;check
  (check (assoc "empty" meta)
    =>
    '("empty" ())
  ) ;check
  (check-true (ljson-object? (cdr (assoc "empty" meta))
              ) ;ljson-object?
  ) ;check-true
) ;let


(let-njson ((root (string->njson "{}")))
  (let ((empty-object (njson-object->alist root)
        ) ;empty-object
       ) ;
    (check empty-object => '(()))
    (check-true (ljson-object? empty-object)
    ) ;check-true
  ) ;let
) ;let-njson


(check-catch 'type-error
  (njson-object->alist 'foo)
) ;check-catch
(let-njson ((arr (string->njson "[1]")))
  (check-catch 'type-error
    (njson-object->alist arr)
  ) ;check-catch
) ;let-njson
(define object->alist-freed
  (string->njson "{\"a\":1}")
) ;define
(check-true (njson-free object->alist-freed)
) ;check-true
(check-catch 'type-error
  (njson-object->alist object->alist-freed
  ) ;njson-object->alist
) ;check-catch


(check-report)
