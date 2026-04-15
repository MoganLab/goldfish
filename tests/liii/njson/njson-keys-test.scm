(import (liii check)
  (liii base)
  (liii njson)
) ;import


(check-set-mode! 'report-failed)


;; njson-keys
;; 获取对象的所有键名列表，并跟随可变操作刷新缓存。
;;
;; 语法
;; ----
;; (njson-keys json)
;;
;; 参数
;; ----
;; json : njson-handle
;;
;; 返回值
;; ----
;; list
;; 对象时返回字符串键列表，非对象时返回空表。
;;
;; 注意
;; ----
;; set!/drop!/merge!/deep-merge! 后缓存会失效并重建。
;;
;; 错误处理
;; ----
;; type-error
;; json 非句柄或句柄已释放时抛出。


(define sample-json
  "{\"name\":\"Goldfish\",\"version\":\"17.11.26\",\"active\":true,\"score\":3.14,\"nums\":[1,2,3,4,5],\"meta\":{\"arch\":\"x86_64\",\"os\":\"linux\"}}"
) ;define


(define (string-list-contains? s xs)
  (cond ((null? xs) #f)
        ((string=? s (car xs)) #t)
        (else (string-list-contains? s (cdr xs))
        ) ;else
  ) ;cond
) ;define


(let-njson ((root (string->njson sample-json)))
  (check-true (> (length (njson-keys root)) 0)
  ) ;check-true
  (check-true (string-list-contains? "active"
                (njson-keys root)
              ) ;string-list-contains?
  ) ;check-true
  (njson-drop! root "active")
  (check-false (string-list-contains? "active"
                 (njson-keys root)
               ) ;string-list-contains?
  ) ;check-false
  (njson-set! root "active" #t)
  (check-true (string-list-contains? "active"
                (njson-keys root)
              ) ;string-list-contains?
  ) ;check-true
  (njson-set! root "new-key" 1)
  (check-true (string-list-contains? "new-key"
                (njson-keys root)
              ) ;string-list-contains?
  ) ;check-true
) ;let-njson


(let-njson ((root (string->njson sample-json)))
  (njson-keys root)
  (njson-drop! root "active")
  (njson-set! root "lazy-key" 1)
  (let ((keys (njson-keys root)))
    (check-false (string-list-contains? "active" keys)
    ) ;check-false
    (check-true (string-list-contains? "lazy-key" keys)
    ) ;check-true
    (check-true (string-list-contains? "name" keys)
    ) ;check-true
  ) ;let
  (let ((keys2 (njson-keys root)))
    (check-false (string-list-contains? "active" keys2)
    ) ;check-false
    (check-true (string-list-contains? "lazy-key" keys2)
    ) ;check-true
    (check-true (string-list-contains? "name" keys2)
    ) ;check-true
  ) ;let
) ;let-njson


(let-njson ((arr (string->njson "[1,2]"))
            (scalar (string->njson "1"))
            (empty-obj (string->njson "{}"))
           ) ;
  (check (njson-keys arr) => '())
  (check (njson-keys scalar) => '())
  (check (njson-keys empty-obj) => '())
) ;let-njson


(check-catch 'type-error
  (njson-keys 'foo)
) ;check-catch


(define njson-keys-freed
  (string->njson "{\"k\":1}")
) ;define
(check-true (njson-free njson-keys-freed)
) ;check-true
(check-catch 'type-error
  (njson-keys njson-keys-freed)
) ;check-catch


(check-report)
