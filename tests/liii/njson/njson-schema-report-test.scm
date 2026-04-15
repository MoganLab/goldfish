(import (liii check)
  (liii base)
  (liii hash-table)
  (liii njson)
) ;import


(check-set-mode! 'report-failed)


;; njson-schema-report
;; 返回结构化 JSON Schema 校验报告。
;;
;; 语法
;; ----
;; (njson-schema-report schema-handle instance)
;;
;; 参数
;; ----
;; schema-handle : njson-handle
;; instance : njson-handle | string | number | boolean | 'null
;;
;; 返回值
;; ----
;; hash-table
;; 返回校验报告，包含 valid?、error-count 和 errors。
;;
;; 注意
;; ----
;; 失败场景也会返回报告，非法 schema 会抛 schema-error。
;;
;; 错误处理
;; ----
;; type-error
;; schema-handle 非句柄、句柄已释放或 instance 类型不支持时抛出。
;; schema-error
;; schema 本身非法时抛出。


(define schema-object-json
  "{\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\"},\"age\":{\"type\":\"integer\"}},\"required\":[\"name\"],\"additionalProperties\":false}"
) ;define
(define schema-instance-ok-json
  "{\"name\":\"Alice\",\"age\":18}"
) ;define
(define schema-instance-bad-type-json
  "{\"name\":\"Alice\",\"age\":\"18\"}"
) ;define
(define schema-instance-bad-missing-json
  "{\"age\":18}"
) ;define
(define schema-instance-bad-extra-json
  "{\"name\":\"Alice\",\"city\":\"HZ\"}"
) ;define
(define schema-instance-name-only-json
  "{\"name\":\"Alice\"}"
) ;define
(define schema-instance-array-json
  "[1,2,3]"
) ;define
(define schema-bad-handle-json
  "{\"type\":\"object\",\"required\":1}"
) ;define
(define schema-bad-non-object-json "1")
(define schema-array-items-int-json
  "{\"type\":\"array\",\"items\":{\"type\":\"integer\"}}"
) ;define
(define schema-array-ok-json "[1,2,3]")
(define schema-array-bad-json
  "[1,\"2\",3]"
) ;define
(define schema-scalar-int-json
  "{\"type\":\"integer\"}"
) ;define
(define schema-scalar-null-json
  "{\"type\":\"null\"}"
) ;define
(define schema-default-count-json
  "{\"type\":\"object\",\"properties\":{\"count\":{\"type\":\"integer\",\"default\":7}}}"
) ;define
(define schema-empty-object-json "{}")


(define (njson-schema-report-with-json schema-json
          instance-json
        ) ;njson-schema-report-with-json
  (let-njson ((schema (string->njson schema-json))
              (instance (string->njson instance-json))
             ) ;
    (njson-schema-report schema instance)
  ) ;let-njson
) ;define


(define (njson-schema-report-with-schema schema-json
          instance
        ) ;njson-schema-report-with-schema
  (let-njson ((schema (string->njson schema-json)))
    (njson-schema-report schema instance)
  ) ;let-njson
) ;define


(define (run-schema-report mode
          schema-input
          instance-input
        ) ;run-schema-report
  (if (eq? mode 'json)
    (njson-schema-report-with-json schema-input
      instance-input
    ) ;njson-schema-report-with-json
    (njson-schema-report-with-schema schema-input
      instance-input
    ) ;njson-schema-report-with-schema
  ) ;if
) ;define


(define (check-schema-report-shape report
          expected-valid
          expected-error-count
        ) ;check-schema-report-shape
  (check (hash-table-ref report 'valid?)
    =>
    expected-valid
  ) ;check
  (check (hash-table-ref report 'error-count)
    =>
    expected-error-count
  ) ;check
  (check (length (hash-table-ref report 'errors))
    =>
    expected-error-count
  ) ;check
) ;define


(define (check-schema-report-error error-entry
          expected-path
          expected-message
          expected-instance
        ) ;check-schema-report-error
  (check (hash-table-ref error-entry
           'instance-path
         ) ;hash-table-ref
    =>
    expected-path
  ) ;check
  (check (hash-table-ref error-entry 'message)
    =>
    expected-message
  ) ;check
  (check (hash-table-ref error-entry 'instance)
    =>
    expected-instance
  ) ;check
) ;define


(define (check-schema-report-invalid-min-errors report
          min-error-count
        ) ;check-schema-report-invalid-min-errors
  (check (hash-table-ref report 'valid?)
    =>
    #f
  ) ;check
  (check-true (>= (hash-table-ref report 'error-count)
                min-error-count
              ) ;>=
  ) ;check-true
  (check-true (>= (length (hash-table-ref report 'errors))
                min-error-count
              ) ;>=
  ) ;check-true
) ;define


(define (run-schema-report-shape-case case)
  (let* ((mode (list-ref case 0))
         (schema-input (list-ref case 1))
         (instance-input (list-ref case 2))
         (expected-valid (list-ref case 3))
         (expected-error-count (list-ref case 4))
         (report (run-schema-report mode
                   schema-input
                   instance-input
                 ) ;run-schema-report
         ) ;report
        ) ;
    (check-schema-report-shape report
      expected-valid
      expected-error-count
    ) ;check-schema-report-shape
    report
  ) ;let*
) ;define


(define (run-schema-report-error-case case)
  (let* ((schema-json (list-ref case 0))
         (instance-json (list-ref case 1))
         (expected-path (list-ref case 2))
         (expected-message (list-ref case 3))
         (expected-instance (list-ref case 4))
         (report (run-schema-report-shape-case (list 'json
                                                 schema-json
                                                 instance-json
                                                 #f
                                                 1
                                               ) ;list
                 ) ;run-schema-report-shape-case
         ) ;report
         (error-entry (car (hash-table-ref report 'errors))
         ) ;error-entry
        ) ;
    (check-schema-report-error error-entry
      expected-path
      expected-message
      expected-instance
    ) ;check-schema-report-error
  ) ;let*
) ;define


(define schema-report-shape-cases
  (list (list 'json
          schema-object-json
          schema-instance-ok-json
          #t
          0
        ) ;list
    (list 'json
      schema-object-json
      schema-instance-name-only-json
      #t
      0
    ) ;list
    (list 'json
      schema-array-items-int-json
      schema-array-ok-json
      #t
      0
    ) ;list
    (list 'schema
      schema-scalar-int-json
      18
      #t
      0
    ) ;list
    (list 'schema
      schema-scalar-null-json
      'null
      #t
      0
    ) ;list
    (list 'json
      schema-default-count-json
      schema-empty-object-json
      #t
      0
    ) ;list
    (list 'schema
      schema-scalar-int-json
      "18"
      #f
      1
    ) ;list
    (list 'schema
      schema-scalar-null-json
      0
      #f
      1
    ) ;list
  ) ;list
) ;define


(define schema-report-error-cases
  (list (list schema-object-json
          schema-instance-bad-type-json
          "/age"
          "unexpected instance type"
          "\"18\""
        ) ;list
    (list schema-object-json
      schema-instance-bad-missing-json
      ""
      "required property 'name' not found in object"
      "{\"age\":18}"
    ) ;list
    (list schema-object-json
      schema-instance-bad-extra-json
      ""
      "validation failed for additional property 'city': instance invalid as per false-schema"
      "{\"city\":\"HZ\",\"name\":\"Alice\"}"
    ) ;list
    (list schema-array-items-int-json
      schema-array-bad-json
      "/1"
      "unexpected instance type"
      "\"2\""
    ) ;list
  ) ;list
) ;define


(for-each run-schema-report-shape-case
  schema-report-shape-cases
) ;for-each
(for-each run-schema-report-error-case
  schema-report-error-cases
) ;for-each


(let ((instance-array-report (njson-schema-report-with-json schema-object-json
                               schema-instance-array-json
                             ) ;njson-schema-report-with-json
      ) ;instance-array-report
     ) ;
  (check-schema-report-invalid-min-errors instance-array-report
    1
  ) ;check-schema-report-invalid-min-errors
) ;let


(check-catch 'type-error
  (let-njson ((instance (string->njson schema-instance-ok-json)
              ) ;instance
             ) ;
    (njson-schema-report 'foo instance)
  ) ;let-njson
) ;check-catch
(check-catch 'type-error
  (njson-schema-report-with-schema schema-object-json
    'foo
  ) ;njson-schema-report-with-schema
) ;check-catch
(check-catch 'schema-error
  (njson-schema-report-with-json schema-bad-handle-json
    schema-instance-ok-json
  ) ;njson-schema-report-with-json
) ;check-catch
(check-catch 'schema-error
  (njson-schema-report-with-json schema-bad-non-object-json
    schema-instance-ok-json
  ) ;njson-schema-report-with-json
) ;check-catch


(define schema-handle-for-freed-check
  (string->njson schema-object-json)
) ;define
(define freed-instance-handle
  (string->njson "{\"name\":\"Bob\"}")
) ;define
(check-true (njson-free freed-instance-handle)
) ;check-true
(check-catch 'type-error
  (njson-schema-report schema-handle-for-freed-check
    freed-instance-handle
  ) ;njson-schema-report
) ;check-catch
(check-true (njson-free schema-handle-for-freed-check
            ) ;njson-free
) ;check-true


(define freed-schema-handle
  (string->njson schema-object-json)
) ;define
(check-true (njson-free freed-schema-handle)
) ;check-true
(check-catch 'type-error
  (njson-schema-report freed-schema-handle
    1
  ) ;njson-schema-report
) ;check-catch


(check-report)
