;; validate-format
;; 校验 format 关键字并返回错误列表。
;;
;; 语法
;; ----
;; (validate-format validator format instance schema path schema-path)
;;
;; 参数
;; ----
;; validator : validator
;; 当前校验器，提供 formats 与 format-checkers 选项。
;;
;; format : string | symbol
;; 格式名称。
;;
;; instance : any?
;; 待校验的实例。
;;
;; schema : any?
;; 实例所属的 Schema 对象。
;;
;; path : list
;; 实例在根资源中的路径段列表。
;;
;; schema-path : list
;; 当前 Schema 在根资源中的路径段列表。
;;
;; 返回值
;; ----
;; list
;; 校验失败时返回错误列表，通过时返回空列表。
;;
;; 注意
;; ----
;; 实例不是字符串时不参与格式检查；
;; 未知格式名只作为注解，不产生错误；
;; 自定义检查器优先于内置检查器。
;;
;; 错误处理
;; ----
;; format-checkers 中的检查器不是过程时抛出 json-schema-options-error。

(import (liii check)
  (liii json schema error)
  (liii json schema format)
  (liii json schema model)
) ;import

(check-set-mode! 'report-failed)

;; 自定义检查器是一 个从字符串到布尔值的过程。

(define (has-at value)
  (and (string? value)
    (>= (string-length value) 5)
    (char=? (string-ref value 4) #\@)
  ) ;and
) ;define

(define email-checkers (list (cons 'email has-at)))

(define email-schema '(("format" . "email")))

(define other-schema '(("format" . "not-a-known-format")))

(define (make-format-validator options)
  (make-validator email-schema options)
) ;define

;; 未开启 formats 时 format 只作为注解。

(check (length (validate-format (make-format-validator '())
                 "email"
                 "2962"
                 email-schema
                 '()
                 '()
               ) ;validate-format
       ) ;length
  =>
  0
) ;check

;; 非字符串实例不参与格式检查。

(check
  (length
    (validate-format (make-format-validator '((formats email)))
      "email"
      12
      email-schema
      '()
      '()
    ) ;validate-format
  ) ;length
  =>
  0
) ;check

;; formats 选项开启后只有列出的格式参与断言。

(check
  (length
    (validate-format (make-format-validator '((formats email)))
      "email"
      "2962"
      email-schema
      '()
      '()
    ) ;validate-format
  ) ;length
  =>
  1
) ;check
(check
  (length
    (validate-format (make-format-validator '((formats hostname)))
      "email"
      "2962"
      email-schema
      '()
      '()
    ) ;validate-format
  ) ;length
  =>
  0
) ;check

;; 自定义检查器按名称生效。

(check
  (length
    (validate-format
      (make-format-validator `((formats email)
                               (format-checkers . ,email-checkers)))
      "email"
      "user@example.com"
      email-schema
      '()
      '()
    ) ;validate-format
  ) ;length
  =>
  0
) ;check

;; formats 为 #t 时断言所有已知格式。

(check
  (length
    (validate-format (make-format-validator '((formats . #t)))
      "email"
      "2962"
      email-schema
      '()
      '()
    ) ;validate-format
  ) ;length
  =>
  1
) ;check

;; 未知格式名不产生错误。

(check
  (length
    (validate-format (make-format-validator '((formats . #t)))
      "not-a-known-format"
      "anything"
      other-schema
      '()
      '()
    ) ;validate-format
  ) ;length
  =>
  0
) ;check

;; 内置检查器可以通过 formats 选项使用。

(check
  (length
    (validate-format (make-format-validator '((formats date)))
      "date"
      "2020-01-01"
      '(("format" . "date"))
      '()
      '()
    ) ;validate-format
  ) ;length
  =>
  0
) ;check
(check
  (length
    (validate-format (make-format-validator '((formats date)))
      "date"
      "2020-13-01"
      '(("format" . "date"))
      '()
      '()
    ) ;validate-format
  ) ;length
  =>
  1
) ;check

;; 错误对象携带关键字与路径信息。

(define error
  (car
    (validate-format (make-format-validator '((formats date)))
      "date"
      "2020-13-01"
      '(("format" . "date"))
      '(("items" 0))
      '("root")
    ) ;validate-format
  ) ;car
) ;define
(check (json-schema-error-keyword error) => "format")
(check (json-schema-error-keyword-value error) => "date")
(check (json-schema-error-path error) => '(("items" 0)))
(check (json-schema-error-schema-path error) => '("root" "format"))

;; 嵌套实例的格式错误路径逐段保留。

(define nested-error
  (car
    (validate-format (make-format-validator '((formats date)))
      "date"
      "2020-13-01"
      '(("properties" ("a" ("format" . "date"))))
      '("a")
      '()
    ) ;validate-format
  ) ;car
) ;define
(check (json-schema-error-keyword nested-error) => "format")
(check (json-schema-error-path nested-error) => '("a"))

(check-report)
