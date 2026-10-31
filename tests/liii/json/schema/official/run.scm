(import (liii json schema)
  (liii json schema data)
  (liii json)
  (liii check)
  (scheme base)
  (scheme file)
  (scheme write)
) ;import

(check-set-mode! 'report-failed)


(define groups
  '("additionalProperties" "allOf" "anchor" "anyOf" "boolean_schema" "const"
    "contains" "content" "default" "defs" "dependentRequired" "dependentSchemas"
    "dynamicRef" "enum" "exclusiveMaximum" "exclusiveMinimum" "format"
    "if-then-else" "infinite-loop-detection" "items" "maxContains" "maximum"
    "maxItems" "maxLength" "maxProperties" "minContains" "minimum" "minItems"
    "minLength" "minProperties" "multipleOf" "not" "oneOf" "pattern"
    "patternProperties" "prefixItems" "properties" "propertyNames" "ref"
    "refRemote" "required" "type" "unevaluatedItems" "unevaluatedProperties"
    "uniqueItems" "vocabulary")
) ;define

(define supported
  (let loop
    ((rest groups) (enabled '()))
    (if (null? rest)
      (reverse enabled)
      (loop (cdr rest)
        (if (member (car rest)
                '("vocabulary"
                  ;; pattern 类关键字依赖正则引擎，无原生后端，按注解忽略
                  "pattern" "patternProperties" "propertyNames")
            ) ;member
          enabled
          (cons (car rest) enabled)
        ) ;if
      ) ;loop
    ) ;if
  ) ;let
) ;define

(define optional-supported
  '("anchor" "bignum" "dynamicRef" "float-overflow" "id" "no-schema"
    "non-bmp-regex" "refOfUnknownKeyword" "unknownKeyword")
) ;define

;; Format groups assert the format keyword, so they need the formats option.

(define format-supported
  (append '("date" "date-time" "duration" "email" "idn-email" "ipv4" "ipv6")
    '("json-pointer" "relative-json-pointer" "time" "uri"
      "uri-reference")
  ) ;append
) ;define

(define format-groups
  '("date" "date-time" "duration" "email" "hostname" "idn-email" "idn-hostname"
    "ipv4" "ipv6" "json-pointer" "regex" "relative-json-pointer" "time" "uri"
    "uri-reference")
) ;define

(define optional-groups
  '("anchor" "bignum" "cross-draft" "dependencies-compatibility" "dynamicRef"
    "ecmascript-regex" "float-overflow" "format-assertion" "id" "no-schema"
    "non-bmp-regex" "refOfUnknownKeyword" "unknownKeyword")
) ;define

(define passed 0)

(define failed 0)

(define skipped 0)

(define (read-json path)
  (call-with-input-file path
    (lambda (port)
      (let loop
        ((chars '()))
        (let ((char (read-char port)))
          (if (eof-object? char)
            (string->json (list->string (reverse chars)))
            (loop (cons char chars))
          ) ;if
        ) ;let
      ) ;let
    ) ;lambda
  ) ;call-with-input-file
) ;define

;; Offline remote resources keep the runner deterministic and avoid network I/O.

(define remote-dir "tests/liii/json/schema/official/remotes/draft2020-12/")

(define remote-base "http://localhost:1234/draft2020-12/")

(define (remote-file-name uri)
  (and (>= (string-length uri) (string-length remote-base))
    (string=? (substring uri 0 (string-length remote-base)) remote-base)
    (substring uri (string-length remote-base) (string-length uri))
  ) ;and
) ;define

(define (retrieve-remote uri)
  (let ((name (remote-file-name uri)))
    (if (not name)
      (error 'json-schema-unresolvable uri)
      (let ((path (string-append remote-dir name)))
        (cond ((file-exists? path) (read-json path))
              ((file-exists? (string-append path ".json"))
               (read-json (string-append path ".json"))
              ) ;
              (else (error 'json-schema-unresolvable uri))
        ) ;cond
      ) ;let
    ) ;if
  ) ;let
) ;define

(define remote-options `((retrieve . ,retrieve-remote)))

(define format-options `(,@remote-options (formats . #t)))

;; 依赖正则引擎的用例：pattern/patternProperties/propertyNames 按注解
;; 忽略（无原生正则后端，见偏差记录），这些用例逐例记为 skipped。
(define regex-dependent-cases
  '(("additionalProperties" . "patternProperties are not additional properties")
    ("additionalProperties" . "matching the pattern is valid")
    ("properties" . "patternProperty invalidates property")
    ("properties" . "patternProperty validates nonproperty")
    ("unevaluatedProperties" . "with no unevaluated properties")
    ("unevaluatedProperties" . "with no additional properties")
    ("unevaluatedProperties" . "valid in case if is evaluated")
    ("unevaluatedProperties" . "xx is valid")
    ("unevaluatedProperties" . "xx + foox is valid")
    ("non-bmp-regex" . "doesn't match one")
    ("non-bmp-regex" . "doesn't match one ASCII")
    ("non-bmp-regex" . "doesn't match two")
    ("non-bmp-regex" . "doesn't match two ASCII"))
) ;define

(define (regex-dependent? group description)
  (and (member (cons group description) regex-dependent-cases) #t)
) ;define

(define (record-test group schema test options)
  (display "TEST ")
  (display (object-ref test "description" ""))
  (newline)
  (let ((instance (object-ref test "data" #f))
        (expected (json-true? (object-ref test "valid" #f)))
       ) ;
    (guard
      (condition (#t
                   (set! failed (+ failed 1))
                   (display "Official exception: ")
                   (display (object-ref test "description" ""))
                   (display ": ")
                   (write condition)
                   (display " Schema=")
                   (write schema)
                   (newline)
                   (check-true #f)
                 ) ;
      ) ;condition
      (let ((actual (json-schema-valid? instance schema options)))
        (if (eq? actual expected)
          (set! passed (+ passed 1))
          (begin
            (set! failed (+ failed 1))
            (display "Official mismatch: ")
            (display (object-ref test "description" ""))
            (display ", actual=")
            (display actual)
            (display ", expected=")
            (display expected)
            (display ", schema=")
            (write schema)
            (newline)
          ) ;begin
        ) ;if
        (check actual => expected)
      ) ;let
    ) ;guard
  ) ;let
) ;define

(define (group-path name optional? format?)
  (string-append "tests/liii/json/schema/official/draft2020-12/"
    (if optional? "optional/" "")
    (if format? "format/" "")
    name
    ".json"
  ) ;string-append
) ;define

(define (run-group name enabled? optional? format? options)
  (display "RUN ")
  (display name)
  (newline)
  (let* ((path (group-path name optional? format?)) (groups-data (read-json path)))
    (for-each
      (lambda (group)
        (let ((schema (object-ref group "schema" #f)) (tests (object-ref group "tests" #())))
          (for-each
            (lambda (test)
              (cond ((not enabled?) (set! skipped (+ skipped 1)))
                    ((regex-dependent? name (object-ref test "description" ""))
                     (set! skipped (+ skipped 1)))
                    (else (record-test name schema test options)))
            ) ;lambda
            (vector->list tests)
          ) ;for-each
        ) ;let
      ) ;lambda
      (vector->list groups-data)
    ) ;for-each
  ) ;let*
) ;define

(for-each
  (lambda (name)
    (run-group name (and (member name supported) #t) #f #f remote-options)
  ) ;lambda
  groups
) ;for-each
(for-each
  (lambda (name)
    (run-group name (and (member name optional-supported) #t) #t #f remote-options)
  ) ;lambda
  optional-groups
) ;for-each
(for-each
  (lambda (name)
    (run-group name (and (member name format-supported) #t) #t #t format-options)
  ) ;lambda
  format-groups
) ;for-each
(display "Official draft2020-12: passed=")
(display passed)
(display ", failed=")
(display failed)
(display ", skipped=")
(display skipped)
(newline)
(check-report)
