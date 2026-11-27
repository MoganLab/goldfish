;; check-arity
;; 检查同文件已解析过程调用的实参数量；不确定的调用保持静默。
;;
;; 语法
;; ----
;; (check-arity file)
;;
;; 参数
;; ----
;; file : string 要检查的 Scheme 文件。
;;
;; 返回值
;; ------
;; violations : list 按文件、行号、过程名、实参数量和签名组织的警告。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii json)
  (liii goldcheck arity)
) ;import

(check-set-mode! 'report-failed)

(define (write-arity-fixture name content)
  (let ((file (path->string (path-join (path-temp-dir) name))))
    (path-write-bytes (path file) (string->utf8 content))
    file
  ) ;let
) ;define

(define (warning-names violations)
  (map (lambda (warning) (list-ref warning 3)) violations)
) ;define

(let ((file (write-arity-fixture "gf-check-arity-counts.scm"
              (string-append "(define (fixed a b) a)\n"
                "(define* (optional a (b 2)) b)\n" "(define (rest a . xs) xs)\n"
                "(define* (star-rest a :rest xs) xs)\n"
                "(define* (ambiguous :key value) value)\n"
                "(define gap (case-lambda (() 0) ((x) x) ((x y z) z)))\n"
                "(define (calls)\n" "  (fixed 1)\n" "  (fixed 1 2 3)\n"
                "  (optional)\n" "  (optional 1 2 3)\n" "  (rest)\n"
                "  (rest 1 2)\n" "  (star-rest)\n" "  (star-rest 1 2)\n"
                "  (ambiguous)\n" "  (gap 1 2))\n"
              ) ;string-append
            ) ;write-arity-fixture
      ) ;file
     ) ;
  (let ((violations (check-arity file)))
    (check (warning-names violations)
      =>
      '(fixed fixed optional optional rest star-rest gap)
    ) ;check
    (check (map (lambda (warning) (list-ref warning 4)) violations)
      =>
      '(1 3 0 3 0 0 2)
    ) ;check
  ) ;let
  (path-unlink file #t)
) ;let

(let ((file (write-arity-fixture "gf-check-arity-lambda.scm"
              (string-append "(define (outer)\n"
                "  (let ((local (lambda (x) x)))\n"
                "    (local)))\n"
              ) ;string-append
            ) ;write-arity-fixture
      ) ;file
     ) ;
  (check (warning-names (check-arity file)) => '(local))
  (path-unlink file #t)
) ;let

(let ((file (write-arity-fixture "gf-check-arity-internal.scm"
              (string-append "(define (known x) x)\n" "(define (outer)\n"
                "  (define (known x) x)\n" "  (known))\n"
              ) ;string-append
            ) ;write-arity-fixture
      ) ;file
     ) ;
  (check (warning-names (check-arity file)) => '(known))
  (path-unlink file #t)
) ;let

(let ((file (write-arity-fixture "gf-check-arity-skip.scm"
              (string-append "(define (known x) x)\n"
                "(define-syntax macro-target (syntax-rules () ((_ x) x)))\n"
                "(define (macro-target x) x)\n" "(define (calls)\n"
                "  (let ((known 1)) (known 2))\n"
                "  (set! known (lambda () 0))\n" "  (known)\n"
                "  (macro-target)\n" "  (unknown-macro (known))\n"
                "  (cond-expand (goldfish (known)) (else (known)))\n"
                "  (eval '(known))\n" "  (apply known '())\n"
                "  (known :value 1))\n"
              ) ;string-append
            ) ;write-arity-fixture
      ) ;file
     ) ;
  (check (check-arity file) => '())
  (path-unlink file #t)
) ;let

(let* ((file (write-arity-fixture "gf-check-arity-excluded.scm"
               "(define (fixed x) x)\n(define (calls) (fixed))\n"
             ) ;write-arity-fixture
       ) ;file
       (cfg (string->json "{\"rules\":{\"arity\":{\"enabled\":true}},\"excludes\":{\"arity\":[\"*gf-check-arity-excluded.scm\"]}}"
            ) ;string->json
       ) ;cfg
      ) ;
  (check (warning-names (check-arity file)) => '(fixed))
  (check (%check-file-rule-arity file cfg '()) => '())
  (path-unlink file #t)
) ;let*

(check-report)
