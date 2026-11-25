;; check-free-variables
;; 检查确定可分析表达式中的未绑定标识符。
;;
;; 语法
;; ----
;; (check-free-variables file . roots)
;;
;; 参数
;; ----
;; file : string       要检查的 Scheme 文件路径
;; roots : list        可选的库搜索根目录
;;
;; 返回值
;; ------
;; violations : list   按文件、行号和标识符组织的警告

(import (scheme base)
  (scheme file)
  (liii list)
  (liii check)
  (liii path)
  (liii goldcheck free-variables)
) ;import

(check-set-mode! 'report-failed)

(define (write-test-file name content)
  (let ((file (path->string (path-join (path-temp-dir) name))))
    (path-write-bytes (path file) (string->utf8 content))
    file
  ) ;let
) ;define

(define (warning-names violations)
  (map cadddr violations)
) ;define

(let ((file (write-test-file "gf-free-variables.scm"
              (string-append "(define bound 1)\n" "(define (local arg)\n"
                "  (let ((a arg))\n" "    (let* ((b a))\n"
                "      (letrec ((c b))\n" "        (let-values (((d) c))\n"
                "          (let*-values (((e) d))\n"
                "            (case-lambda (() e))))))))\n"
                "(define record-ok (make-record 1))\n"
                "(define bad missing-name)\n" "(define quoted '(quoted-name))\n"
                "(define quasi `(data ,bound ,missing-quasi))\n"
                "(define macro-ok (define-syntax local-macro))\n"
                "(define* (keyword-ok x (y 2) :allow-other-keys) x)\n"
                "(define read-ok read)\n" "(define cdddr-ok cdddr)\n"
              ) ;string-append
            ) ;write-test-file
      ) ;file
     ) ;
  (let ((violations (check-free-variables file '())))
    (check (warning-names violations)
      =>
      '("make-record" "missing-name" "missing-quasi")
    ) ;check
  ) ;let
  (path-unlink file #t)
) ;let

(let ((file (write-test-file "gf-free-record.scm"
              (string-append "(define-record-type <record>\n"
                "  (make-record value)\n" "  record?\n"
                "  (fields (value record-value)))\n"
                "(define ok (make-record 1))\n"
                "(define ok2 (record-value (make-record 1)))\n"
              ) ;string-append
            ) ;write-test-file
      ) ;file
     ) ;
  (check (check-free-variables file '()) => '())
  (path-unlink file #t)
) ;let

(let ((file (write-test-file "gf-free-define-star.scm"
              "(define* (optional-arg x (y 1)) x y)\n"
            ) ;write-test-file
      ) ;file
     ) ;
  (check (check-free-variables file '()) => '())
  (path-unlink file #t)
) ;let

(let ((file (write-test-file "gf-free-internal-define.scm"
              (string-append "(define (outer x)\n"
                "  (define (helper y) y)\n"
                "  (helper x))\n"
              ) ;string-append
            ) ;write-test-file
      ) ;file
     ) ;
  (check (check-free-variables file '()) => '())
  (path-unlink file #t)
) ;let

(let ((file (write-test-file "gf-free-do.scm"
              "(define loop (lambda () (do ((i 0 (if #t i missing-step))) (#t i))))\n"
            ) ;write-test-file
      ) ;file
     ) ;
  (let ((violations (check-free-variables file '())))
    (check (warning-names violations) => '("missing-step"))
  ) ;let
  (path-unlink file #t)
) ;let

(let ((file (write-test-file "gf-free-quasiquote.scm"
              "(define quasi `(data ,missing-nested))\n"
            ) ;write-test-file
      ) ;file
     ) ;
  (let ((violations (check-free-variables file '())))
    (check (warning-names violations) => '("missing-nested"))
  ) ;let
  (path-unlink file #t)
) ;let

(check-report)
