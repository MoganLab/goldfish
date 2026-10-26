(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii os)
  (liii json)
  (liii goldcheck)
) ;import

;; check-file
;; 对单个文件执行所有启用的规则检查，收集违规项。若文件命中 exclude 白名单则跳过并返回空列表。
;;
;; 语法
;; ----
;; (check-file file-path cfg)
;;
;; 参数
;; ----
;; file-path : string
;; 待检查的文件路径。
;; cfg : json-object 或 #f
;; 检查配置。
;;
;; 返回值
;; ------
;; list of violations
;; 违规项列表。无违规或被排除时返回 '()。

(check-set-mode! 'report-failed)

;; 临时文件一律用 path-write-bytes 二进制写入，精确控制 LF 行尾，
;; 避免文本端口在 Windows 上把 \n 转写为 \r\n。
(define (write-test-bytes tmp content)
  (path-write-bytes (path tmp) (string->utf8 content))
) ;define

(define (repeat-lines n)
  (let loop
    ((i 0) (acc '()))
    (if (>= i n)
      (apply string-append (reverse acc))
      (loop (+ i 1) (cons "(define x 1)\n" acc))
    ) ;if
  ) ;let
) ;define

;; 1. 干净文件检查通过返回 '()
(let* ((tmp (path->string (path-join (path-temp-dir) "gf-test-clean.scm"))))
  (write-test-bytes tmp "(define a 1)\n(define b 2)\n")
  (check (check-file tmp #f) => '())
  (path-unlink tmp #t)
) ;let*

;; 2. 超限文件返回违规项
(let* ((tmp (path->string (path-join (path-temp-dir) "gf-test-lines-viol.scm"))))
  (write-test-bytes tmp (repeat-lines 405))
  (let ((viols (check-file tmp #f)))
    (check (length viols) => 1)
    (check (caar viols) => 'max-file-lines)
  ) ;let
  (path-unlink tmp #t)
) ;let*

;; 3. 被 exclude 排除的文件即使超限也返回 '()
(let* ((tmp (path->string (path-join (path-temp-dir) "gf-test-excluded-viol.scm")))
       (cfg (string->json "{\"rules\": {\"max-file-lines\": {\"limit\": 10}}, \"excludes\": {\"max-file-lines\": [\"gf-test-excluded-viol.scm\"]}}"
            ) ;string->json
       ) ;cfg
      ) ;
  (write-test-bytes tmp (repeat-lines 20))
  (check (check-file tmp cfg) => '())
  (path-unlink tmp #t)
) ;let*

;; 4. export <xxx> 记录类型句法标识 -> define-record-type 违规
(let* ((tmp (path->string (path-join (path-temp-dir) "gf-test-rec-exp.scm"))))
  (write-test-bytes tmp
    "(define-library (t)\n  (import (scheme base))\n  (export <foo> make-foo)\n) ;define-library\n"
  ) ;write-test-bytes
  (let ((viols (check-file tmp #f)))
    (check (length viols) => 1)
    (check (caar viols) => 'define-record-type)
    (check (caddr (car viols)) => 3)
  ) ;let
  (path-unlink tmp #t)
) ;let*

;; 5. no-glue：同一库内封装并导出对应公开接口时通过
(let* ((tmp (path->string (path-join (path-temp-dir) "gf-test-no-glue-clean.scm"))))
  (write-test-bytes tmp
    "(define-library (demo clean)\n  (export md5)\n  (import (scheme base))\n  (begin\n    (define md5 g_md5)\n    (define (md5-again str) (g_md5 str)))\n) ;define-library\n"
  ) ;write-test-bytes
  (check (check-file tmp #f) => '())
  (path-unlink tmp #t)
) ;let*

;; 6. no-glue：库外直接引用 glue 违规
(let* ((tmp (path->string (path-join (path-temp-dir) "gf-test-no-glue-outside.scm"))))
  (write-test-bytes tmp "(g_md5 \"abc\")\n")
  (check (map (lambda (v) (list (car v) (caddr v) (cadddr v) (list-ref v 4)))
           (check-file tmp #f))
    => '((no-glue 1 g_md5 outside-library)))
  (path-unlink tmp #t)
) ;let*

;; 7. no-glue：glue 不要求存在同名公开绑定
(let* ((tmp (path->string (path-join (path-temp-dir) "gf-test-no-glue-binding.scm"))))
  (write-test-bytes tmp
    "(define-library (demo missing)\n  (export other)\n  (import (scheme base))\n  (begin\n    (define (other str) (g_md5 str)))\n) ;define-library\n"
  ) ;write-test-bytes
  (check (check-file tmp #f) => '())
  (path-unlink tmp #t)
) ;let*

;; 8. no-glue：glue 的封装绑定不要求导出
(let* ((tmp (path->string (path-join (path-temp-dir) "gf-test-no-glue-export.scm"))))
  (write-test-bytes tmp
    "(define-library (demo private)\n  (export)\n  (import (scheme base))\n  (begin\n    (define md5 g_md5))\n) ;define-library\n"
  ) ;write-test-bytes
  (check (check-file tmp #f) => '())
  (path-unlink tmp #t)
) ;let*

;; 9. no-glue：不得直接导出 glue 绑定
(let* ((tmp (path->string (path-join (path-temp-dir) "gf-test-no-glue-export-glue.scm"))))
  (write-test-bytes tmp
    "(define-library (demo exposed)\n  (export md5 g_md5)\n  (import (scheme base))\n  (begin\n    (define md5 g_md5))\n) ;define-library\n"
  ) ;write-test-bytes
  (check (map (lambda (v) (list-ref v 4)) (check-file tmp #f)) => '(exported-glue))
  (path-unlink tmp #t)
) ;let*

;; 10. no-glue：注释、字符串和 quoted 数据不构成 glue 引用
(let* ((tmp (path->string (path-join (path-temp-dir) "gf-test-no-glue-data.scm"))))
  (write-test-bytes tmp
    ";; (g_md5 \"abc\")\n(define doc \"g_md5\")\n(define example 'g_md5)\n(define examples '(g_md5 g_sha1))\n"
  ) ;write-test-bytes
  (check (check-file tmp #f) => '())
  (path-unlink tmp #t)
) ;let*

(check-report)
