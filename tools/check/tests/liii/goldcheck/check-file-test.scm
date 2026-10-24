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

;; 1. 干净文件检查通过返回 '()
(let* ((tmp (path->string (path-join (path-temp-dir) "gf-test-clean.scm"))))
  (call-with-port (open-output-file tmp)
    (lambda (p) (display "(define a 1)\n(define b 2)\n" p))
  ) ;call-with-port
  (check (check-file tmp #f) => '())
  (path-unlink tmp #t)
) ;let*

;; 2. 超限文件返回违规项
(let* ((tmp (path->string (path-join (path-temp-dir) "gf-test-lines-viol.scm"))))
  (call-with-port (open-output-file tmp)
    (lambda (p)
      (let loop
        ((i 0))
        (when (< i 405)
          (display "(define x 1)\n" p)
          (loop (+ i 1))
        ) ;when
      ) ;let
    ) ;lambda
  ) ;call-with-port
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
  (call-with-port (open-output-file tmp)
    (lambda (p)
      (let loop
        ((i 0))
        (when (< i 20)
          (display "(define x 1)\n" p)
          (loop (+ i 1))
        ) ;when
      ) ;let
    ) ;lambda
  ) ;call-with-port
  (check (check-file tmp cfg) => '())
  (path-unlink tmp #t)
) ;let*

;; 4. export <xxx> 记录类型句法标识 -> define-record-type 违规
(let* ((tmp (path->string (path-join (path-temp-dir) "gf-test-rec-exp.scm"))))
  (call-with-port (open-output-file tmp)
    (lambda (p)
      (display "(define-library (t)\n  (import (scheme base))\n  (export <foo> make-foo)\n) ;define-library\n" p)
    ) ;call-with-port
  ) ;call-with-port
  (let ((viols (check-file tmp #f)))
    (check (length viols) => 1)
    (check (caar viols) => 'define-record-type)
    (check (caddr (car viols)) => 3)
  ) ;let
  (path-unlink tmp #t)
) ;let*

(check-report)
