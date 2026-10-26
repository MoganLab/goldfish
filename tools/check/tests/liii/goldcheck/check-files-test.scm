(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii os)
  (liii json)
  (liii goldcheck)
) ;import

;; check-files
;; 对文件向量并行执行所有启用的规则检查，按输入文件顺序收集违规项，返回违规项向量。
;;
;; 语法
;; ----
;; (check-files file-vector cfg)
;;
;; 参数
;; ----
;; file-vector : vector of string
;; 待检查的文件路径向量。
;; cfg : json-object 或 #f
;; 检查配置。
;;
;; 返回值
;; ------
;; vector of violations
;; 违规项向量。无违规时返回 #()。

(check-set-mode! 'report-failed)

;; 临时文件一律用 path-write-bytes 二进制写入，精确控制 LF 行尾，
;; 避免文本端口在 Windows 上把 \n 转写为 \r\n。
(define (make-tmp-file name lines)
  (let* ((tmp (path->string (path-join (path-temp-dir) name)))
         (content
           (let loop
             ((i 0) (acc '()))
             (if (>= i lines)
               (apply string-append (reverse acc))
               (loop (+ i 1) (cons "(define x 1)\n" acc))
             ) ;if
           ) ;let
         ) ;content
        ) ;
    (path-write-bytes (path tmp) (string->utf8 content))
    tmp
  ) ;let*
) ;define

;; 1. 空向量返回 #()
(check (check-files #() #f) => #())

;; 2. 多文件并行检查结果保序：违规按输入文件顺序排列
;;    violations 中的路径统一为正斜杠（跨平台一致），断言用 posix 形式
(let* ((f1 (make-tmp-file "gf-test-files-a.scm" 405))
       (f2 (make-tmp-file "gf-test-files-b.scm" 3))
       (f3 (make-tmp-file "gf-test-files-c.scm" 405))
       (viols (check-files (vector f1 f2 f3) #f))
       (posix (lambda (s) (path->string (path-as-posix (path s)))))
      ) ;
  (check (vector-length viols) => 2)
  (check (car (vector-ref viols 0)) => 'max-file-lines)
  (check (car (vector-ref viols 1)) => 'max-file-lines)
  (check (cadr (vector-ref viols 0)) => (posix f1))
  (check (cadr (vector-ref viols 1)) => (posix f3))
  (path-unlink f1 #t)
  (path-unlink f2 #t)
  (path-unlink f3 #t)
) ;let*

;; 3. cfg 经 JSON 字符串运输到 worker 后 exclude 与 limit 仍生效
(let* ((f1 (make-tmp-file "gf-test-files-excl.scm" 20))
       (f2 (make-tmp-file "gf-test-files-kept.scm" 20))
       (cfg (string->json "{\"rules\": {\"max-file-lines\": {\"limit\": 10}}, \"excludes\": {\"max-file-lines\": [\"gf-test-files-excl.scm\"]}}"
            ) ;string->json
       ) ;cfg
       (viols (check-files (vector f1 f2) cfg))
       (posix (lambda (s) (path->string (path-as-posix (path s)))))
      ) ;
  (check (vector-length viols) => 1)
  (check (cadr (vector-ref viols 0)) => (posix f2))
  (path-unlink f1 #t)
  (path-unlink f2 #t)
) ;let*

;; 4. no-glue：不同 define-library 不得重复封装同一个 glue
(let* ((f1 (make-tmp-file "gf-test-no-glue-owner-a.scm" 0))
       (f2 (make-tmp-file "gf-test-no-glue-owner-b.scm" 0))
       (library
         (lambda (name)
           (string-append "(define-library (demo " name ")\n"
             "  (export md5)\n"
             "  (import (scheme base))\n"
             "  (begin (define md5 g_md5))\n"
             ") ;define-library\n"))))
  (path-write-bytes (path f1) (string->utf8 (library "one")))
  (path-write-bytes (path f2) (string->utf8 (library "two")))
  (check (map (lambda (v) (list (car v) (list-ref v 4)))
           (vector->list (check-files (vector f1 f2) #f)))
    => '((no-glue multiple-libraries) (no-glue multiple-libraries)))
  (path-unlink f1 #t)
  (path-unlink f2 #t)
) ;let*

;; 5. no-glue：不同库分别封装不同 glue 时通过
(let* ((f1 (make-tmp-file "gf-test-no-glue-distinct-a.scm" 0))
       (f2 (make-tmp-file "gf-test-no-glue-distinct-b.scm" 0)))
  (path-write-bytes (path f1)
    (string->utf8
      "(define-library (demo md5) (export md5) (import (scheme base)) (begin (define md5 g_md5)))\n"))
  (path-write-bytes (path f2)
    (string->utf8
      "(define-library (demo sha1) (export sha1) (import (scheme base)) (begin (define sha1 g_sha1)))\n"))
  (check (check-files (vector f1 f2) #f) => #())
  (path-unlink f1 #t)
  (path-unlink f2 #t)
) ;let*

(check-report)
