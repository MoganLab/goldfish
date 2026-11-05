;; library-documented-functions
;; 汇总指定库测试目录中已经编写文档的函数名列表。
;;
;; 语法
;; ----
;; (library-documented-functions library-query)
;;
;; 参数
;; ----
;; library-query : string?
;; 形如 "org/lib" 的库查询字符串。
;;
;; 返回值
;; ----
;; list?
;; 文档中出现的函数名列表；查询非法、库不可见或库分组被排除时返回空列表。
;;
;; 描述
;; ----
;; 该函数依据当前 *load-path* 定位库对应的测试目录，递归扫描目录下的
;; `*-test.scm` 文件并从注释头中提取函数名。扫描顺序取决于目录遍历顺序，
;; 调用方如需稳定顺序应自行排序。

;; 添加 tools/doc 到 load path，以便导入 (liii golddoc)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path* (cons "tools/doc" *load-path*))

(import (liii check)
  (liii golddoc)
  (liii list)
  (liii os)
  (liii path)
  (liii sort)
  (liii string)
) ;import

(check-set-mode! 'report-failed)

(define (sorted-documented-functions library-query)
  (list-sort string<? (library-documented-functions library-query))
) ;define

(define (documented-function? library-query function-name)
  (if (member function-name (library-documented-functions library-query) string=?)
    #t
    #f
  ) ;if
) ;define

(define (cleanup-documented-functions-fixture base-root)
  (let ((load-root (path-join base-root "goldfish"))
        (tests-root (path-join base-root "tests"))
       ) ;
    (path-unlink (path-join tests-root "liii" "demo" "beta-test.scm") #t)
    (path-unlink (path-join tests-root "liii" "demo" "alpha-test.scm") #t)
    (path-unlink (path-join tests-root "liii" "demo" "README.md") #t)
    (path-unlink (path-join load-root "liii" "demo.scm") #t)
    (if (path-dir? (path-join tests-root "liii" "demo"))
      (path-rmdir (path-join tests-root "liii" "demo"))
      #f
    ) ;if
    (if (path-dir? (path-join tests-root "liii"))
      (path-rmdir (path-join tests-root "liii"))
      #f
    ) ;if
    (if (path-dir? tests-root) (path-rmdir tests-root) #f)
    (if (path-dir? (path-join load-root "liii"))
      (path-rmdir (path-join load-root "liii"))
      #f
    ) ;if
    (if (path-dir? load-root) (path-rmdir load-root) #f)
    (if (path-dir? base-root) (path-rmdir base-root) #f)
  ) ;let
) ;define

;; 查询本身不合法：不含 "/"、含空片段或空串
(check (library-documented-functions "string-split") => '())
(check (library-documented-functions "") => '())
(check (library-documented-functions "liii/") => '())

;; goldfish 分组被排除，不产生文档结果
(check (library-documented-functions "goldfish/liii/string") => '())

(let* ((base-root
         (path-join (path-temp-dir)
           (string-append "golddoc-documented-functions-" (number->string (getpid)))
         ) ;path-join
       ) ;base-root
       (load-root (path-join base-root "goldfish"))
       (liii-root (path-join load-root "liii"))
       (tests-root (path-join base-root "tests"))
       (library-root (path-join tests-root "liii" "demo"))
       (old-load-path *load-path*)
      ) ;
  (cleanup-documented-functions-fixture base-root)
  (mkdir (path->string base-root))
  (mkdir (path->string load-root))
  (mkdir (path->string liii-root))
  (mkdir (path->string tests-root))
  (mkdir (path->string library-root))
  (path-write-text (path-join liii-root "demo.scm")
    "(define-library (liii demo) (export alpha beta) (import (scheme base)) (begin))"
  ) ;path-write-text
  (path-write-text (path-join library-root "alpha-test.scm")
    ";; alpha\n;; 返回 alpha 的测试文档。\n;;\n;; 语法\n;; ----\n;; (alpha)\n(check-report)\n"
  ) ;path-write-text
  (path-write-text (path-join library-root "beta-test.scm")
    ";; beta\n;; 返回 beta 的测试文档。\n(check-report)\n"
  ) ;path-write-text
  (path-write-text (path-join library-root "README.md")
    "# (liii demo) 模块导览\n"
  ) ;path-write-text
  (dynamic-wind
    (lambda () (set! *load-path* (list (path->string load-root))))
    (lambda ()
      ;; 库可见：返回测试目录中已文档化的函数名
      (check (sorted-documented-functions "liii/demo") => '("alpha" "beta"))
      ;; 未编写测试文档的导出函数不会出现在结果中
      (check (documented-function? "liii/demo" "gamma") => #f)
      ;; 库源文件存在但测试目录缺失时返回空列表
      (check (library-documented-functions "liii/absent") => '())
      ;; 测试目录存在但没有叶子文档时返回空列表
      (path-unlink (path-join library-root "alpha-test.scm") #t)
      (path-unlink (path-join library-root "beta-test.scm") #t)
      (check (library-documented-functions "liii/demo") => '())
    ) ;lambda
    (lambda ()
      (set! *load-path* old-load-path)
      (cleanup-documented-functions-fixture base-root)
    ) ;lambda
  ) ;dynamic-wind
) ;let*

(check-report)
