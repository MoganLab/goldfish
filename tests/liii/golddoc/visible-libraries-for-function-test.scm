;; 添加 tools/golddoc 到 load path，以便导入 (liii golddoc)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path* (cons "tools/golddoc" *load-path*))

(import (liii check)
        (liii golddoc)
        (liii os)
        (liii path)
) ;import

(check-set-mode! 'report-failed)

;; visible-libraries-for-function
;; 根据函数名和当前 *load-path* 过滤出可见的实现库。
;;
;; 语法
;; ----
;; (visible-libraries-for-function function-name)
;;
;; 参数
;; ----
;; function-name : string?
;; 要查询的导出函数名。
;;
;; 返回值
;; ----
;; list?
;; 返回按可见顺序排列的 `"org/lib"` 字符串列表。
;;
;; 描述
;; ----
;; 该函数先读取 `function-library-index.json` 中的候选库，
;; 再依据当前 *load-path* 过滤不可见库和被排除的测试分组。

(define (cleanup-visible-libraries-fixture base-root)
  (let ((load-root (path-join base-root "goldfish"))
        (tests-root (path-join base-root "tests")))
    (path-unlink (path-join tests-root "function-library-index.json") #t)
    (path-unlink (path-join load-root "liii" "foo.scm") #t)
    (path-unlink (path-join load-root "liii" "bar.scm") #t)
    (path-unlink (path-join load-root "srfi" "1.scm") #t)
    (if (path-dir? (path-join load-root "liii"))
        (path-rmdir (path-join load-root "liii"))
        #f
    ) ;if
    (if (path-dir? (path-join load-root "srfi"))
        (path-rmdir (path-join load-root "srfi"))
        #f
    ) ;if
    (if (path-dir? load-root)
        (path-rmdir load-root)
        #f
    ) ;if
    (if (path-dir? tests-root)
        (path-rmdir tests-root)
        #f
    ) ;if
    (if (path-dir? base-root)
        (path-rmdir base-root)
        #f
    ) ;if
  ) ;let
) ;define

(let*
  ((base-root
     (path-join (path-temp-dir)
                (string-append "golddoc-visible-libraries-"
                               (number->string (getpid))
                ) ;string-append
     ) ;path-join
                ) ;string-append
   (load-root (path-join base-root "goldfish"))
   (liii-root (path-join load-root "liii"))
   (srfi-root (path-join load-root "srfi"))
   (tests-root (path-join base-root "tests"))
   (index-path (path-join tests-root "function-library-index.json"))
   (old-load-path *load-path*)
  ) ;
  (cleanup-visible-libraries-fixture base-root)
  (mkdir (path->string base-root))
  (mkdir (path->string load-root))
  (mkdir (path->string liii-root))
  (mkdir (path->string srfi-root))
  (mkdir (path->string tests-root))
  (path-write-text (path-join liii-root "foo.scm")
                   "(define-library (liii foo) (export) (import (scheme base)) (begin))"
  ) ;path-write-text
  (path-write-text (path-join liii-root "bar.scm")
                   "(define-library (liii bar) (export) (import (scheme base)) (begin))"
  ) ;path-write-text
  (path-write-text (path-join srfi-root "1.scm")
                   "(define-library (srfi 1) (export) (import (scheme base)) (begin))"
  ) ;path-write-text
  (dynamic-wind
    (lambda ()
      (set! *load-path* (list (path->string load-root)))
    ) ;lambda
    (lambda ()
      (check (visible-libraries-for-function "unique-func") => '())
      (check (visible-libraries-for-function "shared-func") => '())
      (path-write-text index-path
                       "{\"shared-func\":[\"(liii foo)\",\"(liii bar)\",\"(srfi 1)\"],\"unique-func\":[\"(liii foo)\"]}"
      ) ;path-write-text
      (check (visible-libraries-for-function "unique-func") => '("liii/foo"))
      (check (visible-libraries-for-function "shared-func") => '("liii/foo" "liii/bar" "srfi/1"))
      (check (visible-libraries-for-function "missing-func") => '())
    ) ;lambda
    (lambda ()
      (set! *load-path* old-load-path)
      (cleanup-visible-libraries-fixture base-root)
    ) ;lambda
  ) ;dynamic-wind
) ;let*

(check-report)
