;; 添加 tools/golddoc 到 load path，以便导入 (liii golddoc)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path* (cons "tools/golddoc" *load-path*))

(import (liii check)
        (liii golddoc)
        (liii os)
        (liii path)
) ;import

(check-set-mode! 'report-failed)

;; suggest-library-functions
;; 在指定库的测试文档中返回编辑距离阈值内的函数候选。
;;
;; 语法
;; ----
;; (suggest-library-functions library-query function-name)
;;
;; 参数
;; ----
;; library-query : string?
;; function-name : string?
;;
;; 返回值
;; ----
;; list?
;; 返回当前库内所有编辑距离小于等于 `2` 的函数候选。
;;
;; 描述
;; ----
;; 该函数只扫描当前 *load-path* 可见库对应的测试目录，
;; 不依赖全局 JSON 索引。

(define (cleanup-library-suggestion-fixture base-root)
  (let ((load-root (path-join base-root "goldfish"))
        (tests-root (path-join base-root "tests")))
    (path-unlink (path-join tests-root "liii" "demo" "string-split-test.scm") #t)
    (path-unlink (path-join tests-root "liii" "demo" "string-splat-test.scm") #t)
    (path-unlink (path-join tests-root "liii" "demo" "string-spilt-test.scm") #t)
    (path-unlink (path-join load-root "liii" "demo.scm") #t)
    (if (path-dir? (path-join tests-root "liii" "demo"))
        (path-rmdir (path-join tests-root "liii" "demo"))
        #f
    ) ;if
    (if (path-dir? (path-join tests-root "liii"))
        (path-rmdir (path-join tests-root "liii"))
        #f
    ) ;if
    (if (path-dir? tests-root)
        (path-rmdir tests-root)
        #f
    ) ;if
    (if (path-dir? (path-join load-root "liii"))
        (path-rmdir (path-join load-root "liii"))
        #f
    ) ;if
    (if (path-dir? load-root)
        (path-rmdir load-root)
        #f
    ) ;if
    (if (path-dir? base-root)
        (path-rmdir base-root)
        #f
    ) ;if
  ) ;let
) ;define

(let* ((base-root (path-join (path-temp-dir)
                             (string-append "golddoc-library-suggestions-"
                                            (number->string (getpid)))))
       (load-root (path-join base-root "goldfish"))
       (liii-root (path-join load-root "liii"))
       (tests-root (path-join base-root "tests"))
       (group-root (path-join tests-root "liii"))
       (library-root (path-join group-root "demo"))
       (old-load-path *load-path*))
  (cleanup-library-suggestion-fixture base-root)
  (mkdir (path->string base-root))
  (mkdir (path->string load-root))
  (mkdir (path->string liii-root))
  (mkdir (path->string tests-root))
  (mkdir (path->string group-root))
  (mkdir (path->string library-root))
  (path-write-text (path-join liii-root "demo.scm")
                   "(define-library (liii demo) (export) (import (scheme base)) (begin))")
  (path-write-text (path-join library-root "string-split-test.scm")
                   ";; string-split\n(check-report)\n")
  (path-write-text (path-join library-root "string-splat-test.scm")
                   ";; string-splat\n(check-report)\n")
  (path-write-text (path-join library-root "string-spilt-test.scm")
                   ";; string-spilt\n(check-report)\n")
  (dynamic-wind
    (lambda ()
      (set! *load-path* (list (path->string load-root)))
    ) ;lambda
    (lambda ()
      (check (suggest-library-functions "liii/demo" "string-splst")
        => '("string-splat" "string-split" "string-spilt")
      ) ;check
      (check (suggest-library-functions "liii/demo" "demo-only")
        => '()
      ) ;check
    ) ;lambda
    (lambda ()
      (set! *load-path* old-load-path)
      (cleanup-library-suggestion-fixture base-root)
    ) ;lambda
  ) ;dynamic-wind
) ;let*

(check-report)
