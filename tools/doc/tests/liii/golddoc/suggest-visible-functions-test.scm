;; 添加 tools/golddoc 到 load path，以便导入 (liii golddoc)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path*
  (cons "tools/golddoc" *load-path*)
) ;set!

(import (liii check)
  (liii golddoc)
  (liii os)
  (liii path)
) ;import

(check-set-mode! 'report-failed)

;; suggest-visible-functions
;; 在当前 *load-path* 可见函数索引中返回编辑距离阈值内的候选。
;;
;; 语法
;; ----
;; (suggest-visible-functions function-name)
;;
;; 参数
;; ----
;; function-name : string?
;;
;; 返回值
;; ----
;; list?
;; 返回所有编辑距离小于等于 `2` 且库在当前 *load-path* 中可见的函数候选。
;;
;; 描述
;; ----
;; 该函数依赖 `function-library-index.json`，
;; 并且会过滤掉被排除测试分组中的库。

(define (cleanup-visible-suggestion-fixture base-root
        ) ;cleanup-visible-suggestion-fixture
  (let ((load-root (path-join base-root "goldfish")
        ) ;load-root
        (tests-root (path-join base-root "tests")
        ) ;tests-root
       ) ;
    (path-unlink (path-join tests-root
                   "function-library-index.json"
                 ) ;path-join
      #t
    ) ;path-unlink
    (path-unlink (path-join load-root "liii" "demo.scm")
      #t
    ) ;path-unlink
    (path-unlink (path-join load-root "srfi" "1.scm")
      #t
    ) ;path-unlink
    (if (path-dir? (path-join load-root "liii"))
      (path-rmdir (path-join load-root "liii")
      ) ;path-rmdir
      #f
    ) ;if
    (if (path-dir? (path-join load-root "srfi"))
      (path-rmdir (path-join load-root "srfi")
      ) ;path-rmdir
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

(let* ((base-root (path-join (path-temp-dir)
                    (string-append "golddoc-visible-suggestions-"
                      (number->string (getpid))
                    ) ;string-append
                  ) ;path-join
       ) ;base-root
       (load-root (path-join base-root "goldfish")
       ) ;load-root
       (liii-root (path-join load-root "liii"))
       (srfi-root (path-join load-root "srfi"))
       (tests-root (path-join base-root "tests")
       ) ;tests-root
       (index-path (path-join tests-root
                     "function-library-index.json"
                   ) ;path-join
       ) ;index-path
       (old-load-path *load-path*)
      ) ;
  (cleanup-visible-suggestion-fixture base-root
  ) ;cleanup-visible-suggestion-fixture
  (mkdir (path->string base-root))
  (mkdir (path->string load-root))
  (mkdir (path->string liii-root))
  (mkdir (path->string srfi-root))
  (mkdir (path->string tests-root))
  (path-write-text (path-join liii-root "demo.scm")
    "(define-library (liii demo) (export) (import (scheme base)) (begin))"
  ) ;path-write-text
  (path-write-text (path-join srfi-root "1.scm")
    "(define-library (srfi 1) (export) (import (scheme base)) (begin))"
  ) ;path-write-text
  (path-write-text index-path
    "{\"string-split\":[\"(liii demo)\"],\"string-splat\":[\"(liii demo)\"],\"string-spilt\":[\"(liii demo)\"],\"srfi-only\":[\"(srfi 1)\"]}"
  ) ;path-write-text
  (dynamic-wind (lambda ()
                  (set! *load-path*
                    (list (path->string load-root))
                  ) ;set!
                ) ;lambda
    (lambda ()
      (check (suggest-visible-functions "string-spl")
        =>
        '("string-splat" "string-split")
      ) ;check
      (check (suggest-visible-functions "string-splst"
             ) ;suggest-visible-functions
        =>
        '("string-splat" "string-split" "string-spilt")
      ) ;check
      (check (suggest-visible-functions "srfi-onle")
        =>
        '("srfi-only")
      ) ;check
    ) ;lambda
    (lambda ()
      (set! *load-path* old-load-path)
      (cleanup-visible-suggestion-fixture base-root
      ) ;cleanup-visible-suggestion-fixture
    ) ;lambda
  ) ;dynamic-wind
) ;let*

(check-report)
