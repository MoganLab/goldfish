;; 添加 tools/golddoc 到 load path，以便导入 (liii golddoc)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path* (cons "tools/golddoc" *load-path*))

(import (liii check)
        (liii golddoc)
        (liii os)
        (liii path)
) ;import

(check-set-mode! 'report-failed)

;; load-function-index
;; 读取当前 *load-path* 可见测试根目录下的函数索引 JSON。
;;
;; 语法
;; ----
;; (load-function-index)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; alist
;; 返回形如 `((function-name . ("(org lib)" ...)) ...)` 的关联列表。
;;
;; 描述
;; ----
;; 该函数会沿着当前 *load-path* 推导关联的 `tests` 根目录，
;; 查找 `function-library-index.json`，再将多个索引合并为一个结果。

(define (contains-function-index-path? paths)
  (let loop ((remaining paths))
    (and (not (null? remaining))
         (or (and (path-file? (car remaining))
                  (string=? (path-name (car remaining)) "function-library-index.json"))
             (loop (cdr remaining))
         ) ;or
    ) ;and
  ) ;let
) ;define

(define (cleanup-load-index-fixture base-root)
  (let ((load-root (path-join base-root "goldfish"))
        (tests-root (path-join base-root "tests")))
    (path-unlink (path-join tests-root "function-library-index.json") #t)
    (if (path-dir? tests-root)
        (path-rmdir tests-root)
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

(check (index-entry->library-query "(liii string)") => "liii/string")
(check (index-entry->library-query "(scheme char)") => "scheme/char")
(check (index-entry->library-query "(bad)") => #f)
(check (index-entry->library-query 1) => #f)

(let*
  ((base-root
     (path-join (path-temp-dir)
                (string-append "golddoc-load-index-"
                               (number->string (getpid))
                ) ;string-append
     ) ;path-join
                ) ;string-append
   (load-root (path-join base-root "goldfish"))
   (tests-root (path-join base-root "tests"))
   (index-path (path-join tests-root "function-library-index.json"))
   (old-load-path *load-path*)
  ) ;
  (cleanup-load-index-fixture base-root)
  (mkdir (path->string base-root))
  (mkdir (path->string load-root))
  (mkdir (path->string tests-root))
  (dynamic-wind
    (lambda ()
      (set! *load-path* (list (path->string load-root)))
    ) ;lambda
    (lambda ()
      (check (find-function-index-paths) => '())
      (check (load-function-index) => '())
      (path-write-text index-path
                       "{\"sample-func\":[\"(liii sample)\"],\"shared-func\":[\"(scheme base)\",\"(liii sample)\"]}"
      ) ;path-write-text
      (let ((index-paths (find-function-index-paths))
            (index (load-function-index)))
        (check-true (pair? index-paths))
        (check-true (contains-function-index-path? index-paths))
        (check (cdr (assoc "sample-func" index)) => '("(liii sample)"))
        (check (cdr (assoc "shared-func" index)) => '("(scheme base)" "(liii sample)"))
      ) ;let
    ) ;lambda
    (lambda ()
      (set! *load-path* old-load-path)
      (cleanup-load-index-fixture base-root)
    ) ;lambda
  ) ;dynamic-wind
) ;let*

(check-report)
