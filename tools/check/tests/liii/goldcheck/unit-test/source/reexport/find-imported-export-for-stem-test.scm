;; find-imported-export-for-stem
;; 根据测试名主干在导入库列表中查找匹配的导出符号及其目标测试目录。
;;
;; 语法
;; ----
;; (find-imported-export-for-stem stem imports base-dir)
;;
;; 参数
;; ----
;; stem : string
;; 测试文件名称主干（不含 -test.scm 后缀）。
;; imports : list of list
;; 导入的库标识符规范列表。
;; base-dir : string
;; 源码库根基准目录。
;;
;; 返回值
;; ------
;; pair or #f
;; 成功找到时返回 (导出符号名 . 目标测试目录) 点对，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii path)
  (liii goldcheck unit-test source reexport)
) ;import

(check-set-mode! 'report-failed)

;; 1. 空导入列表返回 #f
(check (find-imported-export-for-stem "foo" '() "") => #f)

;; 2. 夹具测试：在已存在的导入库中根据 stem 匹配导出符号
(let* ((tmp-dir (path-temp-dir))
       (base-dir (path-join tmp-dir "gf-test-stem-match"))
       (goldfish-dir (path-join base-dir "goldfish"))
       (lib-file (path->string (path-join goldfish-dir "test-mod.scm")))
       (_ (path-mkdir (path goldfish-dir) :parents #t :exist-ok #t))
       (lib-src (string-append "(define-library (test-mod)\n"
                  "  (export hello-world)\n" "  (import (scheme base))\n"
                  "  (begin (define (hello-world) 1)))\n"
                ) ;string-append
       ) ;lib-src
      ) ;
  (path-write-bytes (path lib-file) (string->utf8 lib-src))
  (let ((found (find-imported-export-for-stem "hello-world" '((test-mod))
                 (path->string base-dir)
               ) ;find-imported-export-for-stem
        ) ;found
       ) ;
    (check found => '("hello-world" . "tests/test-mod"))
  ) ;let
  (check
    (find-imported-export-for-stem "not-found" '((test-mod))
      (path->string base-dir)
    ) ;find-imported-export-for-stem
    =>
    #f
  ) ;check

  (path-unlink lib-file #t)
  (path-rmdir goldfish-dir)
  (path-rmdir base-dir)
) ;let*

(check-report)
