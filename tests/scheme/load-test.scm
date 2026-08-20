;; (scheme load) 模块文档与测试
;;
;; `(scheme load)` 提供从文件中加载 Scheme 表达式的过程。
;;
;; ==== 过程 ====
;;
;;   (load filename)    读取并求值 filename 中的 Scheme 表达式
;;
;; ==== 说明 ====
;;
;; 1. load 按顺序读取并求值文件中的全部顶层表达式
;; 2. 文件按 *load-path* 查找
;;
;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc scheme/load "load"
(import (liii check) (liii os) (liii path) (liii string) (scheme load))
(check-set-mode! 'report-failed)

;; ==== 测试：load 过程存在 ====
(check (procedure? load) => #t)

;; ==== 测试：load 求值有效文件不报错 ====
(let* ((base-root (path-join (path-temp-dir)
                    (string-append "goldfish-scheme-load-" (number->string (getpid)))
                  ) ;path-join
       ) ;base-root
       (file (path-join base-root "data.scm"))
      ) ;
  (path-write-text file "(display 1)\n")
  (check-catch #t (load file))
  (path-unlink file #t)
  (if (path-dir? base-root) (path-rmdir base-root) #f)
) ;let*

;; ==== 测试：load 不存在的文件报错 ====
(check-catch #t
  (load (path-join (path-temp-dir) "goldfish-no-such-file-xyz.scm"))
) ;check-catch

(check-report)
