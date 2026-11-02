;; set-find-library-source-fn!
;; 设置根据测试目录查找对应库源码实现文件路径的全局查找函数。
;;
;; 语法
;; ----
;; (set-find-library-source-fn! fn)
;;
;; 参数
;; ----
;; fn : procedure or #f
;; 接受测试目录路径字符串并返回源码文件路径字符串的过程，或传入 #f 清除。
;;
;; 返回值
;; ------
;; unspecified

(import (scheme base)
  (liii check)
  (liii goldcheck unit-test source reexport)
) ;import

(check-set-mode! 'report-failed)

;; 1. 设置自定义库源码查找函数
(set-find-library-source-fn! (lambda (parent) "dummy-lib.scm"))

;; 2. 过程类型校验
(check-true (procedure? set-find-library-source-fn!))

;; 3. 恢复重置查找函数
(set-find-library-source-fn! #f)

(check-report)
