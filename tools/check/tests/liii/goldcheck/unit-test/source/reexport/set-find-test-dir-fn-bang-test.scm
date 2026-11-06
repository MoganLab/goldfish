;; set-find-test-dir-fn!
;; 设置根据库源码文件路径反查其对应单元测试目录的全局查找函数。
;;
;; 语法
;; ----
;; (set-find-test-dir-fn! fn)
;;
;; 参数
;; ----
;; fn : procedure or #f
;; 接受库源码文件路径字符串并返回测试目录路径字符串的过程，或传入 #f 清除。
;;
;; 返回值
;; ------
;; unspecified

(import (scheme base)
  (liii check)
  (liii goldcheck unit-test source reexport)
) ;import

(check-set-mode! 'report-failed)

;; 1. 设置自定义测试目录查找函数
(set-find-test-dir-fn! (lambda (src-path) "dummy-test-dir"))

;; 2. 过程类型校验
(check-true (procedure? set-find-test-dir-fn!))

;; 3. 恢复重置查找函数
(set-find-test-dir-fn! #f)

(check-report)
