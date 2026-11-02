;; set-exported-name-to-test-stem-fn!
;; 设置用于将导出过程符号名转换为单测文件名主干的全局转换函数。
;;
;; 语法
;; ----
;; (set-exported-name-to-test-stem-fn! fn)
;;
;; 参数
;; ----
;; fn : procedure or #f
;; 接受符号名字符串并返回主干字符串的单参数过程，或用 #f 清除。
;;
;; 返回值
;; ------
;; unspecified

(import (scheme base)
  (liii check)
  (liii goldcheck unit-test source reexport)
) ;import

(check-set-mode! 'report-failed)

;; 1. 设置自定义名称转换函数
(set-exported-name-to-test-stem-fn!
  (lambda (name) (string-append name "-stem"))
) ;set-exported-name-to-test-stem-fn!

;; 2. 过程类型校验
(check-true (procedure? set-exported-name-to-test-stem-fn!))

;; 3. 恢复重置转换函数
(set-exported-name-to-test-stem-fn! #f)

(check-report)
