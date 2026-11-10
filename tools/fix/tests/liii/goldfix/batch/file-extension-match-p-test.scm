;; file-extension-match?
;; 判断文件名是否以给定后缀名之一结尾。
;;
;; 语法
;; ----
;; (file-extension-match? filename extensions)
;;
;; 参数
;; ----
;; filename : string? 文件名
;; extensions : list? 后缀名列表
;;
;; 返回值
;; ------
;; boolean? 匹配时为 #t，否则为 #f

(import (liii check) (liii base) (liii goldfix) (liii goldfix batch))

(check-set-mode! 'report-failed)

;; 测试：单个后缀名匹配
(check (file-extension-match? "a.scm" '(".scm")) => #t)

;; 测试：后缀名不匹配
(check (file-extension-match? "a.scm" '(".sld")) => #f)

;; 测试：多个后缀名中任一匹配即可
(check (file-extension-match? "a.scm" '(".sld" ".scm")) => #t)

;; 测试：后缀名列表为空时返回 #f
(check (file-extension-match? "a.scm" '()) => #f)

(check-report)
