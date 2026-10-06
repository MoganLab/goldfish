(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; syntax-violation
;; 在宏展开或语法检查阶段抛出格式化的语法错误异常。
;;
;; 语法
;; ----
;; (syntax-violation who message [form [subform]])
;;
;; 参数
;; ----
;; who : symbol / identifier / #f
;; 出错的宏名或调用方标识符；若无则为 #f。
;;
;; message : string
;; 描述错误原因的说明文本。
;;
;; form : any (可选)
;; 出错的完整语法形式。
;;
;; subform : any (可选)
;; 出错的具体子形式。
;;
;; 返回值
;; -----
;; 不返回，直接抛出 'syntax-error 异常。

;; 1. 抛出基础语法错误
(check-catch 'syntax-error
  (syntax-violation 'my-macro "something wrong" '(bad form)))

;; 2. who 为 #f 时的语法错误
(check-catch 'syntax-error
  (syntax-violation #f "anonymous syntax error"))

(check-report)
