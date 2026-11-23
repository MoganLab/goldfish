;; check-md-stem-document
;; 校验 md-stem 文档头与顶层块列表的形状。
;;
;; 语法
;; ----
;; (check-md-stem-document doc)
;;
;; 参数
;; ----
;; doc : any
;; 待校验的完整文档 S 表达式。
;;
;; 返回值
;; ----
;; unspecified
;; 校验通过时无返回值。
;;
;; 错误处理
;; ----
;; 缺少 md-stem 文档头、文档形状非法或子块非法时抛出 md-stem-error。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown validate))

(check-set-mode! 'report-failed)

(check-true (procedure? check-md-stem-document))

;; 合法文档通过
(check-true (check-md-stem-document '(md-stem)))
(check-true (check-md-stem-document '(md-stem (p "正文") (h1 "标题"))))

;; 缺少文档头
(check-catch 'md-stem-error (check-md-stem-document '(p "正文")))
(check-catch 'md-stem-error (check-md-stem-document 42))
(check-catch 'md-stem-error (check-md-stem-document '(md-stem . tail)))

;; 子块非法沿块校验报错
(check-catch 'md-stem-error (check-md-stem-document '(md-stem (unknown-block))))

(check-report)
