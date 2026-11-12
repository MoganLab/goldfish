;; extract-file-imports
;; 从 Scheme 源文件提取所有 (import ...) 引用的库名列表
;;
;; 语法
;; ----
;; (extract-file-imports file-path)
;;
;; 参数
;; ----
;; file-path : string?，Scheme 源文件路径
;;
;; 返回值
;; ------
;; list?，库名符号列表；文件不存在或解析失败时为空列表

(import (scheme base) (liii check) (liii goldtest-cache))

(check-set-mode! 'report-failed)

;; ===== 场景1: 提取库源文件的 define-library 内 import 子句 =====
(define own-imports (extract-file-imports "liii/goldtest-cache.scm"))
(check-true (list? own-imports))
(check-true (and (member '(liii hashlib) own-imports) #t))
(check-true (and (member '(liii goldtool cache) own-imports) #t))

;; ===== 场景2: 不存在的文件返回空列表 =====
(check (extract-file-imports "nonexistent/xyz.scm") => '())

;; ===== 场景3: 是过程 =====
(check-true (procedure? extract-file-imports))

(check-report)
