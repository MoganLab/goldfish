;; gfproject--opt-gf-lib
;; 从可选参数中解析标准库目录。
;;
;; 语法
;; ----
;; (gfproject--opt-gf-lib opt-gf-lib)
;;
;; 参数
;; ----
;; opt-gf-lib : list
;; 可选库目录参数，形如 ("/path/to/lib")；
;; 为空或首元素不是字符串时回退到默认库目录。
;;
;; 返回值
;; ----
;; string
;; 解析出的库目录路径。

(import (liii check) (liii gfproject config))

(check-set-mode! 'report-failed)

;; 显式指定时取首元素
(check (gfproject--opt-gf-lib '("/tmp/custom-lib")) => "/tmp/custom-lib")

;; 未指定时回退到默认库目录
(check (gfproject--opt-gf-lib '()) => (gfproject-get-gf-lib))
(check (gfproject--opt-gf-lib '(42)) => (gfproject-get-gf-lib))

(check-report)
