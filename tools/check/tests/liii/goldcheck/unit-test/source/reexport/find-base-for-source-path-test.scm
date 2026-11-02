;; find-base-for-source-path
;; 根据库源码路径查找其所在的工程根基准目录。
;;
;; 语法
;; ----
;; (find-base-for-source-path source-path)
;;
;; 参数
;; ----
;; source-path : string
;; 库源码文件路径。
;;
;; 返回值
;; ------
;; string
;; 源码库所在根基准目录路径字符串。

(import (scheme base)
  (liii check)
  (liii path)
  (liii goldcheck unit-test source reexport)
) ;import

(check-set-mode! 'report-failed)

;; 1. 标准 goldfish 库源码路径解析基准目录
(let ((base (find-base-for-source-path "goldfish/scheme/base.scm")))
  (check-true (string? base))
  (check-true (path-dir? (path base)))
) ;let

;; 2. 非 goldfish 路径返回当前工程根或空串
(let ((base (find-base-for-source-path "README.md")))
  (check-true (string? base))
) ;let

(check-report)
