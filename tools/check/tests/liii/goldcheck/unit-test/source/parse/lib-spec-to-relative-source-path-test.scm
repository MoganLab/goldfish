;; lib-spec->relative-source-path
;; 将库标识符规范列表转换为相对源码文件路径。
;;
;; 语法
;; ----
;; (lib-spec->relative-source-path spec)
;;
;; 参数
;; ----
;; spec : list of symbol  库标识符符号列表，如 '(scheme base)
;;
;; 返回值
;; ------
;; string
;; 相对源码文件路径，如 "goldfish/scheme/base.scm"。

(import (scheme base)
  (liii check)
  (liii goldcheck unit-test source parse)
) ;import

(check-set-mode! 'report-failed)

;; 1. 两级库名转换
(check (lib-spec->relative-source-path '(scheme base))
  =>
  "goldfish/scheme/base.scm"
) ;check

;; 2. 三级库名转换
(check (lib-spec->relative-source-path '(liii goldcheck report))
  =>
  "goldfish/liii/goldcheck/report.scm"
) ;check

;; 3. 单级库名转换
(check (lib-spec->relative-source-path '(goldfish))
  =>
  "goldfish/goldfish.scm"
) ;check

(check-report)
