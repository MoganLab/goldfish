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
;; 相对源码文件路径，如 "scheme/base.scm"。
;;
;; 说明
;; ----
;; 返回的是板块内相对路径，不含板块前缀（r7rs/ goldfish/ library/）；
;; 板块根目录由调用方按 r7rs/ goldfish/ library/ 依次尝试。

(import (scheme base)
  (liii check)
  (liii goldcheck unit-test source parse)
) ;import

(check-set-mode! 'report-failed)

;; 1. 两级库名转换
(check (lib-spec->relative-source-path '(scheme base))
  =>
  "scheme/base.scm"
) ;check

;; 2. 三级库名转换
(check (lib-spec->relative-source-path '(liii goldcheck report))
  =>
  "liii/goldcheck/report.scm"
) ;check

;; 3. 单级库名转换
(check (lib-spec->relative-source-path '(goldfish))
  =>
  "goldfish.scm"
) ;check

(check-report)
