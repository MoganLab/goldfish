;; 0-clause BSD
;; Bill Schottstaedt
;; from S7 source repo: r7rs.scm

;; (srfi srfi-39) 是 R7RS (scheme base) 中参数对象的兼容入口。
;;
;; SRFI-39 "Parameter Objects" 的 make-parameter/parameterize 已被
;; R7RS-small 收进 (scheme base)；本库仅重导出这两个绑定，保留原有
;; import 路径可用。与 (scheme base) 同时导入时得到同一绑定，
;; 不会产生重名冲突。

(define-library (srfi srfi-39)
  (import (scheme base))
  (export make-parameter parameterize)
) ;define-library
