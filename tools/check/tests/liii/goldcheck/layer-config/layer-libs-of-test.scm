;; layer-libs-of
;; 获取分层定义对象中的库匹配 pattern 向量。
;;
;; 语法
;; ----
;; (layer-libs-of layer)
;;
;; 参数
;; ----
;; layer : layer   已解析的分层定义对象
;;
;; 返回值
;; ------
;; vector
;; 包含库匹配规则的向量。

(import (scheme base) (liii check) (liii goldcheck layer-config))

(check-set-mode! 'report-failed)

;; 1. 获取分层对象的 libs 属性
(let ((layer '((name . "kernel") (libs . #("liii base" "liii error")))))
  (check (layer-libs-of layer) => '#("liii base" "liii error"))
) ;let

(check-report)
