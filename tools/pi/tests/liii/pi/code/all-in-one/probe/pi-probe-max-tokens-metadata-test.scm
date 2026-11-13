;; pi-probe-max-tokens-metadata
;; 把节点模型元数据的 max-tokens 覆盖为探活专用的极小值。
;;
;; 语法
;; ----
;; (pi-probe-max-tokens-metadata meta)
;;
;; 参数
;; ----
;; meta : <pi-model-metadata> | alist | #f
;; 节点真实模型元数据。
;;
;; 返回值
;; ----
;; <pi-model-metadata> | alist | #f
;; record 形态返回 max-tokens 为 16 的新记录（其余字段保持不变）；
;; 非 record 形态原样返回。

(import (scheme base)
  (liii check)
  (liii pi ai config record)
  (liii pi code all-in-one probe)
) ;import

(check-set-mode! 'report-failed)

;; 1. record 形态：max-tokens 覆盖为 16，其余字段保持不变
(let* ((meta (pi-model-metadata "m1" "model-1" "google" "http://example.com" 1000000 131072))
       (probe-meta (pi-probe-max-tokens-metadata meta))
      ) ;
  (check-true (pi-model-metadata? probe-meta))
  (check (pi-model-metadata-max-tokens probe-meta) => 16)
  (check (pi-model-metadata-id probe-meta) => "m1")
  (check (pi-model-metadata-name probe-meta) => "model-1")
  (check (pi-model-metadata-provider probe-meta) => "google")
  (check (pi-model-metadata-base-url probe-meta) => "http://example.com")
  (check (pi-model-metadata-context-window probe-meta) => 1000000)
) ;let*

;; 2. 原记录不被修改
(let* ((meta (pi-model-metadata "m1" "model-1" "google" "http://example.com" 1000000 131072))
       (probe-meta (pi-probe-max-tokens-metadata meta))
      ) ;
  (check (pi-model-metadata-max-tokens meta) => 131072)
  (check (not (eq? meta probe-meta)) => #t)
) ;let*

;; 3. 非 record 形态原样返回
(check (pi-probe-max-tokens-metadata '((max-tokens . 999))) => '((max-tokens . 999)))
(check (pi-probe-max-tokens-metadata #f) => #f)

(check-report)
