;; pi-config-cursor
;; 将 provider 与模型下标规范化为光标位置。
;;
;; 语法
;; ----
;; (pi-config-cursor provider-index model-index)
;;
;; 参数
;; ----
;; provider-index : integer
;; provider 列下标。
;;
;; model-index : integer
;; 模型列下标。
;;
;; 返回值
;; ----
;; list
;; 包含 provider 与 model 下标的列表。

(import (scheme base) (liii check) (liii pi code modes config layout))

(check-set-mode! 'report-failed)

;; 1. 正常下标原样返回
(check (pi-config-cursor 1 2) => '((provider . 1) (model . 2)))

;; 2. 越界下标同样原样返回，由渲染层负责裁剪
(check (pi-config-cursor 99 99) => '((provider . 99) (model . 99)))

(check-report)
