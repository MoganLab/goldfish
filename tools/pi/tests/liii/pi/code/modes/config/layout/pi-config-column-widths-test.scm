;; pi-config-column-widths
;; 按终端宽度计算 ranger 风格三列的列宽。
;;
;; 语法
;; ----
;; (pi-config-column-widths width)
;;
;; 参数
;; ----
;; width : integer
;; 终端宽度。
;;
;; 返回值
;; ----
;; list
;; 三列宽度组成的整数列表，第三列宽度为 0 表示隐藏。

(import (scheme base)
  (liii check)
  (liii pi code modes config layout)
) ;import

(check-set-mode! 'report-failed)

;; 1. 宽屏三等分（第一列 22，第二列 34，其余给预览）
(check (pi-config-column-widths 120) => '(22 34 64))

;; 2. 中等宽度：第三列变窄但保留
(check (pi-config-column-widths 80) => '(22 34 24))

;; 3. 窄屏：第三列宽度为 0（隐藏）
(check (pi-config-column-widths 56) => '(22 34 0))
(check (pi-config-column-widths 40) => '(20 20 0))

(check-report)
