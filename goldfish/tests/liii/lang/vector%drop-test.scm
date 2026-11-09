;; vector%drop
;; 跳过向量前 n 个元素，越界宽容（对标 Scala Vector.drop）。
;;
;; 语法
;; ----
;; ($ v :drop n)
;;
;; 参数
;; ----
;; n : integer?
;; 跳过的前缀元素数，越界/负数宽容。
;;
;; v : vector?
;; 源向量。
;;
;; 返回值
;; ------
;; vector?
;; 去掉前 n 个元素后的新向量。
;;
;; 描述
;; ----
;; vector 路径去糖为 (vector-drop v n)，(liii vector) 实现，
;; n 超过长度返回空向量，n 为负返回全部（Scala Oracle 验证）。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 2 3) :drop 1) => #(2 3))
(check ($ #(1 2 3) :drop 10) => #())
(check ($ #(1 2 3) :drop -1) => #(1 2 3))
(check ($ #() :drop 3) => #())

(check-report)
