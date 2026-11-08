;; vector%take
;; 取向量前 n 个元素，越界宽容（对标 Scala Vector.take）。
;;
;; 语法
;; ----
;; ($ v :take n)
;;
;; 参数
;; ----
;; n : integer?
;; 保留的前缀元素数，越界/负数宽容。
;;
;; v : vector?
;; 源向量。
;;
;; 返回值
;; ------
;; vector?
;; v 的前 n 个元素构成的新向量。
;;
;; 描述
;; ----
;; vector 路径去糖为 (vector-take v n)，(liii vector) 实现，
;; n 超过长度返回全部，n 为负返回空向量（Scala Oracle 验证）。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ #(1 2 3) :take 2) => #(1 2))
(check ($ #(1 2 3) :take 10) => #(1 2 3))
(check ($ #(1 2 3) :take -1) => #())
(check ($ #() :take 3) => #())

(check-report)
