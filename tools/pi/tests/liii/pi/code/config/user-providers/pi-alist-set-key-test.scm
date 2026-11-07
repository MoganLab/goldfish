;; pi-alist-set-key
;; 就地替换 alist 中的键，不存在时追加，保持其余键顺序。
;;
;; 语法
;; ----
;; (pi-alist-set-key alist key value)
;;
;; 参数
;; ----
;; alist : alist
;; 原始键值列表。
;; key : any
;; 要设置的键。
;; value : any
;; 新的值。
;;
;; 返回值
;; ----
;; alist
;; 替换或追加后的新 alist。

(import (liii check) (liii pi code config user-providers))

(check-set-mode! 'report-failed)

;; 替换已有键并保持位置
(check (pi-alist-set-key '((a . 1) (b . 2) (c . 3)) 'b 20)
  =>
  '((a . 1) (b . 20) (c . 3))
) ;check

;; 不存在时追加到末尾
(check (pi-alist-set-key '((a . 1) (b . 2)) 'c 3) => '((a . 1) (b . 2) (c . 3)))

;; 空 alist
(check (pi-alist-set-key '() 'a 1) => '((a . 1)))

(check-report)
