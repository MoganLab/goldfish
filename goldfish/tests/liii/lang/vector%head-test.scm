;; vector%head / vector%last（对标 Scala Vector.head/last）
;; 向量首末元素；option 形态见 list%head 测试。
;;
;; 语法
;; ----
;; ($ v :head)   ($ v :last)   ($ v :head-option)   ($ v :last-option)

;; 参数
;; ----
;; v : vector?
;; 源向量，不可为空。
;;
;; 返回值
;; ------
;; :head/:last —— 首末元素，空向量抛 value-error；
;; :head-option/:last-option —— (option v)，空向量 (none)。
;;
(import (liii check) (liii lang) (liii option) (liii error))

(check-set-mode! 'report-failed)

(check ($ #(9 7) :head) => 9)
(check ($ #(9 7) :last) => 7)
(check-catch 'value-error ($ #() :head))
(check ($ #() :last-option) => (none))
(check ($ #(9 7) :head-option) => (option 9))

(check-report)
