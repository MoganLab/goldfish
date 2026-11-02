;; %csi-final-key
;; 将 CSI 序列的终结字节转换为按键名。
;;
;; 语法
;; ----
;; (%csi-final-key char-byte)
;;
;; 参数
;; ----
;; char-byte : integer?，CSI 序列的终结字节值（A 到 Z）
;;
;; 返回值
;; ----
;; 已知终结字节返回按键符号（up、down、home 等），
;; 未知终结字节返回 #f。

(import (liii check) (liii tui keys table))

(check-set-mode! 'report-failed)

;; 方向键
(check (%csi-final-key 65) => 'up)
(check (%csi-final-key 66) => 'down)
(check (%csi-final-key 67) => 'right)
(check (%csi-final-key 68) => 'left)

;; 导航键
(check (%csi-final-key 69) => 'clear)
(check (%csi-final-key 70) => 'end)
(check (%csi-final-key 72) => 'home)

;; 修饰组合键
(check (%csi-final-key 90) => 'shift-tab)

;; 未知终结字节
(check (%csi-final-key 64) => #f)
(check (%csi-final-key 71) => #f)
(check (%csi-final-key 97) => #f)
(check (%csi-final-key 126) => #f)

(check-report)
