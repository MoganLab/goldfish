;; %skip-ansi-csi
;; 检测并跳过 ANSI CSI 转义序列，返回序列之后的下一个索引。
;;
;; 语法
;; ----
;; (%skip-ansi-csi text i len)
;;
;; 参数
;; ----
;; text : string?
;; 待扫描的文本。
;;
;; i : integer?
;; 当前扫描位置。
;;
;; len : integer?
;; 文本长度。
;;
;; 返回值
;; ----
;; integer? 或 #f
;; 若当前位置开始的是 CSI 序列则返回序列后的索引，否则返回 #f。

(import (scheme base) (liii check) (liii tui terminal virtual state))

(check-set-mode! 'report-failed)

;; 1. 完整的 CSI 序列被跳过（ESC [ 1 ; 3 2 m 共 7 个字符）
(let ((text "\x1b;[1;32m@"))
  (check (%skip-ansi-csi text 0 (string-length text)) => 7)
) ;let

;; 2. 非 CSI 序列返回 #f
(let ((text "abc"))
  (check-false (%skip-ansi-csi text 0 3))
) ;let

;; 3. 单独 ESC 字符不是 CSI 序列
(let ((text (string-append "\x1b;" "A")))
  (check-false (%skip-ansi-csi text 0 2))
) ;let

;; 4. ESC[ 位于末尾时返回文本长度
(let ((text "\x1b;["))
  (check (%skip-ansi-csi text 0 2) => 2)
) ;let

(check-report)
