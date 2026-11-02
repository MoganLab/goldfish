;; %match-ansi-csi
;; 尝试从字节向量指定位置匹配 ANSI CSI 转义序列。
;;
;; 语法
;; ----
;; (%match-ansi-csi bv i len)
;;
;; 参数
;; ----
;; bv : bytevector?，待匹配的字节向量
;; i : integer?，起始匹配索引
;; len : integer?，字节向量总长度
;;
;; 返回值
;; ----
;; 匹配成功返回 CSI 序列的结束位置（下一字节索引），匹配失败返回 #f。

(import (liii check) (liii tui text-width core))

(check-set-mode! 'report-failed)

;; 成功匹配 CSI 序列
(let ((bv (string->byte-vector "\x1b;[0mrest")))
  (check (%match-ansi-csi bv 0 (bytevector-length bv)) => 4)
) ;let

(let ((bv (string->byte-vector "\x1b;[31;1mxyz")))
  (check (%match-ansi-csi bv 0 (bytevector-length bv)) => 7)
) ;let

(let ((bv (string->byte-vector "pre\x1b;[2J")))
  (check (%match-ansi-csi bv 3 (bytevector-length bv)) => 7)
) ;let

;; 非 CSI 序列或非 ESC 开头返回 #f
(let ((bv (string->byte-vector "hello")))
  (check (%match-ansi-csi bv 0 (bytevector-length bv)) => #f)
) ;let

(let ((bv (string->byte-vector "\x1b;]0;title\x07;")))
  (check (%match-ansi-csi bv 0 (bytevector-length bv)) => #f)
) ;let

;; 不完整的 CSI 序列（到达末尾未终结）
(let ((bv (string->byte-vector "\x1b;[")))
  (check (%match-ansi-csi bv 0 (bytevector-length bv)) => #f)
) ;let

(let ((bv (string->byte-vector "\x1b;[31;")))
  (check (%match-ansi-csi bv 0 (bytevector-length bv)) => #f)
) ;let

(check-report)
