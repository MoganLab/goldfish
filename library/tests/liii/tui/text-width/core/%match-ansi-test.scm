;; %match-ansi
;; 尝试从字节向量指定位置匹配 ANSI 转义序列（CSI 或 OSC）。
;;
;; 语法
;; ----
;; (%match-ansi bv i len)
;;
;; 参数
;; ----
;; bv : bytevector?，待匹配的字节向量
;; i : integer?，起始匹配索引
;; len : integer?，字节向量总长度
;;
;; 返回值
;; ----
;; 匹配成功返回 ANSI 序列的结束位置（下一字节索引），匹配失败返回 #f。

(import (liii check) (liii tui text-width core))

(check-set-mode! 'report-failed)

;; 匹配 CSI 序列
(let ((bv (string->byte-vector "\x1b;[31mhello")))
  (check (%match-ansi bv 0 (bytevector-length bv)) => 5)
) ;let

;; 匹配 OSC 序列
(let ((bv (string->byte-vector "\x1b;]0;title\x07;�\f")))
  (check (%match-ansi bv 0 (bytevector-length bv)) => 10)
) ;let

;; 普通文本返回 #f
(let ((bv (string->byte-vector "hello world")))
  (check (%match-ansi bv 0 (bytevector-length bv)) => #f)
) ;let

;; 不完整序列返回 #f
(let ((bv (string->byte-vector "\x1b;[")))
  (check (%match-ansi bv 0 (bytevector-length bv)) => #f)
) ;let

(let ((bv (string->byte-vector "\x1b;]0;title")))
  (check (%match-ansi bv 0 (bytevector-length bv)) => #f)
) ;let

(check-report)
