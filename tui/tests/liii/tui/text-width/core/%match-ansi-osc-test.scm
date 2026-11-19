;; %match-ansi-osc
;; 尝试从字节向量指定位置匹配 ANSI OSC 转义序列。
;;
;; 语法
;; ----
;; (%match-ansi-osc bv i len)
;;
;; 参数
;; ----
;; bv : bytevector?，待匹配的字节向量
;; i : integer?，起始匹配索引
;; len : integer?，字节向量总长度
;;
;; 返回值
;; ----
;; 匹配成功返回 OSC 序列的结束位置（下一字节索引），匹配失败返回 #f。

(import (liii check) (liii tui text-width core))

(check-set-mode! 'report-failed)

;; BEL (0x07) 终结的 OSC 序列
(let ((bv (string->byte-vector "\x1b;]0;title\x07;rest")))
  (check (%match-ansi-osc bv 0 (bytevector-length bv)) => 10)
) ;let

;; ESC \ (27 92) 终结的 OSC 序列
(let ((bv (string->byte-vector "\x1b;]11;?\x1b;\\rest")))
  (check (%match-ansi-osc bv 0 (bytevector-length bv)) => 8)
) ;let

;; C1 ST (0x9C, 156) 终结的 OSC 序列
(let ((bv (bytevector 27 93 48 59 156 97)))
  (check (%match-ansi-osc bv 0 (bytevector-length bv)) => 5)
) ;let

;; 非 OSC 或普通字符返回 #f
(let ((bv (string->byte-vector "hello")))
  (check (%match-ansi-osc bv 0 (bytevector-length bv)) => #f)
) ;let

(let ((bv (string->byte-vector "\x1b;[0m")))
  (check (%match-ansi-osc bv 0 (bytevector-length bv)) => #f)
) ;let

;; 不完整 OSC（无终结符）返回 #f
(let ((bv (string->byte-vector "\x1b;]0;title")))
  (check (%match-ansi-osc bv 0 (bytevector-length bv)) => #f)
) ;let

(check-report)
