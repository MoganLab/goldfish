(import (liii check)
  (liii unicode)
  (liii base)
) ;import


(check-set-mode! 'report-failed)


;; utf16le->utf8
;; 将 UTF-16LE 编码的字节向量转换为 UTF-8 编码的字节向量。
;;
;; 语法
;; ----
;; (utf16le->utf8 bytevector)
;;
;; 参数
;; ----
;; bytevector : bytevector
;; UTF-16LE 编码的字节向量。
;;
;; 返回值
;; ----
;; bytevector
;; UTF-8 编码的字节向量。
;;
;; 错误处理
;; ----
;; value-error 当字节向量包含无效的 UTF-16LE 编码序列时。
;; type-error 当参数不是字节向量时。


;; ASCII 字符（2 字节 UTF-16LE -> 1 字节 UTF-8）
(check (utf16le->utf8 #u8(72 0))
  =>
  #u8(72)
) ;check
(check (utf16le->utf8 #u8(72 0 101 0 108 0 108 0 111 0)
       ) ;utf16le->utf8
  =>
  #u8(72 101 108 108 111)
) ;check


;; BMP 字符（2 字节 UTF-16LE -> 2-3 字节 UTF-8）
(check (utf16le->utf8 #u8(228 0))
  =>
  #u8(195 164)
) ;check
(check (utf16le->utf8 #u8(45 78))
  =>
  #u8(228 184 173)
) ;check


;; 代理对（4 字节 UTF-16LE -> 4 字节 UTF-8）
(check (utf16le->utf8 #u8(61 216 77 220))
  =>
  #u8(240 159 145 141)
) ;check
(check (utf16le->utf8 #u8(61 216 128 222))
  =>
  #u8(240 159 154 128)
) ;check


;; 空字节向量
(check (utf16le->utf8 #u8()) => #u8())


;; 与 utf8->utf16le 互逆操作
(check (utf16le->utf8 (utf8->utf16le #u8(72)))
  =>
  #u8(72)
) ;check
(check (utf16le->utf8 (utf8->utf16le #u8(228 184 173))
       ) ;utf16le->utf8
  =>
  #u8(228 184 173)
) ;check
(check (utf16le->utf8 (utf8->utf16le #u8(240 159 145 141))
       ) ;utf16le->utf8
  =>
  #u8(240 159 145 141)
) ;check


;; 错误处理
(check-catch 'value-error
  (utf16le->utf8 #u8(0))
) ;check-catch
(check-catch 'value-error
  (utf16le->utf8 #u8(0 216))
) ;check-catch
(check-catch 'type-error
  (utf16le->utf8 "not-a-bytevector")
) ;check-catch


(check-report)
