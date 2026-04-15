(import (liii check)
  (liii unicode)
  (liii base)
) ;import


(check-set-mode! 'report-failed)


;; bytevector-utf16le-advance
;; 从指定字节位置前进到下一个 UTF-16LE 字符的起始位置。
;;
;; 语法
;; ----
;; (bytevector-utf16le-advance bytevector index [end])
;;
;; 参数
;; ----
;; bytevector : bytevector
;; UTF-16LE 编码的字节向量。
;;
;; index : integer
;; 当前字节位置（起始索引）。
;;
;; end : integer (可选，默认字节向量长度)
;; 字节向量的结束位置。
;;
;; 返回值
;; ----
;; integer
;; 下一个 UTF-16LE 字符的起始字节位置；如果遇到无效序列则返回当前位置。


;; ASCII 字符测试（2字节编码）
(check (bytevector-utf16le-advance #u8(72 0 101 0 108 0 108 0 111 0)
         0
       ) ;bytevector-utf16le-advance
  =>
  2
) ;check
(check (bytevector-utf16le-advance #u8(72 0 101 0 108 0 108 0 111 0)
         2
       ) ;bytevector-utf16le-advance
  =>
  4
) ;check
(check (bytevector-utf16le-advance #u8(72 0 101 0 108 0 108 0 111 0)
         4
       ) ;bytevector-utf16le-advance
  =>
  6
) ;check
(check (bytevector-utf16le-advance #u8(72 0 101 0 108 0 108 0 111 0)
         6
       ) ;bytevector-utf16le-advance
  =>
  8
) ;check
(check (bytevector-utf16le-advance #u8(72 0 101 0 108 0 108 0 111 0)
         8
       ) ;bytevector-utf16le-advance
  =>
  10
) ;check


;; 基本多文种平面字符测试（2字节编码）
(check (bytevector-utf16le-advance #u8(228 0 72 0)
         0
       ) ;bytevector-utf16le-advance
  =>
  2
) ;check
(check (bytevector-utf16le-advance #u8(233 0 101 0)
         0
       ) ;bytevector-utf16le-advance
  =>
  2
) ;check
(check (bytevector-utf16le-advance #u8(45 78 72 0)
         0
       ) ;bytevector-utf16le-advance
  =>
  2
) ;check


;; 辅助平面字符测试（4字节编码，代理对）
(check (bytevector-utf16le-advance #u8(61 216 77 220 72 0)
         0
       ) ;bytevector-utf16le-advance
  =>
  4
) ;check
(check (bytevector-utf16le-advance #u8(61 216 128 222 101 0)
         0
       ) ;bytevector-utf16le-advance
  =>
  4
) ;check
(check (bytevector-utf16le-advance #u8(60 216 137 223 108 0)
         0
       ) ;bytevector-utf16le-advance
  =>
  4
) ;check


;; 混合字符序列测试
(check (bytevector-utf16le-advance #u8(72 0 228 0 45 78 61 216 77 220)
         0
       ) ;bytevector-utf16le-advance
  =>
  2
) ;check
(check (bytevector-utf16le-advance #u8(72 0 228 0 45 78 61 216 77 220)
         2
       ) ;bytevector-utf16le-advance
  =>
  4
) ;check
(check (bytevector-utf16le-advance #u8(72 0 228 0 45 78 61 216 77 220)
         4
       ) ;bytevector-utf16le-advance
  =>
  6
) ;check
(check (bytevector-utf16le-advance #u8(72 0 228 0 45 78 61 216 77 220)
         6
       ) ;bytevector-utf16le-advance
  =>
  10
) ;check


;; 边界条件测试
(check (bytevector-utf16le-advance #u8() 0)
  =>
  0
) ;check
(check (bytevector-utf16le-advance #u8(72 0) 0)
  =>
  2
) ;check
(check (bytevector-utf16le-advance #u8(72 0) 2)
  =>
  2
) ;check


;; 无效 UTF-16LE 序列测试
(check (bytevector-utf16le-advance #u8(72) 0)
  =>
  0
) ;check
(check (bytevector-utf16le-advance #u8(61 216)
         0
       ) ;bytevector-utf16le-advance
  =>
  0
) ;check
(check (bytevector-utf16le-advance #u8(61 216 77)
         0
       ) ;bytevector-utf16le-advance
  =>
  0
) ;check
(check (bytevector-utf16le-advance #u8(0 220 0 0)
         0
       ) ;bytevector-utf16le-advance
  =>
  0
) ;check
(check (bytevector-utf16le-advance #u8(61 216 0 0)
         0
       ) ;bytevector-utf16le-advance
  =>
  0
) ;check


;; 结束位置参数测试
(check (bytevector-utf16le-advance #u8(72 0 101 0 108 0)
         0
         2
       ) ;bytevector-utf16le-advance
  =>
  2
) ;check
(check (bytevector-utf16le-advance #u8(72 0 101 0 108 0)
         0
         4
       ) ;bytevector-utf16le-advance
  =>
  2
) ;check
(check (bytevector-utf16le-advance #u8(228 0 72 0)
         0
         2
       ) ;bytevector-utf16le-advance
  =>
  2
) ;check
(check (bytevector-utf16le-advance #u8(228 0 72 0)
         0
         4
       ) ;bytevector-utf16le-advance
  =>
  2
) ;check


(check-report)
