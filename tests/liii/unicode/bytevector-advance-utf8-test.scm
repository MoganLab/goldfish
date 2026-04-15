(import (liii check)
  (liii unicode)
  (liii base)
) ;import


(check-set-mode! 'report-failed)


;; bytevector-advance-utf8
;; 从指定字节位置前进到下一个 UTF-8 字符的起始位置。
;;
;; 语法
;; ----
;; (bytevector-advance-utf8 bytevector index [end])
;;
;; 参数
;; ----
;; bytevector : bytevector
;; UTF-8 编码的字节向量。
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
;; 下一个 UTF-8 字符的起始字节位置；如果遇到无效序列则返回当前位置。


;; ASCII 字符测试（1字节编码）
(check (bytevector-advance-utf8 #u8(72 101 108 108 111)
         0
       ) ;bytevector-advance-utf8
  =>
  1
) ;check
(check (bytevector-advance-utf8 #u8(72 101 108 108 111)
         1
       ) ;bytevector-advance-utf8
  =>
  2
) ;check
(check (bytevector-advance-utf8 #u8(72 101 108 108 111)
         2
       ) ;bytevector-advance-utf8
  =>
  3
) ;check
(check (bytevector-advance-utf8 #u8(72 101 108 108 111)
         3
       ) ;bytevector-advance-utf8
  =>
  4
) ;check
(check (bytevector-advance-utf8 #u8(72 101 108 108 111)
         4
       ) ;bytevector-advance-utf8
  =>
  5
) ;check


;; 基本多文种平面字符测试（2字节编码）
(check (bytevector-advance-utf8 #u8(195 164 72)
         0
       ) ;bytevector-advance-utf8
  =>
  2
) ;check
(check (bytevector-advance-utf8 #u8(195 169 101)
         0
       ) ;bytevector-advance-utf8
  =>
  2
) ;check
(check (bytevector-advance-utf8 #u8(195 182 108)
         0
       ) ;bytevector-advance-utf8
  =>
  2
) ;check


;; BMP 字符测试（3字节编码，中文）
(check (bytevector-advance-utf8 #u8(228 184 173 72)
         0
       ) ;bytevector-advance-utf8
  =>
  3
) ;check
(check (bytevector-advance-utf8 #u8(230 177 137 101)
         0
       ) ;bytevector-advance-utf8
  =>
  3
) ;check
(check (bytevector-advance-utf8 #u8(229 173 151 108)
         0
       ) ;bytevector-advance-utf8
  =>
  3
) ;check


;; 辅助平面字符测试（4字节编码，表情符号）
(check (bytevector-advance-utf8 #u8(240 159 145 141 72)
         0
       ) ;bytevector-advance-utf8
  =>
  4
) ;check
(check (bytevector-advance-utf8 #u8(240 159 154 128 101)
         0
       ) ;bytevector-advance-utf8
  =>
  4
) ;check
(check (bytevector-advance-utf8 #u8(240 159 142 137 108)
         0
       ) ;bytevector-advance-utf8
  =>
  4
) ;check


;; 混合字符序列测试
(check (bytevector-advance-utf8 #u8(72 195 164 228 184 173 240 159 145 141)
         0
       ) ;bytevector-advance-utf8
  =>
  1
) ;check
(check (bytevector-advance-utf8 #u8(72 195 164 228 184 173 240 159 145 141)
         1
       ) ;bytevector-advance-utf8
  =>
  3
) ;check
(check (bytevector-advance-utf8 #u8(72 195 164 228 184 173 240 159 145 141)
         3
       ) ;bytevector-advance-utf8
  =>
  6
) ;check
(check (bytevector-advance-utf8 #u8(72 195 164 228 184 173 240 159 145 141)
         6
       ) ;bytevector-advance-utf8
  =>
  10
) ;check


;; 边界条件测试
(check (bytevector-advance-utf8 #u() 0)
  =>
  0
) ;check
(check (bytevector-advance-utf8 #u8(72) 0)
  =>
  1
) ;check
(check (bytevector-advance-utf8 #u8(72) 1)
  =>
  1
) ;check


;; 无效 UTF-8 序列测试
(check (bytevector-advance-utf8 #u8(128) 0)
  =>
  0
) ;check
(check (bytevector-advance-utf8 #u8(194) 0)
  =>
  0
) ;check
(check (bytevector-advance-utf8 #u8(228 184) 0)
  =>
  0
) ;check
(check (bytevector-advance-utf8 #u8(240 159 145)
         0
       ) ;bytevector-advance-utf8
  =>
  0
) ;check
(check (bytevector-advance-utf8 #u8(255) 0)
  =>
  0
) ;check


;; 无效延续字节测试
(check (bytevector-advance-utf8 #u8(194 0) 0)
  =>
  0
) ;check
(check (bytevector-advance-utf8 #u8(228 0 173)
         0
       ) ;bytevector-advance-utf8
  =>
  0
) ;check
(check (bytevector-advance-utf8 #u8(240 159 0 141)
         0
       ) ;bytevector-advance-utf8
  =>
  0
) ;check


;; 结束位置参数测试
(check (bytevector-advance-utf8 #u8(72 101 108 108 111)
         0
         1
       ) ;bytevector-advance-utf8
  =>
  1
) ;check
(check (bytevector-advance-utf8 #u8(72 101 108 108 111)
         0
         2
       ) ;bytevector-advance-utf8
  =>
  1
) ;check
(check (bytevector-advance-utf8 #u8(195 164 72)
         0
         2
       ) ;bytevector-advance-utf8
  =>
  2
) ;check
(check (bytevector-advance-utf8 #u8(195 164 72)
         0
         3
       ) ;bytevector-advance-utf8
  =>
  2
) ;check


(check-report)
