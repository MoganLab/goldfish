(import (liii check)
  (liii unicode)
  (liii base)
) ;import


(check-set-mode! 'report-failed)


;; string->utf8
;; 将字符串转换为 UTF-8 编码的字节向量。
;;
;; 语法
;; ----
;; (string->utf8 string [start [end]])
;;
;; 参数
;; ----
;; string : string
;; 要转换的字符串。
;;
;; start : integer (可选，默认 0)
;; 起始字符位置（基于字符计数）。
;;
;; end : integer (可选，默认字符串末尾)
;; 结束字符位置（基于字符计数）。
;;
;; 返回值
;; ----
;; bytevector
;; 包含 UTF-8 编码字节的字节向量。
;;
;; 错误处理
;; ----
;; out-of-range 当 start 或 end 超出字符串范围时。


;; 基本 ASCII 测试
(check (string->utf8 "Hello")
  =>
  (bytevector 72 101 108 108 111)
) ;check
(check (string->utf8 "") => #u())


;; 多字节字符测试
(check (string->utf8 "ä")
  =>
  #u8(195 164)
) ;check
(check (string->utf8 "中")
  =>
  #u8(228 184 173)
) ;check
(check (string->utf8 "👍")
  =>
  #u8(240 159 145 141)
) ;check
(check (string->utf8 "🚀")
  =>
  #u8(240 159 154 128)
) ;check
(check (string->utf8 "🎉")
  =>
  #u8(240 159 142 137)
) ;check


;; 混合字符测试
(check (string->utf8 "Hello 🚀 World")
  =>
  #u8(72 101 108 108 111 32 240 159 154 128 32 87 111 114 108 100)
) ;check
(check (string->utf8 "你好 🎉 测试")
  =>
  #u8(228 189 160 229 165 189 32 240 159 142 137 32 230 181 139 232 175 149)
) ;check


;; 带 start 和 end 参数的测试
(check (string->utf8 "Hello" 0 0)
  =>
  #u()
) ;check
(check (string->utf8 "Hello" 1 1)
  =>
  #u()
) ;check
(check (string->utf8 "Hello" 2 3)
  =>
  #u8(108)
) ;check
(check (string->utf8 "Hello" 3 5)
  =>
  #u8(108 111)
) ;check
(check (string->utf8 "Hello" 2)
  =>
  #u8(108 108 111)
) ;check
(check (string->utf8 "Hello" 0 3)
  =>
  #u8(72 101 108)
) ;check


;; Unicode 字符范围测试
(check (string->utf8 "汉")
  =>
  #u8(230 177 137)
) ;check
(check (string->utf8 "字")
  =>
  #u8(229 173 151)
) ;check


;; 错误处理测试
(check-catch 'out-of-range
  (string->utf8 "Hello" 2 6)
) ;check-catch
(check-catch 'out-of-range
  (string->utf8 "汉字书写" 4)
) ;check-catch


;; 与 utf8->string 互逆操作验证
(check (utf8->string (string->utf8 "Hello" 1 2)
       ) ;utf8->string
  =>
  "e"
) ;check
(check (utf8->string (string->utf8 "Hello" 0 2)
       ) ;utf8->string
  =>
  "He"
) ;check
(check (utf8->string (string->utf8 "Hello" 2))
  =>
  "llo"
) ;check
(check (utf8->string (string->utf8 "Hello" 2 5)
       ) ;utf8->string
  =>
  "llo"
) ;check


(check-report)
