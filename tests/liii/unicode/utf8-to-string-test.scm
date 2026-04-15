(import (liii check)
  (liii unicode)
  (liii base)
) ;import


(check-set-mode! 'report-failed)


;; utf8->string
;; 将 UTF-8 编码的字节向量转换为字符串。
;;
;; 语法
;; ----
;; (utf8->string bytevector)
;;
;; 参数
;; ----
;; bytevector : bytevector
;; 包含 UTF-8 编码字节的字节向量。
;;
;; 返回值
;; ----
;; string
;; 转换后的字符串。
;;
;; 错误处理
;; ----
;; value-error 当字节向量包含无效的 UTF-8 编码序列时。
;; type-error 当参数不是字节向量时。


;; 基本 ASCII 测试
(check (utf8->string (bytevector 72 101 108 108 111)
       ) ;utf8->string
  =>
  "Hello"
) ;check
(check (utf8->string #u8(72)) => "H")
(check (utf8->string #u8(72 101))
  =>
  "He"
) ;check


;; 空字节向量
(check (utf8->string #u()) => "")


;; 2 字节 UTF-8 字符 (U+0080 到 U+07FF)
(check (utf8->string #u8(195 164))
  =>
  "ä"
) ;check


;; 3 字节 UTF-8 字符 (U+0800 到 U+FFFF)
(check (utf8->string #u8(228 184 173))
  =>
  "中"
) ;check
(check (utf8->string #u8(230 177 137))
  =>
  "汉"
) ;check
(check (utf8->string #u8(229 173 151))
  =>
  "字"
) ;check


;; 4 字节 UTF-8 字符 (U+10000 到 U+10FFFF)
(check (utf8->string #u8(240 159 145 141))
  =>
  "👍"
) ;check
(check (utf8->string #u8(240 159 154 128))
  =>
  "🚀"
) ;check
(check (utf8->string #u8(240 159 142 137))
  =>
  "🎉"
) ;check
(check (utf8->string #u8(240 159 142 138))
  =>
  "🎊"
) ;check


;; 混合字符测试
(check (utf8->string #u8(240 159 145 141 240 159 154 128)
       ) ;utf8->string
  =>
  "👍🚀"
) ;check
(check (utf8->string #u8(72 101 108 108 111 32 240 159 154 128 32 87 111 114 108 100)
       ) ;utf8->string
  =>
  "Hello 🚀 World"
) ;check
(check (utf8->string #u8(228 189 160 229 165 189 32 240 159 142 137 32 230 181 139 232 175 149)
       ) ;utf8->string
  =>
  "你好 🎉 测试"
) ;check


;; 错误处理测试
(check-catch 'value-error
  (utf8->string (bytevector 255 101 108 108 111)
  ) ;utf8->string
) ;check-catch
(check-catch 'value-error
  (utf8->string (bytevector 128))
) ;check-catch
(check-catch 'value-error
  (utf8->string (bytevector 248 128 128 128 128)
  ) ;utf8->string
) ;check-catch
(check-catch 'value-error
  (utf8->string (bytevector 252 128 128 128 128 128)
  ) ;utf8->string
) ;check-catch


;; 与 string->utf8 互逆操作验证
(check (utf8->string (string->utf8 ""))
  =>
  ""
) ;check
(check (utf8->string (string->utf8 "H"))
  =>
  "H"
) ;check
(check (utf8->string (string->utf8 "Hello"))
  =>
  "Hello"
) ;check
(check (utf8->string (string->utf8 "ä"))
  =>
  "ä"
) ;check
(check (utf8->string (string->utf8 "中"))
  =>
  "中"
) ;check
(check (utf8->string (string->utf8 "👍"))
  =>
  "👍"
) ;check
(check (utf8->string (string->utf8 "汉字书写")
       ) ;utf8->string
  =>
  "汉字书写"
) ;check
(check (utf8->string (string->utf8 "Hello 你好 👍")
       ) ;utf8->string
  =>
  "Hello 你好 👍"
) ;check


(check-report)
