(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)

;; string->utf8
;; 将字符串按 UTF-8 编码转换为字节向量。
;;
;; 语法
;; ----
;; (string->utf8 string [start [end]])
;;
;; 参数
;; ----
;; string : string?
;; 待转换的字符串。
;;
;; start : exact-nonnegative-integer?（可选，默认 0）
;; 起始字符索引（按字符计数，非字节）。
;;
;; end : exact-nonnegative-integer?（可选，默认字符串长度）
;; 结束字符索引，可取到字符串长度（含尾）。
;;
;; 返回值
;; ------
;; bytevector?
;; UTF-8 编码的字节向量。
;;
;; 错误处理
;; --------
;; - string 非 string?               -> type-error
;; - start/end 为负或超出 [0, N]      -> out-of-range
;; - start > end                       -> out-of-range
;; - start == N（空切片位置）          -> out-of-range

;; ============================================================
;; 1. 空串与纯 ASCII
;; ============================================================
(check (string->utf8 "") => #u8())
(check (string->utf8 "a") => #u8(97))
(check (string->utf8 "Hello") => #u8(72 101 108 108 111))

;; ============================================================
;; 2. 全码点范围（1~4 字节 UTF-8）
;; ============================================================
;; 1 字节 ASCII（NUL 与 DEL 边界用 integer->char 构造，避免字面量被格式化吞掉）
(check (string->utf8 (string (integer->char 0))) => #u8(0))
(check (string->utf8 (string (integer->char 127))) => #u8(127))

;; 2 字节（U+0080 ~ U+07FF）
(check (string->utf8 "ä") => #u8(195 164))
(check (string->utf8 "©") => #u8(194 169))

;; 3 字节（U+0800 ~ U+FFFF，CJK 主力区）
(check (string->utf8 "中") => #u8(228 184 173))
(check (string->utf8 "世界") => #u8(228 184 150 231 149 140))
(check (string->utf8 "汉") => #u8(230 177 137))
(check (string->utf8 "字") => #u8(229 173 151))

;; 4 字节（U+10000 ~ U+10FFFF，含 emoji 与代理对边界）
(check (string->utf8 "👍") => #u8(240 159 145 141))
(check (string->utf8 "🚀") => #u8(240 159 154 128))
(check (string->utf8 "🎉") => #u8(240 159 142 137))

;; 混合多字节
(check (string->utf8 "Hello 🚀 World")
  =>
  #u8(72 101 108 108 111 32 240 159 154 128 32 87 111 114 108 100)
) ;check
(check (string->utf8 "你好 🎉 测试")
  =>
  #u8(228 189 160 229 165 189 32 240 159 142 137 32 230 181 139 232 175 149)
) ;check

;; ============================================================
;; 3. start / end 参数边界
;; ============================================================
;; 默认参数（end 缺省 = 取到串尾）
(check (string->utf8 "abc" 1) => #u8(98 99))
(check (string->utf8 "Hello" 2) => #u8(108 108 111))

;; start / end 同时给出
(check (string->utf8 "abc" 0 2) => #u8(97 98))
(check (string->utf8 "Hello" 0 3) => #u8(72 101 108))
(check (string->utf8 "Hello" 2 3) => #u8(108))
(check (string->utf8 "Hello" 3 5) => #u8(108 111))

;; start == end（含非 0 位置）-> 空字节向量
(check (string->utf8 "Hello" 0 0) => #u8())
(check (string->utf8 "Hello" 1 1) => #u8())
(check (string->utf8 "abc" 1 1) => #u8())

;; end == 串长 N（合法边界）
(check (string->utf8 "Hello" 2 5) => #u8(108 108 111))
(check (string->utf8 "Hello" 0 5) => #u8(72 101 108 108 111))

;; 空串 + start=0（N=0 时 start 校验被跳过）
(check (string->utf8 "" 0) => #u8())

;; 多字节串上的字符级切片（start/end 按字符计数，不是字节）
(check (string->utf8 "你好世界" 1 3) => #u8(229 165 189 228 184 150))

;; ============================================================
;; 4. 错误契约
;; ============================================================
;; 参数缺失 / 类型错误 -> type-error
(check-catch 'type-error (string->utf8))
(check-catch 'type-error (string->utf8 '()))
(check-catch 'type-error (string->utf8 123))
(check-catch 'type-error (string->utf8 'sym))

;; start / end 越界 -> out-of-range
(check-catch 'out-of-range (string->utf8 "abc" -1))
(check-catch 'out-of-range (string->utf8 "Hello" 2 6))
(check-catch 'out-of-range (string->utf8 "汉字书写" 4))

;; start == N（等于串长，非合法切片位置）-> out-of-range
(check-catch 'out-of-range (string->utf8 "Hello" 5))

;; start > end -> out-of-range
(check-catch 'out-of-range (string->utf8 "Hello" 3 2))

;; ============================================================
;; 5. round-trip：string->utf8 ∘ utf8->string 互逆
;; ============================================================
(check (utf8->string (string->utf8 "Hello")) => "Hello")
(check (utf8->string (string->utf8 "")) => "")
(check (utf8->string (string->utf8 "你好世界")) => "你好世界")
(check (utf8->string (string->utf8 "Hello 🚀 World")) => "Hello 🚀 World")

;; 切片上的 round-trip
(check (utf8->string (string->utf8 "Hello" 1 2)) => "e")
(check (utf8->string (string->utf8 "Hello" 0 2)) => "He")
(check (utf8->string (string->utf8 "Hello" 2)) => "llo")
(check (utf8->string (string->utf8 "Hello" 2 5)) => "llo")
(check (utf8->string (string->utf8 "你好世界" 1 3)) => "好世")

(check-report)
