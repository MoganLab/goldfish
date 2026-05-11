(import (liii check) (liii unicode) (liii base))


(check-set-mode! 'report-failed)


;; utf8->codepoint-at
;; 从 bytevector 的指定位置开始解码 UTF-8 字符为 Unicode 码点。
;;
;; 语法
;; ----
;; (utf8->codepoint-at bytevector start)
;;
;; 参数
;; ----
;; bytevector : bytevector
;; UTF-8 编码的字节向量。
;; start : integer
;; 开始解码的字节偏移量。
;;
;; 返回值
;; ----
;; integer
;; Unicode 码点。
;;
;; 错误处理
;; ----
;; value-error 当 start 越界或包含无效的 UTF-8 编码序列时。
;; type-error 当参数不是字节向量时。


;; 1 字节编码（偏移 0 和偏移 2）
(check (utf8->codepoint-at #u8(72 65 66) 0) => 72)
(check (utf8->codepoint-at #u8(72 65 66) 1) => 65)
(check (utf8->codepoint-at #u8(72 65 66) 2) => 66)


;; 2 字节编码（偏移 0 和偏移 2）
(check (utf8->codepoint-at #u8(194 164 65) 0) => 164)
(check (utf8->codepoint-at #u8(65 194 164) 1) => 164)
(check (utf8->codepoint-at #u8(194 128 66) 0) => 128)
(check (utf8->codepoint-at #u8(223 191 67) 0) => 2047)


;; 3 字节编码（偏移 0 和偏移 3）
(check (utf8->codepoint-at #u8(228 184 173 65) 0) => 20013)
(check (utf8->codepoint-at #u8(65 228 184 173) 1) => 20013)
(check (utf8->codepoint-at #u8(224 160 128 66) 0) => 2048)
(check (utf8->codepoint-at #u8(239 191 191 67) 0) => 65535)


;; 4 字节编码（偏移 0 和偏移 4）
(check (utf8->codepoint-at #u8(240 159 145 141 65) 0) => 128077)
(check (utf8->codepoint-at #u8(65 240 159 145 141) 1) => 128077)
(check (utf8->codepoint-at #u8(240 144 128 128 66) 0) => 65536)
(check (utf8->codepoint-at #u8(244 143 191 191 67) 0) => 1114111)


;; 常见字符
(check (utf8->codepoint-at #u8(240 159 154 128) 0) => 128640)
(check (utf8->codepoint-at #u8(65 240 159 142 137) 1) => 127881)


;; 混合序列，测试从中间位置解码
(check (utf8->codepoint-at #u8(72 228 184 173 240 159 154 128) 0) => 72)
(check (utf8->codepoint-at #u8(72 228 184 173 240 159 154 128) 1) => 20013)
(check (utf8->codepoint-at #u8(72 228 184 173 240 159 154 128) 4) => 128640)


;; 错误处理
(check-catch 'value-error (utf8->codepoint-at #u8() 0))
(check-catch 'value-error (utf8->codepoint-at #u8(72) 1))
(check-catch 'value-error (utf8->codepoint-at #u8(128) 0))
(check-catch 'value-error (utf8->codepoint-at #u8(192 128) 0))
(check-catch 'type-error (utf8->codepoint-at "not-a-bytevector" 0))
(check-catch 'type-error (utf8->codepoint-at 123 0))


(check-report)
