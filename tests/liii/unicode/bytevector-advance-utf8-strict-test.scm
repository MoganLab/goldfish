(import (liii check)
        (liii unicode))

;; bytevector-advance-utf8-strict
;; 严格 UTF-8 序列推进：返回下一个字节位置；非法时返回原位置。
;;
;; 语法
;; ----
;; (bytevector-advance-utf8-strict bv index)
;; (bytevector-advance-utf8-strict bv index end)
;;
;; 参数
;; ----
;; bv : bytevector?
;; 输入字节序列。
;;
;; index : integer?
;; 当前字节位置。
;;
;; end : integer? 可选
;; 结束位置，默认为字节序列长度。
;;
;; 返回值
;; ----
;; integer
;; 合法序列返回下一个字节位置，非法返回原位置（表示无法推进）。
;;
;; 注意
;; ----
;; 与 bytevector-advance-utf8（宽松版）不同，严格版额外拒绝：
;; - 孤立续字节（0x80-0xBF 不能作为首字节）、0xC0/0xC1 与 0xF5-0xFF 首字节；
;; - 第二字节越界的序列：E0:A0-BF、ED:80-9F、F0:90-BF、F4:80-8F
;;   （过长编码、代理区、超 U+10FFFF）。
;; index 大于等于 end 时返回 index 本身，不抛错。
;; 只做序列推进判断，不分配对象；utf8-replace-invalid 与宽度计算
;; （display-width 等）均以其为严格性基准。

(check-set-mode! 'report-failed)


;; 合法序列正常推进
(check (bytevector-advance-utf8-strict #u8(65 66) 0) => 1)
(check (bytevector-advance-utf8-strict #u8(228 189 160 97) 0) => 3)
(check (bytevector-advance-utf8-strict #u8(240 159 152 128) 0) => 4)
(check (bytevector-advance-utf8-strict #u8(97 228 189 160) 1) => 4)

;; end 越界保护
(check (bytevector-advance-utf8-strict #u8(228 189 160) 0 2) => 0)
(check (bytevector-advance-utf8-strict #u8(97) 1) => 1)

;; 孤立续字节（0x80-0xBF 不能作为首字节）
(check (bytevector-advance-utf8-strict #u8(128) 0) => 0)
(check (bytevector-advance-utf8-strict #u8(191) 0) => 0)

;; 非法首字节（0xC0/0xC1 过长编码、0xF5-0xFF）
(check (bytevector-advance-utf8-strict #u8(192 128) 0) => 0)
(check (bytevector-advance-utf8-strict #u8(193 128) 0) => 0)
(check (bytevector-advance-utf8-strict #u8(245 128 128 128) 0) => 0)
(check (bytevector-advance-utf8-strict #u8(255) 0) => 0)

;; 截断的多字节序列（末尾不完整）
(check (bytevector-advance-utf8-strict #u8(229 165) 0) => 0)
(check (bytevector-advance-utf8-strict #u8(240 159 152) 0) => 0)

;; 第二字节越界：E0 须 A0-BF（拒 overlong < U+0800）
(check (bytevector-advance-utf8-strict #u8(224 159 128) 0) => 0)
(check (bytevector-advance-utf8-strict #u8(224 160 128) 0) => 3)

;; 第二字节越界：ED 须 80-9F（拒代理 D800-DFFF）
(check (bytevector-advance-utf8-strict #u8(237 160 128) 0) => 0)
(check (bytevector-advance-utf8-strict #u8(237 159 191) 0) => 3)

;; 第二字节越界：F0 须 90-BF（拒 overlong < U+10000）
(check (bytevector-advance-utf8-strict #u8(240 143 191 191) 0) => 0)
(check (bytevector-advance-utf8-strict #u8(240 144 128 128) 0) => 4)

;; 第二字节越界：F4 须 80-8F（拒超 U+10FFFF）
(check (bytevector-advance-utf8-strict #u8(244 144 128 128) 0) => 0)
(check (bytevector-advance-utf8-strict #u8(244 143 191 191) 0) => 4)

;; 后续续字节非法（第二字节合法、第三字节非法）
(check (bytevector-advance-utf8-strict #u8(224 160 65) 0) => 0)

(check-report)
