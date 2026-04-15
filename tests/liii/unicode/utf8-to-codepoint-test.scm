(import (liii check)
  (liii unicode)
  (liii base)
) ;import


(check-set-mode! 'report-failed)


;; utf8->codepoint
;; 将 UTF-8 编码的字节向量转换为 Unicode 码点。
;;
;; 语法
;; ----
;; (utf8->codepoint bytevector)
;;
;; 参数
;; ----
;; bytevector : bytevector
;; UTF-8 编码的字节向量。
;;
;; 返回值
;; ----
;; integer
;; Unicode 码点。
;;
;; 错误处理
;; ----
;; value-error 当字节向量为空或包含无效的 UTF-8 编码序列时。
;; type-error 当参数不是字节向量时。


;; 1 字节编码
(check (utf8->codepoint #u8(72)) => 72)
(check (utf8->codepoint #u8(0)) => 0)
(check (utf8->codepoint #u8(127))
  =>
  127
) ;check


;; 2 字节编码
(check (utf8->codepoint #u8(194 164))
  =>
  164
) ;check
(check (utf8->codepoint #u8(194 128))
  =>
  128
) ;check
(check (utf8->codepoint #u8(223 191))
  =>
  2047
) ;check


;; 3 字节编码
(check (utf8->codepoint #u8(228 184 173))
  =>
  20013
) ;check
(check (utf8->codepoint #u8(224 160 128))
  =>
  2048
) ;check
(check (utf8->codepoint #u8(239 191 191))
  =>
  65535
) ;check


;; 4 字节编码
(check (utf8->codepoint #u8(240 159 145 141))
  =>
  128077
) ;check
(check (utf8->codepoint #u8(240 144 128 128))
  =>
  65536
) ;check
(check (utf8->codepoint #u8(244 143 191 191))
  =>
  1114111
) ;check


;; 常见字符
(check (utf8->codepoint #u8(240 159 154 128))
  =>
  128640
) ;check
(check (utf8->codepoint #u8(240 159 142 137))
  =>
  127881
) ;check


;; 与 codepoint->utf8 互逆操作
(check (utf8->codepoint (codepoint->utf8 72))
  =>
  72
) ;check
(check (utf8->codepoint (codepoint->utf8 20013)
       ) ;utf8->codepoint
  =>
  20013
) ;check
(check (utf8->codepoint (codepoint->utf8 128077)
       ) ;utf8->codepoint
  =>
  128077
) ;check
(check (utf8->codepoint (codepoint->utf8 1114111)
       ) ;utf8->codepoint
  =>
  1114111
) ;check


;; 错误处理
(check-catch 'value-error
  (utf8->codepoint #u())
) ;check-catch
(check-catch 'value-error
  (utf8->codepoint #u8(128))
) ;check-catch
(check-catch 'value-error
  (utf8->codepoint #u8(192 128))
) ;check-catch
(check-catch 'type-error
  (utf8->codepoint "not-a-bytevector")
) ;check-catch
(check-catch 'type-error
  (utf8->codepoint 123)
) ;check-catch


(check-report)
