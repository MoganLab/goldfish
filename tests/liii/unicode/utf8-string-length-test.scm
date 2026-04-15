(import (liii check)
  (liii unicode)
  (liii base)
) ;import


(check-set-mode! 'report-failed)


;; utf8-string-length
;; 计算 UTF-8 编码字符串的 Unicode 字符数量（码点数量）。
;;
;; 语法
;; ----
;; (utf8-string-length string)
;;
;; 参数
;; ----
;; string : string
;; UTF-8 编码的字符串。
;;
;; 返回值
;; ----
;; integer
;; 字符串中的 Unicode 字符数量（码点数量）。
;;
;; 描述
;; ----
;; 与 string-length 不同，utf8-string-length 返回的是 Unicode 码点数量，
;; 而不是字节数量。
;;
;; 错误处理
;; ----
;; value-error 当字符串包含无效的 UTF-8 编码序列时。
;; type-error 当参数不是字符串时。


;; 空字符串
(check (utf8-string-length "") => 0)


;; ASCII 字符串
(check (utf8-string-length "Hello")
  =>
  5
) ;check
(check (utf8-string-length "H") => 1)


;; 中文字符（每个字符 3 字节，但 1 个码点）
(check (utf8-string-length "你好")
  =>
  2
) ;check
(check (utf8-string-length "汉字书写")
  =>
  4
) ;check


;; 表情符号（每个 4 字节，但 1 个码点）
(check (utf8-string-length "👍") => 1)
(check (utf8-string-length "🚀") => 1)
(check (utf8-string-length "🎉") => 1)


;; 混合字符
(check (utf8-string-length "Hello 你好")
  =>
  8
) ;check
(check (utf8-string-length "Hello 👍 World")
  =>
  13
) ;check
(check (utf8-string-length "你好 🚀 测试"
       ) ;utf8-string-length
  =>
  7
) ;check
(check (utf8-string-length "👍🚀🎉")
  =>
  3
) ;check


;; 与 string-length 的区别验证
(check-true (> (string-length "中")
              (utf8-string-length "中")
            ) ;>
) ;check-true
(check-true (> (string-length "👍")
              (utf8-string-length "👍")
            ) ;>
) ;check-true
(check-true (= (string-length "Hello")
              (utf8-string-length "Hello")
            ) ;=
) ;check-true


(check-report)
