(import (liii check)
  (liii unicode)
  (liii base)
) ;import


(check-set-mode! 'report-failed)


;; utf8-substring
;; 基于 Unicode 字符位置提取子字符串。
;;
;; 语法
;; ----
;; (utf8-substring string [start [end]])
;;
;; 参数
;; ----
;; string : string
;; UTF-8 编码的字符串。
;;
;; start : integer (可选，默认 0)
;; 起始字符位置（基于 Unicode 字符计数）。
;;
;; end : integer (可选，默认字符串末尾)
;; 结束字符位置（基于 Unicode 字符计数）。
;;
;; 返回值
;; ----
;; string
;; 从 start 到 end 的子字符串。
;;
;; 描述
;; ----
;; 与 string-substring 不同，utf8-substring 基于 Unicode 字符位置而非字节位置进行提取。
;;
;; 错误处理
;; ----
;; out-of-range 当 start 或 end 超出字符串范围时。


;; 基本 ASCII 测试
(check (utf8-substring "Hello" 0 5)
  =>
  "Hello"
) ;check
(check (utf8-substring "Hello" 0 2)
  =>
  "He"
) ;check
(check (utf8-substring "Hello" 2 4)
  =>
  "ll"
) ;check
(check (utf8-substring "Hello" 3)
  =>
  "lo"
) ;check
(check (utf8-substring "Hello" 1)
  =>
  "ello"
) ;check


;; 空字符串
(check (utf8-substring "" 0 0) => "")


;; 中文字符测试
(check (utf8-substring "汉字书写" 0 2)
  =>
  "汉字"
) ;check
(check (utf8-substring "汉字书写" 2)
  =>
  "书写"
) ;check
(check (utf8-substring "汉字书写" 1 3)
  =>
  "字书"
) ;check
(check (utf8-substring "你好世界" 0 2)
  =>
  "你好"
) ;check


;; 混合字符测试
(check (utf8-substring "Hello 你好" 0 8)
  =>
  "Hello 你好"
) ;check
(check (utf8-substring "Hello 你好" 6)
  =>
  "你好"
) ;check
(check (utf8-substring "Hello 你好" 0 6)
  =>
  "Hello "
) ;check


;; 表情符号测试
(check (utf8-substring "👍🚀🎉" 0 2)
  =>
  "👍🚀"
) ;check
(check (utf8-substring "👍🚀🎉" 1)
  =>
  "🚀🎉"
) ;check
(check (utf8-substring "👍🚀🎉" 1 2)
  =>
  "🚀"
) ;check


;; 复杂混合测试
(check (utf8-substring "Hello 👍 World" 0 7)
  =>
  "Hello 👍"
) ;check
(check (utf8-substring "Hello 👍 World" 6 11)
  =>
  "👍 Wor"
) ;check


;; 错误处理
(check-catch 'out-of-range
  (utf8-substring "Hello" 0 6)
) ;check-catch
(check-catch 'out-of-range
  (utf8-substring "汉字" 0 3)
) ;check-catch


(check-report)
