;; %paste-normalize
;; 规范化粘贴文本：将 CRLF/CR 统一转换为 LF，Tab 转 4 空格并过滤控制字符。
;;
;; 语法
;; ----
;; (%paste-normalize str)
;;
;; 参数
;; ----
;; str : string?
;; 原始粘贴文本。
;;
;; 返回值
;; ----
;; string?
;; 规范化后的文本。

(import (scheme base) (liii check) (liii tui editor paste))

(check-set-mode! 'report-failed)

(check (%paste-normalize "") => "")
(check (%paste-normalize "a\r\nb\rc\td") => "a\nb\nc    d")
(check (%paste-normalize (string #\a (integer->char 1) #\b)) => "ab")
(check (%paste-normalize "你好\r\n世界\t🎉\r测试")
  =>
  "你好\n世界    🎉\n测试"
) ;check

(check-report)
