;; %bracketed-paste-content
;; 提取 bracketed paste 转义序列中包裹的纯文本内容。
;;
;; 语法
;; ----
;; (%bracketed-paste-content str)
;;
;; 参数
;; ----
;; str : string?
;; bracketed paste 转义序列。
;;
;; 返回值
;; ----
;; string?
;; 剥除首尾定界符后的纯文本。

(import (scheme base) (liii check) (liii tui editor paste))

(check-set-mode! 'report-failed)

(check (%bracketed-paste-content "\x1b;[200~hello\x1b;[201~") => "hello")
(check (%bracketed-paste-content "\x1b;[200~你好世界🎉\x1b;[201~")
  =>
  "你好世界🎉"
) ;check

(check-report)
