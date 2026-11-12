;; web-browser-open
;; 使用默认浏览器打开指定的 URL。
;;
;; 语法
;; ----
;; (web-browser-open url)
;; (web-browser-open url new)
;;
;; 参数
;; ----
;; url : string?
;; 要打开的 URL 地址。
;; new : integer?
;; 可选的打开模式：0 为默认，1 为新窗口，2 为新标签页。
;;
;; 返回值
;; ----
;; boolean?
;; 成功启动浏览器进程返回 #t，无可执行浏览器或启动失败返回 #f。
;;
;; 错误处理
;; ----
;; url 不是字符串时抛出 type-error；
;; url 为空字符串、包含前导横杠或包含 ASCII 控制字符时抛出 value-error。

(import (liii check)
        (liii os)
        (liii web-browser))

(check-set-mode! 'report-failed)

;; 错误处理测试
(check-catch 'type-error (web-browser-open 123))
(check-catch 'type-error (web-browser-open 'url))
(check-catch 'value-error (web-browser-open ""))
(check-catch 'value-error (web-browser-open "   "))
(check-catch 'value-error (web-browser-open "-h"))
(check-catch 'value-error (web-browser-open "  --version"))
(check-catch 'value-error (web-browser-open "https://example.com\n"))

;; 启动测试（通过安全的环境变量命令覆盖，避免在桌面中弹出网页）
(let ((old-browser (getenv "BROWSER")))
  (putenv "BROWSER" "true")
  (check-true (web-browser-open "https://example.com/oauth/authorize"))
  (check-true (web-browser-open "https://example.com/oauth/authorize" 0))

  (if old-browser
      (putenv "BROWSER" old-browser)
      (unsetenv "BROWSER")))

(check-report)
