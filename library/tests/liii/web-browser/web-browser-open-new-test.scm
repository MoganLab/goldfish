;; web-browser-open-new
;; 尝试在新的浏览器窗口中打开指定的 URL。
;;
;; 语法
;; ----
;; (web-browser-open-new url)
;;
;; 参数
;; ----
;; url : string?
;; 要打开的 URL 地址。
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
        (liii sys)
        (liii web-browser))

(check-set-mode! 'report-failed)

;; 错误处理测试
(check-catch 'type-error (web-browser-open-new 123))
(check-catch 'value-error (web-browser-open-new ""))
(check-catch 'value-error (web-browser-open-new "-n"))

;; 启动测试
(let ((old-browser (getenv "BROWSER")))
  (putenv "BROWSER" (string-append (executable) " eval #t"))
  (check-true (web-browser-open-new "https://example.com/login"))

  (if old-browser
      (putenv "BROWSER" old-browser)
      (unsetenv "BROWSER")))

(check-report)
