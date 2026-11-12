;; web-browser-command
;; 解析在当前系统和环境下打开指定 URL 所执行的完整命令行参数列表。
;;
;; 语法
;; ----
;; (web-browser-command url)
;; (web-browser-command url new)
;;
;; 参数
;; ----
;; url : string?
;; 要打开的 URL 地址。
;; new : integer?
;; 可选的窗口打开模式：0 表示默认，1 表示尝试新窗口，2 表示尝试新标签页。
;;
;; 返回值
;; ----
;; (list-of string?) 或 #f
;; 返回准备执行的命令行列表，找不到可用浏览器时返回 #f。
;;
;; 错误处理
;; ----
;; url 不是字符串时抛出 type-error；
;; url 为空字符串、包含前导横杠或包含 ASCII 控制字符时抛出 value-error。

(import (liii check)
        (liii os)
        (liii web-browser))

(check-set-mode! 'report-failed)

;; 参数类型校验
(check-catch 'type-error (web-browser-command 123))
(check-catch 'type-error (web-browser-command 'http))
(check-catch 'type-error (web-browser-command #f))

;; URL 安全校验（防止参数注入与控制字符截断）
(check-catch 'value-error (web-browser-command ""))
(check-catch 'value-error (web-browser-command "   "))
(check-catch 'value-error (web-browser-command "-h"))
(check-catch 'value-error (web-browser-command "  --proxy=bad"))
(check-catch 'value-error (web-browser-command "https://example.com\n"))
(check-catch 'value-error (web-browser-command "https://example.com\r"))

;; 基本解析测试（测试环境变量自定义与回退机制）
(let ((old-browser (getenv "BROWSER")))
  ;; 自定义 BROWSER 命令（无 %s 时自动追加）
  (putenv "BROWSER" "sh")
  (let ((cmd (web-browser-command "https://example.com/oauth")))
    (check-true (pair? cmd))
    (check (cadr cmd) => "https://example.com/oauth"))

  ;; 自定义 BROWSER 占位符替换
  (putenv "BROWSER" "sh -c %s")
  (let ((cmd (web-browser-command "https://example.com/login")))
    (check-true (pair? cmd))
    (check (caddr cmd) => "https://example.com/login"))

  ;; 恢复环境变量
  (if old-browser
      (putenv "BROWSER" old-browser)
      (unsetenv "BROWSER")))

(check-report)
