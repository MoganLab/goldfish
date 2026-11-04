(import (scheme base)
        (scheme process-context)
        (liii check)
        (liii os))

(check-set-mode! 'report-failed)

;; os-set-clipboard-text!
;; 将文本设置到系统剪贴板。
;;
;; 语法
;; ----
;; (os-set-clipboard-text! text)
;;
;; 参数
;; ----
;; text : string
;; 要设置到剪贴板的纯文本字符串。
;;
;; 返回值
;; -----
;; boolean?
;; 设置成功返回 #t，失败或不支持返回 #f。
;;
;; 说明
;; ----
;; 1. 在 Windows 平台上直接调用 Win32 原生剪贴板 API；
;; 2. 在 Linux 平台上通过 dlopen 动态加载 X11 API 设置剪贴板（支持原生 X11 及 XWayland 环境），无需安装外部命令行工具；
;; 3. 若处于无桌面环境（无 DISPLAY），安全返回 #f，不发生异常或崩溃。

;; 1. 参数类型检查
(check-catch 'type-error (os-set-clipboard-text! 123))
(check-catch 'type-error (os-set-clipboard-text! #\a))
(check-catch 'type-error (os-set-clipboard-text! 'symbol))
(check-catch 'type-error (os-set-clipboard-text! (list "text")))

;; 2. 无桌面环境场景测试（模拟 DISPLAY / WAYLAND_DISPLAY 均为空或被清除）
(let ((saved-display (getenv "DISPLAY"))
      (saved-wayland (getenv "WAYLAND_DISPLAY")))
  (dynamic-wind
    (lambda ()
      (unsetenv "DISPLAY")
      (unsetenv "WAYLAND_DISPLAY"))
    (lambda ()
      ;; 在无桌面环境下，Linux 应安全返回 #f，不发生异常或崩溃
      (if (os-linux?)
        (check-false (os-set-clipboard-text! "headless clipboard test"))
        (check-true (boolean? (os-set-clipboard-text! "headless clipboard test"))))
      (if (os-linux?)
        (check-false (os-set-clipboard-text! ""))
        (check-true (boolean? (os-set-clipboard-text! "")))))
    (lambda ()
      (if saved-display (putenv "DISPLAY" saved-display) (unsetenv "DISPLAY"))
      (if saved-wayland (putenv "WAYLAND_DISPLAY" saved-wayland) (unsetenv "WAYLAND_DISPLAY")))))

;; 3. 无效 DISPLAY 环境变量场景测试
(let ((saved-display (getenv "DISPLAY")))
  (dynamic-wind
    (lambda ()
      ;; 设置指向不存在的 X11 Display
      (putenv "DISPLAY" ":9999.0"))
    (lambda ()
      (if (os-linux?)
        (check-false (os-set-clipboard-text! "invalid display test"))
        (check-true (boolean? (os-set-clipboard-text! "invalid display test")))))
    (lambda ()
      (if saved-display (putenv "DISPLAY" saved-display) (unsetenv "DISPLAY")))))

;; 4. 正常环境返回布尔值
(check-true (boolean? (os-set-clipboard-text! "normal test")))

(check-report)
