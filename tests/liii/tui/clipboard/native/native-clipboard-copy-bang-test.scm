;; native-clipboard-copy!
;; 调用操作系统底层命令（clip / pbcopy / wl-copy / xclip / xsel）将文本写入系统剪贴板。
;;
;; 语法
;; ----
;; (native-clipboard-copy! text)
;;
;; 参数
;; ----
;; text : string
;; 欲复制到剪贴板的文本字符串。
;;
;; 返回值
;; ----
;; boolean
;; 成功返回 #t，失败或不支持返回 #f。

(import (scheme base) (liii check) (liii os) (liii tui clipboard native))

(check-set-mode! 'report-failed)

;; 1. 原生剪贴板调用验证（返回布尔值，不抛异常）
(check-true (boolean? (native-clipboard-copy! "test native copy")))
(check-true (boolean? (native-clipboard-copy! "")))

;; 2. 无桌面环境场景测试（模拟 DISPLAY / WAYLAND_DISPLAY 均被清除）
(let ((saved-display (getenv "DISPLAY"))
      (saved-wayland (getenv "WAYLAND_DISPLAY")))
  (dynamic-wind
    (lambda ()
      (unsetenv "DISPLAY")
      (unsetenv "WAYLAND_DISPLAY"))
    (lambda ()
      (check-true (boolean? (native-clipboard-copy! "headless native copy test"))))
    (lambda ()
      (if saved-display (putenv "DISPLAY" saved-display) (unsetenv "DISPLAY"))
      (if saved-wayland (putenv "WAYLAND_DISPLAY" saved-wayland) (unsetenv "WAYLAND_DISPLAY")))))

(check-report)
