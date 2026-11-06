;; native-clipboard-text
;; 从 Linux 外部系统剪贴板读取文本，支持注入命令执行器进行测试。
;;
;; 语法
;; ----
;; (native-clipboard-text [runner])
;;
;; 参数
;; ----
;; runner : procedure（可选）
;; 接收命令参数列表，返回剪贴板文本或 #f。
;;
;; 返回值
;; ----
;; string 或 #f
;; 非空剪贴板文本；剪贴板为空、读取失败或平台不支持时返回 #f。

(import (scheme base)
  (scheme process-context)
  (liii check)
  (liii os)
  (liii tui clipboard native)
) ;import

(check-set-mode! 'report-failed)

(when (os-linux?)
  (let ((saved-wayland (getenv "WAYLAND_DISPLAY")) (saved-display (getenv "DISPLAY")))
    (dynamic-wind (lambda () #t)
      (lambda ()
        ;; Wayland 优先使用 wl-paste。
        (putenv "WAYLAND_DISPLAY" "test-wayland")
        (putenv "DISPLAY" "test-x11")
        (let ((calls '()))
          (check
            (native-clipboard-text (lambda (command) (set! calls (cons command calls)) "来自 Wayland")
            ) ;native-clipboard-text
            =>
            "来自 Wayland"
          ) ;check
          (check (reverse calls) => '(("wl-paste" "--no-newline" "--type" "text")))
        ) ;let

        ;; X11 下 xclip 失败时回退到 xsel。
        (unsetenv "WAYLAND_DISPLAY")
        (let ((calls '()))
          (check
            (native-clipboard-text
              (lambda (command)
                (set! calls (cons command calls))
                (if (equal? (car command) "xclip") #f "来自 X11")
              ) ;lambda
            ) ;native-clipboard-text
            =>
            "来自 X11"
          ) ;check
          (check (reverse calls)
            =>
            '(("xclip" "-selection" "clipboard" "-out")
              ("xsel" "--clipboard" "--output"))
          ) ;check
        ) ;let

        ;; 没有图形剪贴板环境时不执行外部命令。
        (unsetenv "DISPLAY")
        (check (native-clipboard-text (lambda (command) "不应读取")) => #f)
      ) ;lambda
      (lambda ()
        (if saved-wayland
          (putenv "WAYLAND_DISPLAY" saved-wayland)
          (unsetenv "WAYLAND_DISPLAY")
        ) ;if
        (if saved-display (putenv "DISPLAY" saved-display) (unsetenv "DISPLAY"))
      ) ;lambda
    ) ;dynamic-wind
  ) ;let
) ;when

;; 默认读取器直接调用系统命令，测试只读取，不修改剪贴板内容。
(let ((text (native-clipboard-text)))
  (check-true (or (not text) (string? text)))
) ;let

(check-report)
