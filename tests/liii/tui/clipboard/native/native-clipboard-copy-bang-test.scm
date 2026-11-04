;; native-clipboard-copy!
;; 调用操作系统底层命令（clip / pbcopy / wl-copy / xclip / xsel）将文本写入系统剪贴板。
;; Linux 支持注入命令执行器，验证文本通过标准输入发送给剪贴板命令。
;;
;; 语法
;; ----
;; (native-clipboard-copy! text [runner])
;;
;; 参数
;; ----
;; text : string
;; 欲复制到剪贴板的文本字符串。
;;
;; runner : procedure（可选）
;; 接收命令参数列表和文本，返回 #t 表示写入成功，#f 表示失败。
;;
;; 返回值
;; ----
;; boolean
;; 成功返回 #t，失败或不支持返回 #f。

(import (scheme base)
  (scheme process-context)
  (liii check)
  (liii os)
  (liii tui clipboard native)
) ;import

(check-set-mode! 'report-failed)

;; 测试执行器记录每次调用的 (command . text)，由 results 依次决定成败。
;; 返回执行器与记录读取器，二者通过词法闭包共享调用记录。

(define (%make-recording-runner results)
  (let ((calls '()))
    (values
      (lambda (command text)
        (set! calls (cons (cons command text) calls))
        (let ((result (car results)))
          (set! results (cdr results))
          result
        ) ;let
      ) ;lambda
      (lambda () (reverse calls))
    ) ;values
  ) ;let
) ;define

(when (os-linux?)
  (let ((saved-wayland
          (let ((value (getenv "WAYLAND_DISPLAY")))
            (and value (not (string=? value "")) value)
          ) ;let
        ) ;saved-wayland
        (saved-display (getenv "DISPLAY"))
       ) ;
    (dynamic-wind (lambda () #t)
      (lambda ()
        ;; 1. Wayland 优先调用 wl-copy，成功后不调用 X11 命令。
        (putenv "WAYLAND_DISPLAY" "test-wayland")
        (putenv "DISPLAY" "test-x11")
        (let-values (((runner recorded-calls) (%make-recording-runner '(#t))))
          (check-true (native-clipboard-copy! "来自 Wayland" runner))
          (check (recorded-calls) => '((("wl-copy") . "来自 Wayland")))
        ) ;let-values

        ;; 2. 纯 X11 环境优先调用 xclip，成功后不调用 xsel。
        (unsetenv "WAYLAND_DISPLAY")
        (let-values (((runner recorded-calls) (%make-recording-runner '(#t))))
          (check-true (native-clipboard-copy! "来自 X11" runner))
          (check (recorded-calls)
            =>
            '((("xclip" "-selection" "clipboard") . "来自 X11"))
          ) ;check
        ) ;let-values

        ;; 3. wl-copy 失败后回退到 xclip。
        (putenv "WAYLAND_DISPLAY" "test-wayland")
        (let-values (((runner recorded-calls) (%make-recording-runner '(#f #t))))
          (check-true (native-clipboard-copy! "回退到 X11" runner))
          (check (recorded-calls)
            =>
            '((("wl-copy") . "回退到 X11")
              (("xclip" "-selection" "clipboard") . "回退到 X11"))
          ) ;check
        ) ;let-values

        ;; 4. xclip 失败后回退到 xsel。
        (unsetenv "WAYLAND_DISPLAY")
        (let-values (((runner recorded-calls) (%make-recording-runner '(#f #t))))
          (check-true (native-clipboard-copy! "回退到 xsel" runner))
          (check (recorded-calls)
            =>
            '((("xclip" "-selection" "clipboard") . "回退到 xsel")
              (("xsel" "--clipboard" "--input") . "回退到 xsel"))
          ) ;check
        ) ;let-values

        ;; 5. 所有命令均失败时返回 #f，不抛出异常。
        (let-values (((runner recorded-calls) (%make-recording-runner '(#f #f))))
          (check-false (native-clipboard-copy! "复制失败" runner))
          (check (length (recorded-calls)) => 2)
        ) ;let-values

        ;; 6. 中文、多行文本与空字符串均原样传给命令标准输入。
        (let ((text "第一行\n第二行 🎉\n"))
          (let-values (((runner recorded-calls) (%make-recording-runner '(#t))))
            (check-true (native-clipboard-copy! text runner))
            (check (recorded-calls)
              =>
              (list (cons '("xclip" "-selection" "clipboard") text))
            ) ;check
          ) ;let-values
        ) ;let
        (let-values (((runner recorded-calls) (%make-recording-runner '(#t))))
          (check-true (native-clipboard-copy! "" runner))
          (check (recorded-calls) => '((("xclip" "-selection" "clipboard") . "")))
        ) ;let-values

        ;; 7. 没有图形剪贴板环境时不执行外部命令。
        (unsetenv "DISPLAY")
        (let ((calls '()))
          (check-false
            (native-clipboard-copy! "无图形环境"
              (lambda (command text) (set! calls (cons command calls)) #t)
            ) ;native-clipboard-copy!
          ) ;check-false
          (check calls => '())
        ) ;let
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

;; 8. 默认执行器调用真实系统命令，返回值始终为布尔值。
(check-true (boolean? (native-clipboard-copy! "test native copy")))
(check-true (boolean? (native-clipboard-copy! "")))

;; 9. Linux 可用原生复制命令时，验证标准输入写入与系统读取的往返结果。
;; 无图形环境（如 CI）时复制返回 #f，跳过往返断言。
(when (os-linux?)
  (let ((text "Goldfish Linux 复制\n第二行"))
    (let ((copied? (native-clipboard-copy! text)))
      (check-true (boolean? copied?))
      (when copied?
        (check (native-clipboard-text) => text)
      ) ;when
    ) ;let
  ) ;let
) ;when

(check-report)
