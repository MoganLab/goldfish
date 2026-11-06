;; pi-app-run-alt-c-copy
;; 验证 macOS 终端把 Command+C 作为 Meta 上报（ESC c 解析为 'alt-c）时，
;; 交互主循环能把鼠标选区文本复制到剪贴板。
;;
;; 语法
;; ----
;; (pi-app-run-alt-c-copy ...)
;;
;; 参数
;; ----
;; 详见语法说明。
;;
;; 返回值
;; ----
;; any
;; 视具体调用结果而定。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii base64)
  (liii tui)
  (liii tui mouse)
  (liii tui keys)
  (liii pi code modes interactive app)
  (liii pi code modes interactive run)
  (liii pi code modes interactive state)
  (liii pi code modes interactive selection)
) ;import

(check-set-mode! 'report-failed)

;; 端到端测试：
;; 注入 SGR 鼠标按下 \x1b;[<0;5;5M、拖拽 \x1b;[<32;20;8M、释放 \x1b;[<0;20;8m
;; 建立选区，再注入 macOS Command+C 对应的 ESC c 序列（解析为 'alt-c），
;; 验证捕获到的 OSC 52 base64 解码文本与屏幕选区文本严格相符。
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  (pi-app-add-user-message! app "line 0 of chat content")
  (pi-app-add-user-message! app "line 1 of chat content")
  (pi-app-add-user-message! app "line 2 of chat content")
  (pi-app-add-user-message! app "line 3 of chat content")
  (pi-app-add-user-message! app "line 4 of chat content")
  (pi-app-render! app)

  (let-values (((frame _) (pi-app-frame-lines app)))
    (let* ((sel-range (make-selection-range '(4 . 4) '(19 . 7) #t))
           (expected-text (selection-extract-text frame sel-range))
          ) ;
      (check-true (> (string-length expected-text) 0))

      ;; 按下 / 拖拽 / 释放建立选区
      (virtual-terminal-push-key! vt (parse-mouse-event "\x1b;[<0;5;5M"))
      (virtual-terminal-push-key! vt (parse-mouse-event "\x1b;[<32;20;8M"))
      (virtual-terminal-push-key! vt (parse-mouse-event "\x1b;[<0;20;8m"))
      ;; macOS Command+C：终端上报 ESC c，解析为 alt-c
      ;; 注意：\x 十六进制转义是贪婪的，必须用分号终止，否则 \x1bc 被读成 U+1BC
      (check (parse-key "\x1b;c") => 'alt-c)
      (virtual-terminal-push-key! vt (parse-key "\x1b;c"))
      ;; 退出
      (virtual-terminal-push-key! vt 'ctrl-d)

      (check (pi-app-run app) => #t)

      (let* ((raw (virtual-terminal-raw vt))
             (osc-item
               (find (lambda (s) (and (string? s) (string-contains s "\x1b;]52;c;"))) raw)
             ) ;osc-item
            ) ;
        (check-true (string? osc-item))
        (let* ((prefix "\x1b;]52;c;")
               (body-start (string-length prefix))
               (body-end (- (string-length osc-item) 1))
               (b64 (substring osc-item body-start body-end))
               (decoded (string-base64-decode b64))
              ) ;
          (check decoded => expected-text)
        ) ;let*
      ) ;let*

      ;; 验证渲染了 Copied! 浮层
      (let* ((raw (virtual-terminal-raw vt))
             (copied-item
               (find (lambda (s) (and (string? s) (string-contains s "Copied!"))) raw)
             ) ;copied-item
            ) ;
        (check-true (string? copied-item))
      ) ;let*
    ) ;let*
  ) ;let-values
) ;let*

;; copy-on-select：对齐上游 pi 的 copyOnSelect（默认开启）——
;; macOS 终端拦截 Command 键，Command+C 不会上报给程序，
;; 主复制路径是鼠标释放时自动复制选区：按下/拖拽/释放后无需任何按键，
;; 就应看到 OSC 52 复制序列与 Copied! 浮层。
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  (pi-app-add-user-message! app "line 0 of chat content")
  (pi-app-add-user-message! app "line 1 of chat content")
  (pi-app-add-user-message! app "line 2 of chat content")
  (pi-app-render! app)
  (let-values (((frame _) (pi-app-frame-lines app)))
    (let* ((sel-range (make-selection-range '(4 . 4) '(19 . 5) #t))
           (expected-text (selection-extract-text frame sel-range))
          ) ;
      (check-true (> (string-length expected-text) 0))
      ;; 只有鼠标按下/拖拽/释放，不注入任何复制快捷键
      (virtual-terminal-push-key! vt (parse-mouse-event "\x1b;[<0;5;5M"))
      (virtual-terminal-push-key! vt (parse-mouse-event "\x1b;[<32;20;6M"))
      (virtual-terminal-push-key! vt (parse-mouse-event "\x1b;[<0;20;6m"))
      (virtual-terminal-push-key! vt 'ctrl-d)
      (check (pi-app-run app) => #t)
      (let* ((raw (virtual-terminal-raw vt))
             (osc-item
               (find (lambda (s) (and (string? s) (string-contains s "\x1b;]52;c;"))) raw)
             ) ;osc-item
            ) ;
        (check-true (string? osc-item))
        (let ((b64 (substring osc-item (string-length "\x1b;]52;c;") (- (string-length osc-item) 1))))
          (check (string-base64-decode b64) => expected-text)
        ) ;let
        (check-true (string? (find (lambda (s) (and (string? s) (string-contains s "Copied!"))) raw)))
      ) ;let*
    ) ;let*
  ) ;let-values
) ;let*

;; 无选区时按 alt-c：不触发复制（无 OSC 52）、不影响退出流程
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  (virtual-terminal-push-key! vt 'alt-c)
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app) => #t)
  (let ((raw (virtual-terminal-raw vt)))
    (check-false (find (lambda (s) (and (string? s) (string-contains s "\x1b;]52;c;"))) raw))
  ) ;let
) ;let*

(check-report)
