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
        (liii pi code modes interactive selection))

(check-set-mode! 'report-failed)

;; 辅助：构造带虚拟终端的 app
(define (%make-test-app)
  (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)
    :handler (lambda (a m) #t)
  ) ;make-pi-app
) ;define

;; 端到端自动化测试：
;; 向虚拟终端注入 SGR 鼠标序列按下 \x1b;[<0;5;5M、拖拽 \x1b;[<32;20;8M、
;; 释放 \x1b;[<0;20;8m，再注入 \x1b;[99;6u (Ctrl+Shift+C)，
;; 验证捕获到的 OSC 52 base64 解码文本与预设屏幕行文本严格相符。
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t))))
  ;; 填充多行对话消息使第 4 ~ 7 行包含稳定可预期的正文内容
  (pi-app-add-user-message! app "line 0 of chat content")
  (pi-app-add-user-message! app "line 1 of chat content")
  (pi-app-add-user-message! app "line 2 of chat content")
  (pi-app-add-user-message! app "line 3 of chat content")
  (pi-app-add-user-message! app "line 4 of chat content")
  (pi-app-render! app)

  ;; 获取当前帧内容并根据选区范围 (4 . 4) 到 (19 . 7) 提取预期纯文本
  (let-values (((frame _) (pi-app-frame-lines app)))
    (let* ((sel-range (make-selection-range '(4 . 4) '(19 . 7) #t))
           (expected-text (selection-extract-text frame sel-range)))
      (check-true (> (string-length expected-text) 0))

      ;; 依次注入 SGR 序列与快捷键
      ;; 按下：\x1b;[<0;5;5M (1-based (5, 5) -> 0-based (4, 4))
      (virtual-terminal-push-key! vt (parse-mouse-event "\x1b;[<0;5;5M"))
      ;; 拖拽：\x1b;[<32;20;8M (1-based (20, 8) -> 0-based (19, 7))
      (virtual-terminal-push-key! vt (parse-mouse-event "\x1b;[<32;20;8M"))
      ;; 释放：\x1b;[<0;20;8m
      (virtual-terminal-push-key! vt (parse-mouse-event "\x1b;[<0;20;8m"))
      ;; 复制快捷键：\x1b;[99;6u (Ctrl+Shift+C)
      (virtual-terminal-push-key! vt (parse-key "\x1b;[99;6u"))
      ;; 退出
      (virtual-terminal-push-key! vt 'ctrl-d)

      ;; 启动主循环消费全部事件
      (check (pi-app-run app) => #t)

      ;; 从标准输出流中检索截获的 OSC 52 序列
      (let* ((raw (virtual-terminal-raw vt))
             (osc-item (find (lambda (s) (and (string? s) (string-contains s "\x1b;]52;c;"))) raw)))
        (check-true (string? osc-item))
        ;; 提取 base64 payload 并解码
        (let* ((prefix "\x1b;]52;c;")
               (body-start (string-length prefix))
               (body-end (- (string-length osc-item) 1))
               (b64 (substring osc-item body-start body-end))
               (decoded (string-base64-decode b64)))
          ;; 验证解码文本与屏幕选中文本严格相符
          (check decoded => expected-text))))))

(check-report)
