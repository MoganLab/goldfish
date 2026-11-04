;; pi-app-run-copy
;; 验证在交互主循环中通过鼠标选区与快捷键复制文本到剪贴板流程。
;;
;; 语法
;; ----
;; (pi-app-run-copy ...)
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

;; 辅助：构造带虚拟终端的 app

(define (%make-test-app)
  (make-pi-app
    :terminal (make-virtual-terminal :width 80 :height 24)
    :handler  (lambda (a m) #t)
  ) ;make-pi-app
) ;define

;; 端到端自动化测试：
;; 向虚拟终端注入 SGR 鼠标序列按下 \x1b;[<0;5;5M、拖拽 \x1b;[<32;20;8M、
;; 释放 \x1b;[<0;20;8m，再注入 \x1b;[99;6u (Ctrl+Shift+C)，
;; 验证捕获到的 OSC 52 base64 解码文本与预设屏幕行文本严格相符。
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
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
           (expected-text (selection-extract-text frame sel-range))
          ) ;
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
             (osc-item
               (find (lambda (s) (and (string? s) (string-contains s "\x1b;]52;c;"))) raw)
             ) ;osc-item
            ) ;
        (check-true (string? osc-item))
        ;; 提取 base64 payload 并解码
        (let* ((prefix "\x1b;]52;c;")
               (body-start (string-length prefix))
               (body-end (- (string-length osc-item) 1))
               (b64 (substring osc-item body-start body-end))
               (decoded (string-base64-decode b64))
              ) ;
          ;; 验证解码文本与屏幕选中文本严格相符
          (check decoded => expected-text)
        ) ;let*
      ) ;let*
    ) ;let*
  ) ;let-values
) ;let*

;; 测试 2：Linux 传统终端下 Ctrl+Shift+C 被识别为 'ctrl-c
;; 验证当选区激活时，注入 'ctrl-c 会触发复制选区，且不清除当前输入框内容
(let* ((submitted #f)
       (vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app
              :terminal vt
              :handler (lambda (a m) (set! submitted m) #t)
            ) ;make-pi-app
       ) ;app
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

      ;; 激活选区
      (pi-app-selection-set! app sel-range)

      ;; 输入框预设文本
      (editor-set-text! (pi-app-editor app) "keep this input text")

      ;; 注入 'ctrl-c (模拟 Linux 终端按 Ctrl+Shift+C 发出的 0x03 键码)
      (virtual-terminal-push-key! vt 'ctrl-c)
      ;; 注入 'enter 提交输入框内容（提交后输入框清空）
      (virtual-terminal-push-key! vt 'enter)
      ;; 退出
      (virtual-terminal-push-key! vt 'ctrl-d)

      ;; 运行主循环
      (check (pi-app-run app) => #t)

      ;; 验证输入框文字未被 ctrl-c 清空，成功被 enter 提交
      (check submitted => "keep this input text")

      ;; 验证截获了包含 expected-text 的 OSC 52 复制指令
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
    ) ;let*
  ) ;let-values
) ;let*

;; 测试 3：无激活选区时，'ctrl-c 正常清空输入框
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  (editor-set-text! (pi-app-editor app) "text to clear")
  ;; 注入 'ctrl-c 清空输入框，再注入 'ctrl-d（输入框为空时触发退出）
  (virtual-terminal-push-key! vt 'ctrl-c)
  (virtual-terminal-push-key! vt 'ctrl-d)

  (check (pi-app-run app) => #t)
  (check (editor-text (pi-app-editor app)) => "")
) ;let*

(check-report)
