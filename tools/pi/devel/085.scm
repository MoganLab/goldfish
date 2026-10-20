(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (liii check)
  (liii error)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii os)
  (liii pi bash)
  (liii tui text-width)
  (liii tui terminal)
  (liii pi modes tui)
  (liii pi modes tui state)
  (liii pi modes tui loop)
  (liii pi modes tui message)
  (liii pi modes tui viewport)
) ;import

(check-set-mode! 'report-failed)

;; [085] 验收脚本：消息历史视口与自然折行渲染

(define (%run-ok? tag cmd timeout)
  (let ((tmp (string-append "tmp-085-" tag ".txt")))
    (let ((ret (run (list (pi-find-bash-path) "-c" (string-append cmd " > " tmp " 2>&1"))
                 :stdin   'null
                 :timeout timeout
               ) ;run
          ) ;ret
         ) ;
      (when (file-exists? tmp)
        (delete-file tmp)
      ) ;when
      ret
    ) ;let
  ) ;let
) ;define

(define (%run-with-output tag cmd timeout)
  (let ((tmp (string-append "tmp-085-" tag ".txt")))
    (let ((ret (run (list (pi-find-bash-path) "-c" (string-append cmd " > " tmp " 2>&1"))
                 :stdin   'null
                 :timeout timeout
               ) ;run
          ) ;ret
         ) ;
      (let ((out (if (file-exists? tmp) (path-read-text (path tmp)) "")))
        (when (file-exists? tmp)
          (delete-file tmp)
        ) ;when
        (cons ret out)
      ) ;let
    ) ;let
  ) ;let
) ;define

(display "=== 验收点 1: 门禁检查（构建 / fmt / check / doc） ===\n")
(check (%run-ok? "build" "xmake b goldfish" 300) => 0)
(check (%run-ok? "fmt-chk" "bin/gf fmt --dry-run" 120) => 0)
(check (%run-ok? "check" "bin/gf check" 120) => 0)
(check (%run-ok? "doc" "bin/gf doc --build-json" 120) => 0)

(display "=== 验收点 2: 任务 085 代码与测试文件行数约束 (<= 300 行) ===\n"
) ;display
(for-each
  (lambda (f)
    (let ((lines (length (string-split (path-read-text (path f)) "\n"))))
      (check-true (<= lines 300))
    ) ;let
  ) ;lambda
  '("tools/pi/liii/pi/modes/tui/message.scm"
    "tools/pi/liii/pi/modes/tui/viewport.scm"
    "tools/pi/liii/pi/modes/tui/loop.scm"
    "tools/pi/liii/pi/modes/tui/loop/dispatch.scm"
    "tools/pi/liii/pi/modes/tui.scm"
    "tools/pi/tests/liii/pi/modes/tui/message-test.scm"
    "tools/pi/tests/liii/pi/modes/tui/viewport-test.scm"
    "tools/pi/tests/liii/pi/modes/tui/loop-test.scm" "tools/pi/devel/085.md"
    "tools/pi/devel/085.scm")
) ;for-each

(display "=== 验收点 3: 消息条目数据结构与角色前缀格式化 ===\n"
) ;display
(let ((u (make-pi-tui-message 'user "User prompt"))
      (a (make-pi-tui-message 'assistant "Assistant reply"))
      (s (make-pi-tui-message 'system "System notification"))
     ) ;
  (check-true (pi-tui-message? u))
  (check (message-role u) => 'user)
  (check (message-content u) => "User prompt")

  (check-true (pi-tui-message? a))
  (check (message-role a) => 'assistant)
  (check (message-content a) => "Assistant reply")

  (check-true (pi-tui-message? s))
  (check (message-role s) => 'system)
  (check (message-content s) => "System notification")

  ;; 前缀着色
  (check-true (string-contains (message-role-prefix 'user) ">"))
  (check-true (string-contains (message-role-prefix 'assistant) "●"))
  (check-true (string-contains (message-role-prefix 'system) "■"))

  ;; 规范化
  (let ((norm (normalize-pi-tui-message (cons 'user "test msg"))))
    (check-true (pi-tui-message? norm))
    (check (message-role norm) => 'user)
    (check (message-content norm) => "test msg")
  ) ;let

  ;; 异常校验
  (check-catch 'type-error (make-pi-tui-message 'other "foo"))
  (check-catch 'type-error (make-pi-tui-message 'user 123))
  (check-catch 'type-error (normalize-pi-tui-message 'invalid))
) ;let

(display "=== 验收点 4: 自然折行与多字节中英文字符宽度感知 ===\n"
) ;display
;; 1. 不超宽文本
(let* ((msg (make-pi-tui-message 'user "short text"))
       (lines (render-message-lines msg 30))
      ) ;
  (check (length lines) => 1)
  (check-true (<= (display-width (car lines)) 30))
) ;let*

;; 2. 纯英文超宽折行
(let* ((msg (make-pi-tui-message 'assistant
              "The quick brown fox jumps over the lazy dog repeatedly"
            ) ;make-pi-tui-message
       ) ;msg
       (lines (render-message-lines msg 20))
      ) ;
  (check-true (> (length lines) 1))
  (for-each
    (lambda (line) (check-true (<= (display-width line) 20)))
    lines
  ) ;for-each
) ;let*

;; 3. 中英文混排与全角字符宽度处理 (每个汉字占 2 列)
(let* ((chinese-text "测试中文字符宽度折行能力，确保不撕裂、不截断！"
       ) ;chinese-text
       (msg (make-pi-tui-message 'user chinese-text))
       (lines (render-message-lines msg 24))
      ) ;
  (check-true (> (length lines) 1))
  (for-each
    (lambda (line) (check-true (<= (display-width line) 24)))
    lines
  ) ;for-each
) ;let*

;; 4. 原生换行符多行文本
(let* ((msg (make-pi-tui-message 'system "alpha\nbeta\ngamma"))
       (lines (render-message-lines msg 40))
      ) ;
  (check (length lines) => 3)
) ;let*

(display "=== 验收点 5: 视口自动贴底跟随与窗口范围计算 ===\n")
;; 1. 总行数 <= 视口高度
(let-values (((start end) (compute-viewport-window 8 20 0)))
  (check start => 0)
  (check end => 8)
) ;let-values

;; 2. 总行数 > 视口高度，贴底跟随 (offset = 0)
(let-values (((start end) (compute-viewport-window 35 15 0)))
  (check start => 20)
  (check end => 35)
) ;let-values

;; 3. 视口渲染渲染结果验证
(let* ((msgs (list (make-pi-tui-message 'user "Line 1")
               (make-pi-tui-message 'assistant "Line 2\nLine 3")
               (make-pi-tui-message 'user "Line 4")
             ) ;list
       ) ;msgs
       (view (render-messages-viewport msgs 40 10 0))
      ) ;
  (check (length view) => 4)
) ;let*

(display "=== 验收点 6: 历史滚动浏览与滚动指示器 ===\n")
;; 1. 向上滚动偏移
(let-values (((start end) (compute-viewport-window 30 10 5)))
  (check start => 15)
  (check end => 25)
) ;let-values

;; 2. 滚到顶
(let-values (((start end) (compute-viewport-window 30 10 20)))
  (check start => 0)
  (check end => 10)
) ;let-values

;; 3. 偏移超限 clamp
(let-values (((start end) (compute-viewport-window 30 10 999)))
  (check start => 0)
  (check end => 10)
) ;let-values

;; 4. 最大偏移
(check (compute-max-scroll-offset 30 10) => 20)
(check (compute-max-scroll-offset 5 10) => 0)

;; 5. 滚动指示器
(check (viewport-scroll-indicator 30 10 0) => "")
(check (viewport-scroll-indicator 30 10 20) => "[Top]")
(check (viewport-scroll-indicator 30 10 10) => "[50%]")

(display "=== 验收点 7: 状态机滚动按键分发（PageUp / PageDown） ===\n"
) ;display
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (st (make-pi-tui-state :scroll-offset 0 :status 'idle))
      ) ;
  ;; 初始 offset 0
  (check (state-scroll-offset st) => 0)
  ;; 推入 page-up 两次，page-down 一次，最后 escape 退出
  (virtual-terminal-push-key! vt 'page-up)
  (virtual-terminal-push-key! vt 'page-up)
  (virtual-terminal-push-key! vt 'page-down)
  (virtual-terminal-push-key! vt 'escape)

  (let ((final-st (run-tui-loop vt :state st :return-state #t)))
    (check (state-status final-st) => 'exit)
    ;; 两次 page-up (+5 +5 = 10)，一次 page-down (-5 = 5)
    (check (state-scroll-offset final-st) => 5)
  ) ;let
) ;let*

(display "=== 验收点 8: 门面导出接口完整性 ===\n")
(check-true (procedure? make-pi-tui-message))
(check-true (procedure? pi-tui-message?))
(check-true (procedure? message-role))
(check-true (procedure? message-content))
(check-true (procedure? normalize-pi-tui-message))
(check-true (procedure? message-role-prefix))
(check-true (procedure? message-role-indent))
(check-true (procedure? render-message-lines))
(check-true (procedure? compute-max-scroll-offset))
(check-true (procedure? compute-viewport-window))
(check-true (procedure? viewport-scroll-indicator))
(check-true (procedure? render-messages-viewport))

(display "=== 验收点 9: 真实冒烟（非 TTY 下启动 bin/gf pi 打印帮助并以 0 退出） ===\n"
) ;display
(let ((res (%run-with-output "smoke" "bin/gf pi" 30)))
  (check (car res) => 0)
  (check-true (string-contains (cdr res) "gf pi [options]"))
) ;let

(display "=== 验收点 10: 全量测试套件执行并硬断言总计数与通过数（Total: 326, Passed: 326） ===\n"
) ;display
(let ((res (%run-with-output "all-tests" "bin/gf test tools/pi/tests/liii/" 300)))
  (check (car res) => 0)
  (let ((out (cdr res)))
    (check-true (string-contains out "Total:  326"))
    (check-true (string-contains out "Passed: 326"))
  ) ;let
) ;let

(check-report)
