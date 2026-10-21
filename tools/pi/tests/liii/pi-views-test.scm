(import (scheme base) (liii check) (liii string) (liii tui) (liii pi tui views))

(check-set-mode! 'report-failed)

;; -----------------------------------------------------------------------------
;; 测试项 1: 用户消息卡片 (make-user-card) - 具有深蓝背景与整行补齐
;; -----------------------------------------------------------------------------
(let* ((card (make-user-card "ls -la")) (w 40) (lines (component-render card w)))
  (check (component? card) => #t)
  (check (pair? lines) => #t)
  ;; 验证渲染输出包含深蓝背景 ANSI 转义码 (48;2;38;44;64)
  (check (string-contains? (car lines) "48;2;38;44;64") => #t)
  ;; 验证包含用户输入文本
  (check (string-contains? (car lines) "ls -la") => #t)
  ;; 验证末尾有 ANSI 重置码
  (check (string-contains? (car lines) "\x1b;[0m") => #t)
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 2: 工具调用卡片 (make-tool-card) - 成功状态为纯正深绿背景
;; -----------------------------------------------------------------------------
(let* ((tool-card (make-tool-card "bash" "npm test" 'success "Tests passed"))
       (w 50)
       (lines (component-render tool-card w))
      ) ;
  (check (component? tool-card) => #t)
  (check (>= (length lines) 1) => #t)
  ;; 验证成功状态具有绿色背景 ANSI 转义码 (48;2;35;55;35)
  (check (string-contains? (car lines) "48;2;35;55;35") => #t)
  ;; 验证输出包含工具名称与成功标识
  (check (string-contains? (car lines) "$ npm test") => #t)
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 3: 工具调用卡片 (make-tool-card) - 运行中 (pending) 与失败 (error) 状态
;; -----------------------------------------------------------------------------
(let* ((pending-card (make-tool-card "read" "file.scm" 'pending))
       (w 50)
       (lines (component-render pending-card w))
      ) ;
  ;; 验证等待状态具有暗灰背景 ANSI 转义码 (48;2;40;42;50)
  (check (string-contains? (car lines) "48;2;40;42;50") => #t)
  (check (string-contains? (car lines) "read") => #t)
) ;let*

(let* ((error-card (make-tool-card "write" "out.txt" 'error "Permission denied"))
       (w 50)
       (lines (component-render error-card w))
      ) ;
  ;; 验证出错状态具有深红背景 ANSI 转义码 (48;2;60;35;35)
  (check (string-contains? (car lines) "48;2;60;35;35") => #t)
  (check (string-contains? (car lines) "write") => #t)
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 4: 助手消息卡片 (make-assistant-card) - 透明无背景底色
;; -----------------------------------------------------------------------------
(let* ((assistant-card (make-assistant-card "这是模型的回答文本。"))
       (w 40)
       (lines (component-render assistant-card w))
      ) ;
  (check (component? assistant-card) => #t)
  (check (pair? lines) => #t)
  ;; 验证助手回复不含自定义背景色转义码 (48;2;)
  (check (string-contains? (car lines) "\x1b;[48;2;") => #f)
  (check (string-contains? (car lines) "这是模型的回答文本。") => #t)
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 5: 等待队列提示卡片 (make-queued-card)
;; -----------------------------------------------------------------------------
(let* ((queued-card (make-queued-card "等待执行的后续指令"))
       (w 40)
       (lines (component-render queued-card w))
      ) ;
  (check (component? queued-card) => #t)
  (check (string-contains? (car lines) "Queued") => #t)
  (check (string-contains? (car lines) "等待执行的后续指令") => #t)
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 6: 重试提示卡片 (make-retry-card)
;; -----------------------------------------------------------------------------
(let* ((retry-card (make-retry-card "[pi retry] HTTP 429, attempt 1/12, waiting 2.0s"))
       (w 60)
       (lines (component-render retry-card w)))
  (check (component? retry-card) => #t)
  (check (pair? lines) => #t)
  ;; 验证包含 ↻ 符号与重试描述
  (check (string-contains? (car lines) "↻") => #t)
  (check (string-contains? (car lines) "HTTP 429") => #t)
  ;; 验证带背景色码填充整行宽度
  (check (string-contains? (car lines) "\x1b;[48;2;") => #t))

(check-report)
