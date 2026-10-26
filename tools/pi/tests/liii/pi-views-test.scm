(import (scheme base) (liii check) (liii string) (liii tui) (liii pi code modes interactive views))

(check-set-mode! 'report-failed)

;; -----------------------------------------------------------------------------
;; 测试项 1: 用户消息卡片 (make-user-card) - 对齐 pi UserMessageComponent：
;; 背景为 userMessageBg #213b49、前景 text #dee0e1、上下各一行背景 padding 空行、
;; 正文 1 空格缩进且无 " > " 前缀
;; -----------------------------------------------------------------------------
(let* ((card (make-user-card "ls -la")) (w 40) (lines (component-render card w)))
  (check (component? card) => #t)
  (check (pair? lines) => #t)
  ;; 共 4 行：背景空行 + 内容行 + 背景空行 + 无背景 spacer
  (check (length lines) => 4)
  ;; 首行为带 userMessageBg (48;2;33;59;73) 的空 padding 行，不含正文
  (check (string-contains? (car lines) "48;2;33;59;73") => #t)
  (check-false (string-contains? (car lines) "ls -la"))
  ;; 内容行：1 空格缩进、无 " > " 前缀，userMessageText (38;2;222;224;225)
  (check (string-contains? (cadr lines) "48;2;33;59;73") => #t)
  (check (string-contains? (cadr lines) "38;2;222;224;225") => #t)
  (check (string-contains? (cadr lines) " ls -la") => #t)
  (check-false (string-contains? (cadr lines) ">"))
  ;; 末行为背景空行，之后是无背景 spacer
  (check (string-contains? (list-ref lines 2) "48;2;33;59;73") => #t)
  (check (list-ref lines 3) => "")
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 2: bash 工具卡片 - 对齐 pi Box(paddingX=1, paddingY=1)：
;; 上下各一行背景 padding 空行；命令行 toolTitle 粗体；输出 toolOutput muted；
;; 成功背景 toolSuccessBg #254131 包住命令行
;; -----------------------------------------------------------------------------
(let* ((tool-card (make-tool-card "bash" "npm test" 'success "Tests passed"))
       (w 50)
       (lines (component-render tool-card w))
      ) ;
  (check (component? tool-card) => #t)
  ;; 共 6 行：padding 空行 + 命令 + 空行 + 输出 + padding 空行 + spacer
  (check (length lines) => 6)
  ;; 首行 padding 空行带绿背景，不含命令
  (check (string-contains? (car lines) "48;2;37;65;49") => #t)
  (check-false (string-contains? (car lines) "npm test"))
  ;; 命令行：绿背景 + toolTitle 粗体（[1m 与 38;2;222;224;225 分开两段，
  ;; 且不得使用前导 0 的全重置序列——那会清掉背景色）
  (check (string-contains? (cadr lines) "48;2;37;65;49") => #t)
  (check (string-contains? (cadr lines) "$ npm test") => #t)
  (check (string-contains? (cadr lines) "\x1b;[1m\x1b;[38;2;222;224;225m") => #t)
  (check-false (string-contains? (cadr lines) "0;1;38;2"))
  ;; 输出行 muted (38;2;157;165;169)
  (check (string-contains? (list-ref lines 3) "38;2;157;165;169") => #t)
  (check-false (string-contains? (cadr lines) "38;2;157;165;169"))
  ;; 尾部 padding 空行带绿背景 + 无背景 spacer
  (check (string-contains? (list-ref lines 4) "48;2;37;65;49") => #t)
  (check (list-ref lines 5) => "")
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 2b: 非 bash 工具卡片（read pending）- 对齐 pi 专用渲染：
;; 无 ● 徽标；标题 = 粗体工具名 (toolTitle) + accent 紫路径 (38;2;167;152;215)；
;; pending 背景 toolPendingBg #34383a
;; -----------------------------------------------------------------------------
(let* ((pending-card (make-tool-card "read" "file.scm" 'pending))
       (w 50)
       (lines (component-render pending-card 50))
       (plain (string-join (map strip-ansi lines) "\n"))
      ) ;
  ;; pending 背景
  (check (string-contains? (cadr lines) "48;2;52;56;58") => #t)
  ;; 粗体工具名 + accent 路径（无前导 0 全重置，粗体用 1m）
  (check (string-contains? (cadr lines) "\x1b;[1m\x1b;[38;2;222;224;225mread") => #t)
  (check (string-contains? (cadr lines) "38;2;167;152;215m file.scm") => #t)
  ;; 无徽标
  (check-false (string-contains? plain "●"))
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 2c: write 失败卡片 - 红背景把命令行也整个包进去（状态仅由背景表达）；
;; 前景不随状态染色（无 250;210;210 红字、无 ✖ 徽标），输出 muted
;; -----------------------------------------------------------------------------
(let* ((error-card (make-tool-card "write" "out.txt" 'error "Permission denied"))
       (w 50)
       (lines (component-render error-card w))
       (joined (string-join lines ""))
      ) ;
  ;; 命令行被 toolErrorBg 红背景 (48;2;91;40;42) 包住，粗体 toolTitle 前景（无前导 0）
  (check (string-contains? (cadr lines) "48;2;91;40;42") => #t)
  (check (string-contains? (cadr lines) "\x1b;[1m\x1b;[38;2;222;224;225mwrite") => #t)
  (check (string-contains? (cadr lines) "38;2;167;152;215m out.txt") => #t)
  ;; 前景不随状态染色、无徽标
  (check-false (string-contains? joined "38;2;250;210;210"))
  (check-false (string-contains? joined "✖"))
  ;; 输出行 muted
  (check (string-contains? (list-ref lines 3) "38;2;157;165;169") => #t)
  (check (string-contains? (list-ref lines 3) "Permission denied") => #t)
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 2d: read 成功卡片 - 绿背景包住命令（无 ✔ 徽标、无绿字染色）
;; -----------------------------------------------------------------------------
(let* ((success-card (make-tool-card "read" "a.scm" 'success "(content)"))
       (w 50)
       (lines (component-render success-card w))
       (joined (string-join lines ""))
      ) ;
  (check (string-contains? (cadr lines) "48;2;37;65;49") => #t)
  (check (string-contains? (cadr lines) "\x1b;[1m\x1b;[38;2;222;224;225mread") => #t)
  (check-false (string-contains? joined "38;2;220;245;220"))
  (check-false (string-contains? joined "✔"))
  ;; 输出 muted（无中途全重置，背景覆盖到行尾 padding）
  (check (string-contains? (list-ref lines 3) "38;2;157;165;169") => #t)
  (check-false (string-contains? (list-ref lines 3) "\x1b;[0m "))
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 2e: bash 工具卡片输出中的空行 - 背景必须铺满整行宽度
;; （修复：中途 \x1b[0m 全重置会清掉背景，导致空行只剩最左 1 空格看背景）
;; -----------------------------------------------------------------------------
(let* ((blank-card (make-tool-card "bash" "printf a" 'success "a\n\nb"))
       (w 50)
       (lines (component-render blank-card w))
      ) ;
  ;; 结构：padding 空行 + 标题 + 空行 + a + 空行 + b + padding 空行 + spacer = 8 行
  (check (length lines) => 8)
  ;; 输出内空行（索引 4）：背景铺满整行，不存在“重置后再补 padding”的断裂
  (check (string-contains? (list-ref lines 4) "48;2;37;65;49") => #t)
  (check-false (string-contains? (list-ref lines 4) "\x1b;[0m "))
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 3: 助手消息卡片 (make-assistant-card) - 透明无背景底色
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
;; 测试项 4: 等待队列提示卡片 (make-queued-card) - dim 前景 #7e888e
;; -----------------------------------------------------------------------------
(let* ((queued-card (make-queued-card "等待执行的后续指令"))
       (w 40)
       (lines (component-render queued-card w))
      ) ;
  (check (component? queued-card) => #t)
  (check (string-contains? (car lines) "Queued") => #t)
  (check (string-contains? (car lines) "等待执行的后续指令") => #t)
  (check (string-contains? (car lines) "38;2;126;136;142") => #t)
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 5: 重试提示卡片 (make-retry-card) - pending 背景整行填充
;; -----------------------------------------------------------------------------
(let* ((retry-card (make-retry-card "[pi retry] HTTP 429, attempt 1/12, waiting 2.0s"))
       (w 60)
       (lines (component-render retry-card w)))
  (check (component? retry-card) => #t)
  (check (pair? lines) => #t)
  (check (string-contains? (car lines) "↻") => #t)
  (check (string-contains? (car lines) "HTTP 429") => #t)
  ;; 验证带背景色码填充整行宽度
  (check (string-contains? (car lines) "\x1b;[48;2;") => #t))

(check-report)
