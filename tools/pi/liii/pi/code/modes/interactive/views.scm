;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code modes interactive views)
  (export make-user-card make-tool-card make-assistant-card make-queued-card
    make-retry-card apply-card-bg format-tool-call format-tool-result-lines
    tool-card? tool-card-component? tool-card-expanded? tool-card-set-expanded!
    tool-card-set-output! tool-card-set-complete!
  ) ;export
  (import (scheme base)
    (scheme time)
    (liii string)
    (liii tui component)
    (liii tui text-width)
    (liii pi code modes interactive renderers)
  ) ;import
  (begin

    ;; ANSI 颜色常量（严格对应 pi dark 主题 dark.json 的 okhsl 精确换算值）
    ;; userMessageBg: 深蓝背景 (#213b49 = okhsl(233 41% 24%))
    (define %bg-user "\x1b;[48;2;33;59;73m")
    ;; userMessageText = text (#dee0e1 = okhsl(234 3% 89%))
    (define %fg-user "\x1b;[38;2;222;224;225m")

    ;; toolPendingBg: 执行中微蓝暗灰背景 (#34383a = okhsl(229 5% 24%))
    (define %bg-pending "\x1b;[48;2;52;56;58m")
    ;; toolSuccessBg: 工具执行成功标志性深绿背景 (#254131 = okhsl(158 46% 25%))
    (define %bg-success "\x1b;[48;2;37;65;49m")
    ;; toolErrorBg: 工具执行失败警示深红背景 (#5b282a = okhsl(19 54% 25%))
    (define %bg-error "\x1b;[48;2;91;40;42m")

    ;; toolTitle: 命令标题亮色加粗前景（text，暗色主题 #dee0e1）。
    ;; 注意：不得使用前导 0 的全重置序列（如 0;1;38;2;...），
    ;; 0 会清掉行首已设置的背景色；粗体用独立的 1m 开启
    (define %fg-title "\x1b;[1m\x1b;[38;2;222;224;225m")
    ;; toolOutput: 输出/计时 muted 前景（#9da5a9）
    (define %fg-output "\x1b;[38;2;157;165;169m")
    ;; accent: 路径/模式紫色 (#a798d7 = okhsl(295 50% 67%))
    (define %fg-accent "\x1b;[38;2;167;152;215m")
    ;; warning: read 范围后缀黄色 (#cd9a22 = okhsl(83 88% 67%))
    (define %fg-warning "\x1b;[38;2;205;154;34m")

    ;; 选择性重置（对齐 pi styleTextWithAnsi 的 39/22 语义）：
    ;; 只关前景/粗体，绝不清背景，保证背景铺满整行（含尾部 padding）
    (define %fg-reset "\x1b;[39m")
    (define %bold-reset "\x1b;[22m\x1b;[39m")

    ;; 暗淡排队提示（dim，#7e888e = okhsl(229 8% 56%)）
    (define %fg-dim "\x1b;[38;2;126;136;142m")
    (define %ansi-reset "\x1b;[0m")

    ;; 行背景填充函数：计算可见宽度，补足空格并包裹在指定背景/前景 ANSI 码中。
    ;; padding 空格在背景色范围内：line 内部只允许出现选择性重置（39/22），
    ;; 不得包含 \x1b[0m 全重置（会清背景，导致空行只剩左侧有色）
    (define (apply-card-bg line width bg-code fg-code)
      (let* ((vlen (display-width line))
             (pad-needed (max 0 (- width vlen)))
             (padded (string-append line (make-string pad-needed #\space)))
            ) ;
        (string-append bg-code fg-code padded %ansi-reset)
      ) ;let*
    ) ;define

    ;; 用户提问卡片组件 (make-user-card text)
    ;; 对齐 pi UserMessageComponent：Markdown(paddingX=1, paddingY=1) + userMessageBg/userMessageText，
    ;; 上下各一行带背景的空 padding 行，正文 1 空格缩进，无前缀符号
    (define (make-user-card text)
      (make-component
        :render
        (lambda (self width)
          (if (<= width 0)
            '()
            (let* ((lines (string-split text "\n"))
                   (rows
                     (map (lambda (line) (apply-card-bg (string-append " " line) width %bg-user %fg-user))
                       lines
                     ) ;map
                   ) ;rows
                  ) ;
              (append (list (apply-card-bg "" width %bg-user %fg-user))
                rows
                (list (apply-card-bg "" width %bg-user %fg-user) "")
              ) ;append
            ) ;let*
          ) ;if
        ) ;lambda
      ) ;make-component
    ) ;define

    ;; 工具调用卡片的可变状态。pending 卡片在 start/update/end 事件间复用。
    (define-record-type <tool-card>
      (%make-tool-card tool-name args status result expanded? started-at
        ended-at duration-ms render-cache
      ) ;%make-tool-card
      tool-card?
      (tool-name %tool-card-name)
      (args %tool-card-args)
      (status %tool-card-status %tool-card-status-set!)
      (result %tool-card-result %tool-card-result-set!)
      (expanded? %tool-card-expanded? %tool-card-expanded?-set!)
      (started-at %tool-card-started-at)
      (ended-at %tool-card-ended-at %tool-card-ended-at-set!)
      (duration-ms %tool-card-duration-ms %tool-card-duration-ms-set!)
      ;; bash 输出渲染缓存：(key . lines)，key = (result width expanded?)
      (render-cache %tool-card-render-cache %tool-card-render-cache-set!)
    ) ;define-record-type

    (define (%tool-now-ms)
      (* 1000.0 (/ (current-jiffy) (jiffies-per-second)))
    ) ;define

    (define (tool-card-set-expanded! card flag)
      (%tool-card-expanded?-set! (component-data card) flag)
    ) ;define

    (define (tool-card-expanded? card)
      (%tool-card-expanded? (component-data card))
    ) ;define

    (define (tool-card-component? obj)
      (and (component? obj) (tool-card? (component-data obj)))
    ) ;define

    (define (tool-card-set-output! card result)
      (%tool-card-result-set! (component-data card) result)
    ) ;define

    (define (tool-card-set-complete! card result is-error? duration-ms)
      (let ((st (component-data card)))
        (%tool-card-result-set! st result)
        (%tool-card-status-set! st (if is-error? 'error 'success))
        (%tool-card-ended-at-set! st (%tool-now-ms))
        (%tool-card-duration-ms-set! st duration-ms)
      ) ;let
    ) ;define

    ;; 与上游每秒刷新一致：Working 动画可更频繁重绘，但时间只按秒推进
    (define (%tool-elapsed-text started)
      (let ((elapsed
              (* 1000 (quotient (inexact->exact (floor (- (%tool-now-ms) started))) 1000))
            ) ;elapsed
           ) ;
        (string-append "Elapsed " (format-tool-duration elapsed))
      ) ;let
    ) ;define

    (define (%tool-timer-text st)
      (if (not (string=? (%tool-card-name st) "bash"))
        #f
        (let ((started (%tool-card-started-at st))
              (status (%tool-card-status st))
              (duration (%tool-card-duration-ms st))
             ) ;
          (cond
           ((and (not (eq? status 'pending)) (number? duration))
            (string-append "Took " (format-tool-duration duration))
           ) ;
           ((not started) #f)
           ((eq? status 'pending) (%tool-elapsed-text started))
           (else
             (string-append "Took "
               (format-tool-duration (- (or (%tool-card-ended-at st) (%tool-now-ms)) started))
             ) ;string-append
           ) ;else
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    ;; 状态仅由背景表达（对齐 pi Box bgFn）：pending 灰蓝 / success 绿 / error 红；
    ;; 前景不随状态染色（标题 toolTitle、输出 muted）
    (define (%tool-bg status)
      (case status
       ((success) %bg-success)
       ((error) %bg-error)
       (else %bg-pending)
      ) ;case
    ) ;define

    ;; 对齐上游 VisualLinePreview：按 (result width expanded?) 缓存渲染行。
    ;; 转录中每个结果每帧都会渲染，不缓存时大输出会使 CPU 飕升
    (define (%cached-bash-result-lines st result width)
      (let ((key (list result width (%tool-card-expanded? st))))
        (let ((cache (%tool-card-render-cache st)))
          (if (and (pair? cache) (equal? (car cache) key))
            (cdr cache)
            (let ((lines (format-bash-tool-result-lines result width (%tool-card-expanded? st))))
              (%tool-card-render-cache-set! st (cons key lines))
              lines
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; Bash 使用上游专用渲染：无状态徽标，输出与计时前后各一个空行；
    ;; 命令标题用 toolTitle（亮色加粗），输出/计时用 toolOutput（muted）；
    ;; 对齐 pi Box(paddingX=1, paddingY=1)：上下各一行背景空行包住命令与输出
    (define (%render-bash-tool-card st width)
      (let* ((bg (%tool-bg (%tool-card-status st)))
             (content-width (max 1 (- width 2)))
             (title-lines (wrap-text (format-tool-call (%tool-card-name st) (%tool-card-args st))
                            content-width
                          ) ;wrap-text
             ) ;title-lines
             (result (%tool-card-result st))
             (result-lines (%cached-bash-result-lines st result content-width))
             (timer (%tool-timer-text st))
             (title-rows
               (map (lambda (line) (apply-card-bg (string-append " " line) width bg %fg-title))
                 title-lines
               ) ;map
             ) ;title-rows
             (rest-body (append (if (null? result-lines) '() (cons "" result-lines))
                          (if timer (list "" timer) '())
                        ) ;append
             ) ;rest-body
             (rest-rows
               (map (lambda (line) (apply-card-bg (string-append " " line) width bg %fg-output))
                 rest-body
               ) ;map
             ) ;rest-rows
             (pad-rows (list (apply-card-bg "" width bg %fg-output)))
            ) ;
        (append pad-rows title-rows rest-rows pad-rows (list ""))
      ) ;let*
    ) ;define

    ;; 着色分段 → ANSI 文本：bold=toolTitle 粗体 / accent / muted / warning
    ;; 对齐上游 theme.fg：每段末尾只选择性重置（粗体 22+39，其余 39），
    ;; 绝不发 \x1b[0m 全重置，否则会清掉行首 apply-card-bg 设置的卡片背景色
    (define (%style-segments segs)
      (apply string-append
        (map
          (lambda (seg)
            (let ((role (car seg)) (text (cdr seg)))
              (string-append (case role
                              ((bold) %fg-title)
                              ((accent) %fg-accent)
                              ((warning) %fg-warning)
                              (else %fg-output)
                             ) ;case
                text
                (if (eq? role 'bold) %bold-reset %fg-reset)
              ) ;string-append
            ) ;let
          ) ;lambda
          segs
        ) ;map
      ) ;apply
    ) ;define

    ;; 渲染工具卡片多行行列表的辅助函数（非 bash 工具）
    ;; 对齐 pi ToolExecutionComponent 的 Box(paddingX=1, paddingY=1)：
    ;; 无状态徽标；标题 = 粗体工具名 + accent 路径；输出 = muted、缩进 1；
    ;; 上下各一行背景空行；pending/绿/红背景把命令与输出整个包住；
    ;; 标题超宽时降级为纯文本 wrap 后按 toolTitle 着色；
    ;; 展开态不限制输出行数（Ctrl+O 全局切换）
    (define (render-tool-card-lines tool-name args status result width . maybe-expanded?)
      (let* ((expanded? (if (pair? maybe-expanded?) (car maybe-expanded?) #f))
             (bg (%tool-bg status))
             (content-width (max 1 (- width 2)))
             (segs (format-tool-call-segments tool-name args))
             (plain (format-tool-call tool-name args))
             (title-rows
               (if (<= (display-width plain) content-width)
                 (list (apply-card-bg (string-append " " (%style-segments segs)) width bg ""))
                 (map (lambda (line) (apply-card-bg (string-append " " %fg-title line) width bg ""))
                   (wrap-text plain content-width)
                 ) ;map
               ) ;if
             ) ;title-rows
             (res-lines
               (if result
                 (if expanded?
                   (let ((lines (format-tool-result-lines tool-name result :max-lines 0)))
                     (if (null? lines) '() lines)
                   ) ;let
                   (format-tool-result-lines tool-name result)
                 ) ;if
                 '()
               ) ;if
             ) ;res-lines
             (out-rows
               (if (null? res-lines)
                 '()
                 (cons (apply-card-bg "" width bg %fg-output)
                   (map (lambda (line) (apply-card-bg (string-append " " %fg-output line) width bg ""))
                     res-lines
                   ) ;map
                 ) ;cons
               ) ;if
             ) ;out-rows
             (pad-rows (list (apply-card-bg "" width bg %fg-output)))
            ) ;
        (append pad-rows title-rows out-rows pad-rows (list ""))
      ) ;let*
    ) ;define

    ;; 工具调用状态卡片组件 (make-tool-card tool-name args status [result])
    ;; status: 'pending / 'success / 'error
    (define (make-tool-card tool-name args status . maybe-result)
      (let* ((result (if (pair? maybe-result) (car maybe-result) #f))
             (duration (if (> (length maybe-result) 1) (cadr maybe-result) #f))
             (st (%make-tool-card tool-name
                   args
                   status
                   result
                   #f
                   (if (eq? status 'pending) (%tool-now-ms) #f)
                   #f
                   duration
                   #f
                 ) ;%make-tool-card
             ) ;st
             (card
               (make-component
                 :render
                 (lambda (self width)
                   (cond ((<= width 0) '())
                         ((string=? tool-name "bash") (%render-bash-tool-card st width))
                         (else (render-tool-card-lines tool-name
                                 args
                                 (%tool-card-status st)
                                 (%tool-card-result st)
                                 width
                                 (%tool-card-expanded? st)
                               ) ;render-tool-card-lines
                         ) ;else
                   ) ;cond
                 ) ;lambda
               ) ;make-component
             ) ;card
            ) ;
        (component-data-set! card st)
        card
      ) ;let*
    ) ;define

    ;; 助手纯文本回复卡片组件 (make-assistant-card text)
    (define (make-assistant-card text)
      (make-component
        :render
        (lambda (self width)
          (if (<= width 0)
            '()
            (let ((lines (string-split text "\n")))
              (append lines (list ""))
            ) ;let
          ) ;if
        ) ;lambda
      ) ;make-component
    ) ;define

    ;; 等待队列提示卡片组件 (make-queued-card text)
    (define (make-queued-card text)
      (make-component
        :render
        (lambda (self width)
          (if (<= width 0)
            '()
            (list (string-append %fg-dim "  ↳ [Queued: " text "]" %ansi-reset))
          ) ;if
        ) ;lambda
      ) ;make-component
    ) ;define

    ;; 重试提示卡片组件 (make-retry-card msg)
    (define (make-retry-card msg)
      (let ((line (string-append " ↻ " (string-trim-both msg))))
        (make-component
          :render
          (lambda (self width)
            (if (<= width 0)
              '()
              (list (apply-card-bg line width %bg-pending %fg-output) "")
            ) ;if
          ) ;lambda
        ) ;make-component
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
