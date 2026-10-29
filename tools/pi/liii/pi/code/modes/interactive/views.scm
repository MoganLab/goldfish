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
    (liii pi code modes interactive views cache)
    (liii pi code modes interactive views theme)
  ) ;import
  (begin

    ;; 用户提问卡片组件 (make-user-card text)
    ;; 对齐 pi UserMessageComponent：Markdown(paddingX=1, paddingY=1) + userMessageBg/userMessageText，
    ;; 上下各一行带背景的空 padding 行，正文 1 空格缩进，无前缀符号
    (define (%render-user-card text width)
      (if (<= width 0)
        '()
        (let ((rows
                (map (lambda (line) (apply-card-bg (string-append " " line) width %bg-user %fg-user))
                  (string-split text "\n")
                ) ;map
              ) ;rows
             ) ;
          (append (list (apply-card-bg "" width %bg-user %fg-user))
            rows
            (list (apply-card-bg "" width %bg-user %fg-user) "")
          ) ;append
        ) ;let
      ) ;if
    ) ;define

    (define (make-user-card text)
      (%make-cached-component text (lambda (width) (%render-user-card text width)))
    ) ;define

    ;; 工具调用卡片的可变状态。pending 卡片在 start/update/end 事件间复用。
    (define-record-type <tool-card>
      (%make-tool-card tool-name args status result expanded? started-at
        ended-at duration-ms render-cache card-render-cache
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
      ;; 原始 bash 输出缓存与完整卡片渲染缓存分别保存，避免互相覆盖。
      (render-cache %tool-card-render-cache %tool-card-render-cache-set!)
      (card-render-cache %tool-card-card-render-cache)
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

    ;; 各工具折叠预览行数，对齐上游 renderers/*.ts：
    ;; read=0（折叠态不显示输出，仅标题行；错误时例外，见下）、write/edit=10、
    ;; grep=15、find/ls=20，未知工具兑底=10；bash 走专用渲染不经过此表
    (define (%tool-preview-lines tool-name status)
      (cond
       ((and (string=? tool-name "read") (not (eq? status 'error))) 0)
       ((or (string=? tool-name "write") (string=? tool-name "edit")) 10)
       ((string=? tool-name "grep") 15)
       ((or (string=? tool-name "find") (string=? tool-name "ls")) 20)
       (else 10)
      ) ;cond
    ) ;define

    ;; 渲染工具卡片多行行列表的辅助函数（非 bash 工具）
    ;; 对齐 pi ToolExecutionComponent 的 Box(paddingX=1, paddingY=1)：
    ;; 无状态徽标；标题 = 粗体工具名 + accent 路径；输出 = muted、缩进 1；
    ;; 上下各一行背景空行；pending/绿/红背景把命令与输出整个包住；
    ;; 标题超宽时降级为纯文本 wrap 后按 toolTitle 着色；
    ;; 展开态不受预览行数限制，折叠态按工具取预览行数（Ctrl+O 全局切换）
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
             (res-lines (if result
                          (format-tool-result-lines tool-name
                            result
                            :max-lines (%tool-preview-lines tool-name status)
                            :expanded? expanded?
                          ) ;format-tool-result-lines
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

    (define (%tool-card-cache-key st width timer)
      (list (%tool-card-name st)
        (%tool-card-args st)
        (%tool-card-status st)
        (%tool-card-result st)
        width
        (%tool-card-expanded? st)
        timer
      ) ;list
    ) ;define

    (define (%render-tool-card st width timer)
      (let ((cache (%tool-card-card-render-cache st))
            (key (%tool-card-cache-key st width timer))
           ) ;
        ;; bash pending 卡片将按秒变化的计时文本纳入键，其余字段
        ;; 覆盖状态、流式输出与展开切换，命中时不会残留旧内容。
        (if (and (render-cache? cache) (equal? (%render-cache-key cache) key))
          (%render-cache-lines cache)
          (%cache-set-lines! cache
            key
            (let ((tool-name (%tool-card-name st)))
              (cond ((<= width 0) '())
                    ((string=? tool-name "bash") (%render-bash-tool-card st width))
                    (else (render-tool-card-lines tool-name
                            (%tool-card-args st)
                            (%tool-card-status st)
                            (%tool-card-result st)
                            width
                            (%tool-card-expanded? st)
                          ) ;render-tool-card-lines
                    ) ;else
              ) ;cond
            ) ;let
          ) ;%cache-set-lines!
        ) ;if
      ) ;let
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
                   (%make-render-cache #f '())
                 ) ;%make-tool-card
             ) ;st
             (card
               (make-component
                 :render
                 (lambda (self width) (%render-tool-card st width (%tool-timer-text st)))
               ) ;make-component
             ) ;card
            ) ;
        (component-data-set! card st)
        card
      ) ;let*
    ) ;define

    ;; 助手纯文本回复卡片组件 (make-assistant-card text)
    (define (%render-assistant-card text width)
      (if (<= width 0) '() (append (string-split text "\n") (list "")))
    ) ;define

    (define (make-assistant-card text)
      (%make-cached-component text
        (lambda (width) (%render-assistant-card text width))
      ) ;%make-cached-component
    ) ;define

    ;; 等待队列提示卡片组件 (make-queued-card text)
    (define (%render-queued-card text width)
      (if (<= width 0)
        '()
        (list (string-append %fg-dim "  ↳ [Queued: " text "]" %ansi-reset))
      ) ;if
    ) ;define

    (define (make-queued-card text)
      (%make-cached-component text (lambda (width) (%render-queued-card text width)))
    ) ;define

    ;; 重试提示卡片组件 (make-retry-card msg)
    (define (make-retry-card msg)
      (let ((line (string-append " ↻ " (string-trim-both msg))))
        (%make-cached-component msg
          (lambda (width)
            (if (<= width 0)
              '()
              (list (apply-card-bg line width %bg-pending %fg-output) "")
            ) ;if
          ) ;lambda
        ) ;%make-cached-component
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
