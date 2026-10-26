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

    ;; ANSI 颜色常量（严格对应 pi 交互主题体系）
    ;; userMessageBg: 深蓝背景
    (define %bg-user "\x1b;[48;2;38;44;64m")
    (define %fg-user "\x1b;[38;2;230;235;245m")

    ;; toolPendingBg: 执行中微蓝暗灰背景
    (define %bg-pending "\x1b;[48;2;40;42;50m")
    (define %fg-pending "\x1b;[38;2;180;185;195m")

    ;; toolSuccessBg: 工具执行成功标志性深绿背景
    (define %bg-success "\x1b;[48;2;35;55;35m")
    (define %fg-success "\x1b;[38;2;220;245;220m")

    ;; toolErrorBg: 工具执行失败警示深红背景
    (define %bg-error "\x1b;[48;2;60;35;35m")
    (define %fg-error "\x1b;[38;2;250;210;210m")

    ;; 暗淡排队提示
    (define %fg-dim "\x1b;[38;2;130;135;145m")
    (define %ansi-reset "\x1b;[0m")

    ;; 行背景填充函数：计算可见宽度，补足空格并包裹在指定背景/前景 ANSI 码中
    (define (apply-card-bg line width bg-code fg-code)
      (let* ((vlen (display-width line))
             (pad-needed (max 0 (- width vlen)))
             (padded (string-append line (make-string pad-needed #\space)))
            ) ;
        (string-append bg-code fg-code padded %ansi-reset)
      ) ;let*
    ) ;define

    ;; 用户提问卡片组件 (make-user-card text)
    (define (make-user-card text)
      (make-component
        :render
        (lambda (self width)
          (if (<= width 0)
            '()
            (let* ((lines (string-split text "\n"))
                   (rows
                     (map (lambda (line)
                            (apply-card-bg (string-append " > " line) width %bg-user %fg-user)
                          ) ;lambda
                       lines
                     ) ;map
                   ) ;rows
                  ) ;
              (append rows (list ""))
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

    (define (%tool-colors status)
      (case status
       ((success) (list %bg-success %fg-success))
       ((error) (list %bg-error %fg-error))
       (else (list %bg-pending %fg-pending))
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

    ;; Bash 使用上游专用渲染：无状态徽标，输出与计时前后各一个空行。
    (define (%render-bash-tool-card st width)
      (let* ((colors (%tool-colors (%tool-card-status st)))
             (bg (car colors))
             (fg (cadr colors))
             (content-width (max 1 (- width 2)))
             (title-lines (wrap-text (format-tool-call (%tool-card-name st) (%tool-card-args st))
                            content-width
                          ) ;wrap-text
             ) ;title-lines
             (result (%tool-card-result st))
             (result-lines (%cached-bash-result-lines st result content-width))
             (timer (%tool-timer-text st))
             (body (append title-lines
                     (if (null? result-lines) '() (cons "" result-lines))
                     (if timer (list "" timer) '())
                   ) ;append
             ) ;body
             (rows
               (map (lambda (line) (apply-card-bg (string-append " " line) width bg fg)) body)
             ) ;rows
            ) ;
        (append rows (list ""))
      ) ;let*
    ) ;define

    ;; 渲染工具卡片多行行列表的辅助函数
    (define (render-tool-card-lines tool-name args status result width)
      (let* ((badge-and-colors (case status
                                ((success) (list " ✔ " %bg-success %fg-success))
                                ((error) (list " ✖ " %bg-error %fg-error))
                                (else (list " ● " %bg-pending %fg-pending))
                               ) ;case
             ) ;badge-and-colors
             (badge (car badge-and-colors))
             (bg (cadr badge-and-colors))
             (fg (caddr badge-and-colors))
             (title-line (string-append badge (format-tool-call tool-name args)))
             (row1 (apply-card-bg title-line width bg fg))
             (res-lines (if result (format-tool-result-lines tool-name result) '()))
            ) ;
        (if (null? res-lines)
          (list row1 "")
          (let ((rows
                  (map (lambda (line) (apply-card-bg (string-append "    " line) width bg fg))
                    res-lines
                  ) ;map
                ) ;rows
               ) ;
            (append (cons row1 rows) (list ""))
          ) ;let
        ) ;if
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
              (list (apply-card-bg line width %bg-pending %fg-pending) "")
            ) ;if
          ) ;lambda
        ) ;make-component
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
