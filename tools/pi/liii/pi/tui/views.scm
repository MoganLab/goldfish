;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi tui views)
  (export make-user-card make-tool-card make-assistant-card make-queued-card
    make-retry-card apply-card-bg format-tool-call format-tool-result-lines
  ) ;export
  (import (scheme base)
    (liii string)
    (liii tui component)
    (liii tui text-width)
    (liii pi tui renderers)
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
            (let* ((content (string-append " > " text))
                   (row (apply-card-bg content width %bg-user %fg-user))
                  ) ;
              (list row "")
            ) ;let*
          ) ;if
        ) ;lambda
      ) ;make-component
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
      (let ((result (if (pair? maybe-result) (car maybe-result) #f)))
        (make-component
          :render
          (lambda (self width)
            (if (<= width 0)
              '()
              (render-tool-card-lines tool-name args status result width)
            ) ;if
          ) ;lambda
        ) ;make-component
      ) ;let
    ) ;define

    ;; 助手纯文本回复卡片组件 (make-assistant-card text)
    (define (make-assistant-card text)
      (make-component
        :render (lambda (self width) (if (<= width 0) '() (list text "")))
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
