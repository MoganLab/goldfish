;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi tui views)
  (export make-user-card make-tool-card make-assistant-card make-queued-card
    apply-card-bg
  ) ;export
  (import (scheme base) (liii string) (liii tui component) (liii tui text-width))
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

    ;; 工具调用状态卡片组件 (make-tool-card tool-name args status [result])
    ;; status: 'pending / 'success / 'error
    (define (make-tool-card tool-name args status . maybe-result)
      (let ((result (if (pair? maybe-result) (car maybe-result) #f)))
        (make-component
          :render
          (lambda (self width)
            (if (<= width 0)
              '()
              (let* ((prefix-and-colors (case status
                                         ((success) (list " ✔ " %bg-success %fg-success))
                                         ((error) (list " ✖ " %bg-error %fg-error))
                                         (else (list " ● " %bg-pending %fg-pending))
                                        ) ;case
                     ) ;prefix-and-colors
                     (badge (car prefix-and-colors))
                     (bg (cadr prefix-and-colors))
                     (fg (caddr prefix-and-colors))
                     (title-line (string-append badge tool-name " " (object->string args)))
                     (row1 (apply-card-bg title-line width bg fg))
                    ) ;
                (if result
                  (let* ((res-str (if (string? result) result (object->string result)))
                         (row2 (apply-card-bg (string-append "    " res-str) width bg fg))
                        ) ;
                    (list row1 row2 "")
                  ) ;let*
                  (list row1 "")
                ) ;if
              ) ;let*
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

  ) ;begin
) ;define-library
