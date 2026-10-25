;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; bash 模式卡片组件（对齐 pi 的 BashExecutionComponent）：
;; 输入 !cmd 直接经 bash 执行（不发给模型），输出显示在绿色边框卡片内；
;; 折叠时仅显示最后 PI-BASH-PREVIEW-LINES 行，Ctrl+O 全局切换展开/折叠。
(define-library (liii pi code tui bash)
  (export pi-bash-border-color pi-bash-preview-lines make-bash-card bash-card?
    bash-card-component? bash-card-expanded? bash-card-set-output!
    bash-card-set-complete! bash-card-set-expanded! pi-bash-run-command
    pi-bash-worker pi-bash-send-event!
  ) ;export
  (import (scheme base)
    (scheme write)
    (liii string)
    (liii unicode)
    (liii either)
    (liii os)
    (liii go)
    (liii subprocess)
    (liii tui)
    (liii tui component)
    (liii pi code tui common)
    (liii pi code tools bash)
  ) ;import
  (begin

    ;; bash mode 主题色（pi system-theme bashMode: green）
    (define pi-bash-border-color "\x1b;[38;2;94;178;134m")
    (define %fg-muted "\x1b;[38;2;157;165;169m")
    (define %fg-error "\x1b;[38;2;235;106;106m")
    (define %fg-key "\x1b;[38;2;126;136;142m")
    (define %bold-green "\x1b;[0;1;38;2;94;178;134m")
    (define %ansi-reset "\x1b;[0m")

    ;; 折叠态预览行数（对齐 pi BashExecutionComponent PREVIEW_LINES = 20）
    (define pi-bash-preview-lines 20)

    ;; 卡片状态：running → complete(0) / error(N) / timeout(-1)
    (define-record-type <bash-card>
      (%make-bash-card command lines status exit-code expanded?)
      bash-card?
      (command bash-card-command)
      (lines %bash-card-lines %bash-card-lines-set!)
      (status %bash-card-status %bash-card-status-set!)
      (exit-code %bash-card-exit-code %bash-card-exit-code-set!)
      (expanded? %bash-card-expanded? %bash-card-expanded?-set!)
    ) ;define-record-type

    (define (%border width)
      (string-append pi-bash-border-color (utf8-make-string width #\─) %ansi-reset)
    ) ;define

    (define (%color-line prefix text fg width)
      ;; 超宽行截断到终端宽度，保证布局稳定
      (let ((raw (string-append prefix text)))
        (string-append fg
          (if (> (display-width raw) width) (truncate-to-width raw width) raw)
          %ansi-reset
        ) ;string-append
      ) ;let
    ) ;define

    ;; 状态区行列表：运行中 / 省略提示 / 退出码，均为带色整行
    (define (%status-lines st hidden width)
      (define (muted text)
        (%color-line " " text %fg-muted width)
      ) ;define
      (cond ((eq? (%bash-card-status st) 'running) (list (muted "Running...")))
            (else
              (let ((hint
                      (if (> hidden 0)
                        (if (%bash-card-expanded? st)
                          (muted "(ctrl+o to collapse)")
                          (muted (string-append "... " (number->string hidden) " more lines (ctrl+o to expand)")
                          ) ;muted
                        ) ;if
                        #f
                      ) ;if
                    ) ;hint
                    (exit-line
                      (cond
                       ((eq? (%bash-card-status st) 'error)
                        (%color-line " "
                          (string-append "(exit " (number->string (%bash-card-exit-code st)) ")")
                          %fg-error
                          width
                        ) ;%color-line
                       ) ;
                       ((eq? (%bash-card-status st) 'timeout)
                        (%color-line " " "(timeout)" %fg-error width)
                       ) ;
                       (else #f)
                      ) ;cond
                    ) ;exit-line
                   ) ;
                (append (if hint (list hint) '()) (if exit-line (list exit-line) '()))
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    ;; 渲染逻辑：
    ;; 边框 / " $ cmd" 绿粗体 / 空行 / 输出（折叠取尾部 N 行） / 空行 / 状态行 / 边框 / 空行
    (define (%render st width)
      (if (<= width 0)
        '()
        (let* ((header (%color-line " $ " (bash-card-command st) %bold-green width))
               (all-lines (%bash-card-lines st))
               (total (length all-lines))
               ;; 隐藏行数按折叠预览计算（与展开状态无关，对齐 pi hiddenLineCount）
               (hidden (max 0 (- total pi-bash-preview-lines)))
               (shown (if (or (%bash-card-expanded? st) (<= total pi-bash-preview-lines))
                        all-lines
                        (list-tail all-lines hidden)
                      ) ;if
               ) ;shown
               (out-lines (map (lambda (l) (%color-line " " l %fg-muted width)) shown))
               (status (%status-lines st hidden width))
              ) ;
          (append (list (%border width) header)
            ;; 输出非空时：空行 + 输出行（对齐 pi 的 "\n${output}" 结构）
            (if (null? out-lines) '() (cons "" out-lines))
            ;; 状态区：仅在有输出时前置空行（Running... 直接跟在头部后）
            (if (null? status) '() (if (null? out-lines) status (cons "" status)))
            (list (%border width) "")
          ) ;append
        ) ;let*
      ) ;if
    ) ;define

    (define (make-bash-card command . opts)
      (let* ((st (%make-bash-card command '() 'running #f (%keyword-value :expanded? opts #f))
             ) ;st
            ) ;
        (let ((card (make-component :render (lambda (width) (%render st width)))))
          (component-data-set! card st)
          card
        ) ;let
      ) ;let*
    ) ;define

    ;; 设置输出文本（按 \n 拆分为逻辑行；空串归一化为无行）
    (define (bash-card-set-output! card text)
      (let ((st (component-data card)))
        (%bash-card-lines-set! st (if (string-null? text) '() (string-split text "\n")))
      ) ;let
    ) ;define

    ;; 结束运行：exit-code 0 → complete；-1 → timeout；其他 → error
    (define (bash-card-set-complete! card exit-code)
      (let ((st (component-data card)))
        (%bash-card-exit-code-set! st exit-code)
        (%bash-card-status-set! st
          (cond ((= exit-code 0) 'complete)
                ((= exit-code -1) 'timeout)
                (else 'error)
          ) ;cond
        ) ;%bash-card-status-set!
      ) ;let
    ) ;define

    (define (bash-card-set-expanded! card flag)
      (%bash-card-expanded?-set! (component-data card) flag)
    ) ;define

    (define (bash-card-expanded? card)
      (%bash-card-expanded? (component-data card))
    ) ;define

    ;; 组件级谓词：chat 容器中的子项是 component，其 data 才是 <bash-card> 状态
    (define (bash-card-component? obj)
      (and (component? obj) (bash-card? (component-data obj)))
    ) ;define

    ;; 生产执行器：经 bash -c 运行命令，返回 (values 合并输出 退出码)
    ;; 退出码 -1 表示超时（对齐 subprocess 的超时约定）
    (define (pi-bash-run-command command . opts)
      (let ((timeout (%keyword-value :timeout opts 600)))
        (let ((res (run-either (list (pi-find-bash-path) "-c" command)
                     :cwd     (getcwd)
                     :stdin   'null
                     :stdout  'capture
                     :stderr  'stdout
                     :timeout timeout
                   ) ;run-either
              ) ;res
             ) ;
          (if (either-right? res)
            (values (to-right res) 0)
            (let ((err (to-left res)))
              (values (process-error-stdout err) (process-error-code err))
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 安全带超时的事件发送（导出：go 线程只能看到导出绑定）
    (define (pi-bash-send-event! ch ev)
      (catch #t (lambda () (chan-send! ch ev 2000)) (lambda (t . a) #f))
    ) ;define

    ;; 后台工作协程：顺序消费 (id command) 任务并回传 (bash_complete id text exit-code)
    ;; 通道关闭（TUI 退出）时 recv 返回 eof-object，循环正常结束
    ;; 注意：go 线程只能看到导出绑定，故此过程必须顶层导出
    (define (pi-bash-worker bash-jobs-chan event-chan)
      (let loop
        ()
        (let ((job (chan-recv! bash-jobs-chan)))
          (when (and job (not (eof-object? job)))
            (catch #t
              (lambda ()
                (let-values (((text code) (pi-bash-run-command (cadr job))))
                  (pi-bash-send-event! event-chan (list 'bash_complete (car job) text code))
                ) ;let-values
              ) ;lambda
              (lambda (t . a)
                (pi-bash-send-event! event-chan
                  (list 'bash_complete
                    (car job)
                    (string-append "bash error: " (object->string t))
                    1
                  ) ;list
                ) ;pi-bash-send-event!
              ) ;lambda
            ) ;catch
            (loop)
          ) ;when
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
