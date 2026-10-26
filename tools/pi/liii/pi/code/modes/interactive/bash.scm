;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; bash 模式卡片组件（对齐 pi 的 BashExecutionComponent）：
;; 输入 !cmd 直接经 bash 执行（不发给模型），输出显示在绿色边框卡片内；
;; 折叠时仅显示最后 PI-BASH-PREVIEW-LINES 行，Ctrl+O 全局切换展开/折叠。
(define-library (liii pi code modes interactive bash)
  (export pi-bash-border-color pi-bash-dim-border-color pi-bash-preview-lines
    make-bash-card bash-card? bash-card-component? bash-card-expanded?
    bash-card-set-output! bash-card-set-complete! bash-card-set-expanded!
    pi-bash-run-command pi-bash-worker pi-bash-send-event!
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
    (liii pi code modes interactive common)
    (liii pi code core tools bash)
  ) ;import
  (begin

    ;; bash mode 主题色（pi system-theme bashMode: green）
    (define pi-bash-border-color "\x1b;[38;2;94;178;134m")
    ;; !! 排除卡片用暗色（对齐 pi colorKey "dim"：结果不进入模型上下文）
    (define pi-bash-dim-border-color "\x1b;[38;2;126;136;142m")
    (define %fg-muted "\x1b;[38;2;157;165;169m")
    ;; error (#ea7f81 = okhsl(20 72% 67%))
    (define %fg-error "\x1b;[38;2;234;127;129m")
    (define %fg-key "\x1b;[38;2;126;136;142m")
    (define %bold-green "\x1b;[0;1;38;2;94;178;134m")
    (define %bold-dim "\x1b;[0;1;38;2;126;136;142m")
    (define %ansi-reset "\x1b;[0m")

    ;; 折叠态预览行数（对齐 pi BashExecutionComponent PREVIEW_LINES = 20）
    (define pi-bash-preview-lines 20)

    ;; 卡片状态：running → complete(0) / error(N) / timeout(-1)
    (define-record-type <bash-card>
      (%make-bash-card command lines status exit-code expanded? excluded?)
      bash-card?
      (command bash-card-command)
      (lines %bash-card-lines %bash-card-lines-set!)
      (status %bash-card-status %bash-card-status-set!)
      (exit-code %bash-card-exit-code %bash-card-exit-code-set!)
      (expanded? %bash-card-expanded? %bash-card-expanded?-set!)
      (excluded? %bash-card-excluded?)
    ) ;define-record-type

    (define (%border width excluded?)
      (string-append (if excluded? pi-bash-dim-border-color pi-bash-border-color)
        (utf8-make-string width #\─)
        %ansi-reset
      ) ;string-append
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
        (let* ((excluded? (%bash-card-excluded? st))
               (header (%color-line " $ "
                         (bash-card-command st)
                         (if excluded? %bold-dim %bold-green)
                         width
                       ) ;%color-line
               ) ;header
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
          (append (list (%border width excluded?) header)
            ;; 输出非空时：空行 + 输出行（对齐 pi 的 "\n${output}" 结构）
            (if (null? out-lines) '() (cons "" out-lines))
            ;; 状态区：仅在有输出时前置空行（Running... 直接跟在头部后）
            (if (null? status) '() (if (null? out-lines) status (cons "" status)))
            (list (%border width excluded?) "")
          ) ;append
        ) ;let*
      ) ;if
    ) ;define

    (define (make-bash-card command . opts)
      (let* ((st (%make-bash-card command
                   '()
                   'running
                   #f
                   (%keyword-value :expanded? opts #f)
                   (%keyword-value :excluded? opts #f)
                 ) ;%make-bash-card
             ) ;st
            ) ;
        (let ((card (make-component :render (lambda (width) (%render st width)))))
          (component-data-set! card st)
          card
        ) ;let
      ) ;let*
    ) ;define

    ;; 设置输出文本（对齐上游 appendOutput + Text 组件：
    ;; 剥 ANSI（CSI+OSC）、\r\n→\n、\r→\n（进度条帧变为独立行）、
    ;; TAB 展开为定宽 3 空格；尾部空行不渲染；空串归一化为无行）
    (define (bash-card-set-output! card text)
      (let* ((st (component-data card))
             (no-ansi (strip-ansi text))
             (no-cr (string-replace (string-replace no-ansi "\r\n" "\n") "\r" "\n"))
             (no-tab (string-replace no-cr "\t" "   "))
             (clean (string-trim-right no-tab #\newline))
            ) ;
        (%bash-card-lines-set! st
          (if (string-null? clean) '() (string-split clean "\n"))
        ) ;%bash-card-lines-set!
      ) ;let*
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
    ;; 退出码 -1 表示超时（对齐 subprocess 的超时约定）；
    ;; 输出对齐上游 session.executeBash，返回前已清理
    ;; （剥 ANSI/OSC、删控制字符、删 \r，保留 \t\n）
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
            (values (pi-bash-clean-output (to-right res)) 0)
            (let ((err (to-left res)))
              (values (pi-bash-clean-output (process-error-stdout err))
                (process-error-code err)
              ) ;values
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 安全带超时的事件发送（导出：go 线程只能看到导出绑定）
    (define (pi-bash-send-event! ch ev)
      (catch #t (lambda () (chan-send! ch ev 2000)) (lambda (t . a) #f))
    ) ;define

    ;; 后台工作协程：顺序消费 (id command excluded?) 任务并回传
    ;; (bash_complete id command text exit-code excluded?)；主线程据此回填卡片
    ;; 并把未排除的结果作为 bash_record 任务投递给 agent worker 注入上下文。
    ;; 通道关闭（TUI 退出）时 recv 返回 eof-object，循环正常结束
    ;; 注意：go 线程只能看到导出绑定，故此过程必须顶层导出
    (define (pi-bash-worker bash-jobs-chan event-chan)
      (let loop
        ()
        (let ((job (chan-recv! bash-jobs-chan)))
          (when (and job (not (eof-object? job)))
            (let* ((id (car job))
                   (command (cadr job))
                   (excluded? (and (> (length job) 2) (caddr job)))
                  ) ;
              (catch #t
                (lambda ()
                  (let-values (((text code) (pi-bash-run-command command)))
                    (pi-bash-send-event! event-chan
                      (list 'bash_complete id command text code (and excluded? #t))
                    ) ;pi-bash-send-event!
                  ) ;let-values
                ) ;lambda
                (lambda (t . a)
                  (pi-bash-send-event! event-chan
                    (list 'bash_complete
                      id
                      command
                      (string-append "bash error: " (object->string t))
                      1
                      #f
                    ) ;list
                  ) ;pi-bash-send-event!
                ) ;lambda
              ) ;catch
              (loop)
            ) ;let*
          ) ;when
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
