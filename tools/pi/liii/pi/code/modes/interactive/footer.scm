;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code modes interactive footer)
  (export make-pi-footer pi-footer? format-cwd-for-footer pi-footer-model-label
    pi-footer-set-thinking-level! pi-footer-branch pi-footer-set-branch!
    pi-footer-set-session-name! pi-footer-set-model! pi-footer-set-message!
    pi-footer-message pi-footer-stats pi-footer-set-stats!
    pi-footer-set-token-stats! pi-footer-set-context-unknown!
    pi-footer-context-window
  ) ;export
  (import (scheme base)
    (liii string)
    (liii os)
    (liii path)
    (liii format base)
    (liii format color)
    (liii tui text-width)
    (liii tui component)
    (liii pi code core session-stats)
    (liii pi code core context-usage)
    (liii pi code modes interactive common)
  ) ;import
  (begin

    ;; 用户主目录：复用 (liii path) 的 path-home（含 Windows HOMEDRIVE+HOMEPATH 回退），
    ;; 未设置时返回 #f
    (define (%default-home)
      (catch #t (lambda () (path->string (path-home))) (lambda args #f))
    ) ;define

    ;; 页脚配色（24-bit RGB）
    (define %c-dim '(102 102 102))

    ;; 上色
    (define (c-dim s)
      (show #f (as-true-color %c-dim s))
    ) ;define

    ;; 上下文占用占比阈值（对齐上游 pi 页脚：>90% 报错色、>70% 警告色）
    (define %ctx-warn-threshold 70)
    (define %ctx-error-threshold 90)

    ;; 上下文描述上色：超阈值标红/标黄，其余按常规暗色显示
    (define (c-context percent info)
      (cond ((not (string? info)) "")
            ((string-null? info) "")
            ((and (number? percent) (> percent %ctx-error-threshold))
             (show #f (as-red info))
            ) ;
            ((and (number? percent) (> percent %ctx-warn-threshold))
             (show #f (as-yellow info))
            ) ;
            (else (c-dim info))
      ) ;cond
    ) ;define

    (define (format-cwd-for-footer cwd home)
      (if (and home (string? home) (not (string-null? home)))
        (if (string=? cwd home)
          "~"
          (if (string-starts? cwd (string-append home "/"))
            (string-append "~" (substring cwd (string-length home) (string-length cwd)))
            cwd
          ) ;if
        ) ;if
        cwd
      ) ;if
    ) ;define

    (define-record-type <pi-footer-state>
      (%make-pi-footer-state cwd branch provider model stats thinking-level
        supports-thinking? session-name message context-info context-percent
        context-window
      ) ;%make-pi-footer-state
      pi-footer-state?
      (cwd pi-footer-state-cwd)
      (branch pi-footer-state-branch pi-footer-state-branch-set!)
      (provider pi-footer-state-provider pi-footer-state-provider-set!)
      (model pi-footer-state-model pi-footer-state-model-set!)
      (stats pi-footer-state-stats pi-footer-state-stats-set!)
      (thinking-level pi-footer-state-thinking-level
        pi-footer-state-thinking-level-set!
      ) ;thinking-level
      (supports-thinking? pi-footer-state-supports-thinking?
        pi-footer-state-supports-thinking?-set!
      ) ;supports-thinking?
      (session-name pi-footer-state-session-name pi-footer-state-session-name-set!)
      (message pi-footer-state-message pi-footer-state-message-set!)
      ;; 上下文占用：描述串（如 "12.3%/1.0M (auto)"）、占比（着色用）与窗口容量
      (context-info pi-footer-state-context-info pi-footer-state-context-info-set!)
      (context-percent pi-footer-state-context-percent
        pi-footer-state-context-percent-set!
      ) ;context-percent
      (context-window pi-footer-state-context-window
        pi-footer-state-context-window-set!
      ) ;context-window
    ) ;define-record-type

    (define (pi-footer? obj)
      (and (component? obj) (pi-footer-state? (component-data obj)))
    ) ;define

    ;; 页脚左下角统计行：token 统计 + 上下文占用描述
    ;; 上下文段按占比阈值着色，其余部分保持暗色（对齐上游 pi 页脚）
    (define (%render-footer st home width)
      (let* ((formatted-cwd (format-cwd-for-footer (pi-footer-state-cwd st) home))
             (branch (pi-footer-state-branch st))
             (session-name (pi-footer-state-session-name st))
             (provider (pi-footer-state-provider st))
             (model (pi-footer-state-model st))
             (stats (pi-footer-state-stats st))
             (context-info (pi-footer-state-context-info st))
             (context-percent (pi-footer-state-context-percent st))
             (message (pi-footer-state-message st))
             (base-line1
               (if (and branch (not (string-null? branch)))
                 (string-append formatted-cwd " (" branch ")")
                 formatted-cwd
               ) ;if
             ) ;base-line1
             (line1
               (if (and session-name (not (string-null? session-name)))
                 (string-append base-line1 " • " session-name)
                 base-line1
               ) ;if
             ) ;line1
             (right-part
               (if (and provider (not (string-null? provider)))
                 (string-append "("
                   provider
                   ") "
                   (pi-footer-model-label model
                     (pi-footer-state-thinking-level st)
                     (pi-footer-state-supports-thinking? st)
                   ) ;pi-footer-model-label
                 ) ;string-append
                 (pi-footer-model-label model
                   (pi-footer-state-thinking-level st)
                   (pi-footer-state-supports-thinking? st)
                 ) ;pi-footer-model-label
               ) ;if
             ) ;right-part
             (left-part
               (cond
                ((and message (not (string-null? message))) (c-dim message))
                ((string-null? stats) (c-context context-percent context-info))
                (else (string-append (c-dim stats) " " (c-context context-percent context-info))
                ) ;else
               ) ;cond
             ) ;left-part
             (left-w (display-width left-part))
             (right-w (display-width right-part))
             (min-padding 2)
             (rem-w (- width left-w min-padding))
             (line2
               (cond
                ((<= (+ left-w min-padding right-w) width)
                 (string-append left-part
                   (make-string (- width left-w right-w) #\space)
                   (c-dim right-part)
                 ) ;string-append
                ) ;
                ((and (<= left-w width) (> rem-w 0))
                 (string-append left-part "  " (c-dim (truncate-to-width right-part rem-w "")))
                ) ;
                (else (truncate-to-width left-part width "..."))
               ) ;cond
             ) ;line2
             (res1 (c-dim (truncate-to-width line1 width "...")))
             (res2 line2)
            ) ;
        (list res1 res2)
      ) ;let*
    ) ;define

    ;; 组装 footer 右下角的模型显示串（对齐上游 pi）：
    ;; 模型支持 reasoning 时，off 也显式显示 " • thinking off"；
    ;; 不支持 reasoning 的模型只显示 model-id
    (define (pi-footer-model-label model-id thinking-level supports-thinking?)
      (if supports-thinking?
        (let ((tl (if (string? thinking-level) (string-trim-both thinking-level) "")))
          (if (or (string-null? tl) (string=? tl "off"))
            (string-append model-id " • thinking off")
            (string-append model-id " • " tl)
          ) ;if
        ) ;let
        model-id
      ) ;if
    ) ;define

    (define (%footer-state comp)
      (component-data comp)
    ) ;define

    (define (make-pi-footer . args)
      (let* ((home (or (%keyword-value :home args #f) (%default-home)))
             (cwd (%keyword-value :cwd args (getcwd)))
             (branch (%keyword-value :branch args #f))
             (provider (or (%keyword-value :provider args #f) "google"))
             (model (or (%keyword-value :model args #f) "gemini-3.8-flash"))
             (stats-opt (%keyword-value :stats args #f))
             (context-opt (%keyword-value :context-info args #f))
             (context-window (or (%keyword-value :context-window args #f) 1000000))
             ;; 显式给定整串统计时不再追加上下文描述
             (stats (if (string? stats-opt) stats-opt ""))
             (context-info
               (cond ((string? context-opt) context-opt)
                     ((string? stats-opt) "")
                     (else (pi-format-context-usage (make-pi-context-usage 0.0 context-window #t)))
               ) ;cond
             ) ;context-info
             (tl (%keyword-value :thinking-level args "off"))
             (supports? (%keyword-value :supports-thinking args #f))
             (session-name (%keyword-value :session-name args #f))
             (comp
               (make-component
                 :render
                 (lambda (self width)
                   (let ((st (%footer-state self)))
                     (%render-footer st home width)
                   ) ;let
                 ) ;lambda
               ) ;make-component
             ) ;comp
            ) ;
        (component-data-set! comp
          (%make-pi-footer-state cwd branch provider model stats tl supports?
            session-name #f context-info 0.0 context-window
          ) ;%make-pi-footer-state
        ) ;component-data-set!
        comp
      ) ;let*
    ) ;define

    ;; 读取 footer 当前显示的分支名（#f 表示无分支）
    (define (pi-footer-branch comp)
      (pi-footer-state-branch (%footer-state comp))
    ) ;define

    ;; 分支切换检测联动：更新左下角分支显示（与 set-thinking-level! 同风格）
    (define (pi-footer-set-branch! comp branch)
      (pi-footer-state-branch-set! (%footer-state comp) branch)
    ) ;define

    ;; /model 会话内切换模型后同步 footer 右下角：provider/model 与 supports 联动
    (define (pi-footer-set-model! comp provider model supports?)
      (let ((st (%footer-state comp)))
        (pi-footer-state-provider-set! st provider)
        (pi-footer-state-model-set! st model)
        (pi-footer-state-supports-thinking?-set! st supports?)
      ) ;let
    ) ;define

    ;; 会话内切换 thinking level 后同步 footer 右下角标签
    (define (pi-footer-set-thinking-level! comp tl)
      (pi-footer-state-thinking-level-set! (%footer-state comp) tl)
    ) ;define

    (define (pi-footer-set-session-name! comp session-name)
      (pi-footer-state-session-name-set! (%footer-state comp) session-name)
    ) ;define

    (define (pi-footer-set-message! comp msg)
      (pi-footer-state-message-set! (%footer-state comp) msg)
    ) ;define

    (define (pi-footer-message comp)
      (pi-footer-state-message (%footer-state comp))
    ) ;define

    ;; 左下角统计串：token 统计与上下文描述的合并结果
    (define (pi-footer-stats comp)
      (let* ((st (%footer-state comp))
             (stats (pi-footer-state-stats st))
             (context-info (pi-footer-state-context-info st))
            ) ;
        (cond
         ((or (not (string? stats)) (string-null? stats)) context-info)
         ((or (not (string? context-info)) (string-null? context-info)) stats)
         (else (string-append stats " " context-info))
        ) ;cond
      ) ;let*
    ) ;define

    ;; 整串统计覆盖：同时清空上下文描述（由调用方自行拼接）
    (define (pi-footer-set-stats! comp stats)
      (let ((st (%footer-state comp)))
        (pi-footer-state-stats-set! st (if (string? stats) stats ""))
        (pi-footer-state-context-info-set! st "")
        (pi-footer-state-context-percent-set! st #f)
      ) ;let
    ) ;define

    ;; 更新 token 统计与上下文占用：
    ;; context 可为 pi-context-usage record（页脚负责格式化与着色）、
    ;; 字符串（直接作为描述，不着色）或 #f（沿用当前描述）
    (define (pi-footer-set-token-stats! comp tokens . rest)
      (let* ((st (%footer-state comp))
             (latest-hit-rate (if (pair? rest) (car rest) #f))
             (opt-context
               (if (and (pair? rest) (pair? (cdr rest))) (cadr rest) #f)
             ) ;opt-context
             (tok-str (pi-format-token-stats tokens latest-hit-rate))
             ;; 窗口缺失时沿用最近一次已知值，避免容量退化为 0
             (context-window
               (let ((w (if (pi-context-usage? opt-context) (pi-context-usage-window opt-context) #f)
                     ) ;w
                    ) ;
                 (if (and (number? w) (> w 0)) w (pi-footer-state-context-window st))
               ) ;let
             ) ;context-window
             (context-info
               (cond
                ((pi-context-usage? opt-context)
                 (pi-format-context-usage (make-pi-context-usage (if (pi-context-usage-known? opt-context)
                                                                   (pi-context-usage-percent opt-context)
                                                                   #f
                                                                 ) ;if
                                            context-window
                                            (pi-context-usage-known? opt-context)
                                          ) ;make-pi-context-usage
                 ) ;pi-format-context-usage
                ) ;
                ((string? opt-context) opt-context)
                (else (pi-footer-state-context-info st))
               ) ;cond
             ) ;context-info
             (context-percent
               (cond ((pi-context-usage? opt-context)
                      (if (pi-context-usage-known? opt-context)
                        (pi-context-usage-percent opt-context)
                        #f
                      ) ;if
                     ) ;
                     ((string? opt-context) #f)
                     (else (pi-footer-state-context-percent st))
               ) ;cond
             ) ;context-percent
            ) ;
        (pi-footer-state-stats-set! st (if (string? tok-str) tok-str ""))
        (pi-footer-state-context-info-set! st context-info)
        (pi-footer-state-context-percent-set! st context-percent)
        (pi-footer-state-context-window-set! st context-window)
      ) ;let*
    ) ;define

    ;; 上下文重建（压缩）后占比未知：显示 "?"，窗口沿用最近一次已知值
    (define (pi-footer-set-context-unknown! comp)
      (let ((st (%footer-state comp)))
        (pi-footer-state-context-info-set! st
          (pi-format-context-usage (make-pi-context-usage #f (pi-footer-state-context-window st) #f)
          ) ;pi-format-context-usage
        ) ;pi-footer-state-context-info-set!
        (pi-footer-state-context-percent-set! st #f)
      ) ;let
    ) ;define

    ;; 当前上下文窗口容量（Token 数）
    (define (pi-footer-context-window comp)
      (pi-footer-state-context-window (%footer-state comp))
    ) ;define

  ) ;begin
) ;define-library
