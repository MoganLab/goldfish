;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code modes interactive footer)
  (export make-pi-footer pi-footer? format-cwd-for-footer pi-footer-model-label
    pi-footer-set-thinking-level! pi-footer-branch pi-footer-set-branch!
  ) ;export
  (import (scheme base)
    (liii string)
    (liii os)
    (liii path)
    (liii format base)
    (liii format color)
    (liii tui text-width)
    (liii tui component)
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

    (define (pi-footer? obj)
      (and (component? obj)
        (let ((d (component-data obj)))
          (and (pair? d) (eq? (car d) 'pi-footer))
        ) ;let
      ) ;and
    ) ;define

    (define (%render-footer cwd home branch provider model stats width)
      (let* ((formatted-cwd (format-cwd-for-footer cwd home))
             (line1
               (if (and branch (not (string-null? branch)))
                 (string-append formatted-cwd " (" branch ")")
                 formatted-cwd
               ) ;if
             ) ;line1
             (right-part
               (if (and provider (not (string-null? provider)))
                 (string-append "(" provider ") " model)
                 model
               ) ;if
             ) ;right-part
             (left-part stats)
             (left-w (display-width left-part))
             (right-w (display-width right-part))
             (min-padding 2)
             (rem-w (- width left-w min-padding))
             (line2
               (cond
                ((<= (+ left-w min-padding right-w) width)
                 (string-append left-part
                   (make-string (- width left-w right-w) #\space)
                   right-part
                 ) ;string-append
                ) ;
                ((and (<= left-w width) (> rem-w 0))
                 (string-append left-part "  " (truncate-to-width right-part rem-w ""))
                ) ;
                (else (truncate-to-width left-part width "..."))
               ) ;cond
             ) ;line2
             (res1 (c-dim (truncate-to-width line1 width "...")))
             (res2 (c-dim line2))
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
      (cdr (component-data comp))
    ) ;define

    (define (make-pi-footer . args)
      (let* ((home (or (%keyword-value :home args #f) (%default-home)))
             (cwd (%keyword-value :cwd args (getcwd)))
             (branch (%keyword-value :branch args #f))
             (provider (or (%keyword-value :provider args #f) "google"))
             (model (or (%keyword-value :model args #f) "gemini-3.8-flash"))
             (stats (%keyword-value :stats args "0.0%/1.0M (auto)"))
             (tl (%keyword-value :thinking-level args "off"))
             (supports? (%keyword-value :supports-thinking args #f))
             (comp
               (make-component
                 :render
                 (lambda (self width)
                   (let ((st (%footer-state self)))
                     (%render-footer (list-ref st 0)
                       home
                       (list-ref st 1)
                       (list-ref st 2)
                       (pi-footer-model-label (list-ref st 3) (list-ref st 5) (list-ref st 6))
                       (list-ref st 4)
                       width
                     ) ;%render-footer
                   ) ;let
                 ) ;lambda
               ) ;make-component
             ) ;comp
            ) ;
        (component-data-set! comp
          (list 'pi-footer cwd branch provider model stats tl supports?)
        ) ;component-data-set!
        comp
      ) ;let*
    ) ;define

    ;; 读取 footer 当前显示的分支名（#f 表示无分支）
    (define (pi-footer-branch comp)
      (list-ref (%footer-state comp) 1)
    ) ;define

    ;; 分支切换检测联动：更新左下角分支显示（与 set-thinking-level! 同风格）
    (define (pi-footer-set-branch! comp branch)
      (let ((st (%footer-state comp)))
        (component-data-set! comp
          (list 'pi-footer
            (list-ref st 0)
            branch
            (list-ref st 2)
            (list-ref st 3)
            (list-ref st 4)
            (list-ref st 5)
            (list-ref st 6)
          ) ;list
        ) ;component-data-set!
      ) ;let
    ) ;define

    ;; 会话内切换 thinking level 后同步 footer 右下角标签
    (define (pi-footer-set-thinking-level! comp tl)
      (let ((st (%footer-state comp)))
        (component-data-set! comp
          (list 'pi-footer
            (list-ref st 0)
            (list-ref st 1)
            (list-ref st 2)
            (list-ref st 3)
            (list-ref st 4)
            tl
            (list-ref st 6)
          ) ;list
        ) ;component-data-set!
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
