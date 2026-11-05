;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 上下文占用：由单轮 usage 换算上下文 Token 数、占窗口比例，
;; 并格式化为页脚左下角的 "x.x%/1.0M (auto)" 描述
;; （对齐上游 pi packages/coding-agent 的 getContextUsage 与 Footer 显示）
(define-library (liii pi code core context-usage)
  (export pi-calculate-context-tokens pi-context-percent pi-context-usage?
    make-pi-context-usage pi-context-usage-percent pi-context-usage-window
    pi-context-usage-known? pi-format-context-usage pi-session-context-usage
  ) ;export
  (import (scheme base)
    (scheme file)
    (liii string)
    (liii pi code core session-stats)
    (liii pi code core compaction session)
  ) ;import
  (begin

    ;; usage 字段读取：兼容符号键与字符串键
    (define (%ref obj key default)
      (if (not (pair? obj))
        default
        (let* ((str-key (if (symbol? key) (symbol->string key) key))
               (sym-key (if (string? key) (string->symbol key) key))
               (pair (or (assoc key obj) (assoc str-key obj) (assoc sym-key obj)))
              ) ;
          (if (pair? pair) (cdr pair) default)
        ) ;let*
      ) ;if
    ) ;define

    ;; 保留一位小数的定点格式化（12.34 -> "12.3"）
    (define (%fixed1 n)
      (let* ((negative? (< n 0))
             (scaled
               (inexact->exact (round (* (abs n) 10)))
             ) ;scaled
             (int-part (quotient scaled 10))
             (frac (remainder scaled 10))
            ) ;
        (string-append (if negative? "-" "")
          (number->string int-part)
          "."
          (string-pad (number->string frac) 1 #\0)
        ) ;string-append
      ) ;let*
    ) ;define

    ;; 上下文占用 record：百分比 + 窗口容量 + 是否已知
    ;; known? 为 #f 表示压缩后尚无新响应，占比未知（对齐上游显示 "?"）
    (define-record-type <pi-context-usage>
      (make-pi-context-usage percent window known?)
      pi-context-usage?
      (percent pi-context-usage-percent)
      (window pi-context-usage-window)
      (known? pi-context-usage-known?)
    ) ;define-record-type

    ;; 单轮 usage 换算上下文 Token 数
    ;; （对齐上游 calculateContextTokens：totalTokens 优先，否则四项求和）
    (define (pi-calculate-context-tokens usage)
      (let* ((total (%ref usage 'totalTokens 0))
             (total-alt (%ref usage 'total 0))
             (in (%ref usage 'input 0))
             (out (%ref usage 'output 0))
             (c-read (%ref usage 'cacheRead 0))
             (c-write (%ref usage 'cacheWrite 0))
            ) ;
        (cond ((and (number? total) (> total 0)) total)
              ((and (number? total-alt) (> total-alt 0)) total-alt)
              (else (+ (if (number? in) in 0)
                      (if (number? out) out 0)
                      (if (number? c-read) c-read 0)
                      (if (number? c-write) c-write 0)
                    ) ;+
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    ;; 上下文占比（百分数）；窗口缺失或非正数时无法计算，返回 #f
    (define (pi-context-percent tokens window)
      (if (and (number? tokens) (number? window) (> window 0))
        (* 100.0 (/ tokens window))
        #f
      ) ;if
    ) ;define

    ;; 格式化为页脚左下角的上下文描述："12.3%/1.0M (auto)"
    ;; 占比未知时显示 "?"（压缩后尚无新响应，对齐上游）
    (define (pi-format-context-usage ctx . opt-auto)
      (let* ((window (if (pi-context-usage? ctx) (pi-context-usage-window ctx) #f))
             (known (if (pi-context-usage? ctx) (pi-context-usage-known? ctx) #f))
             (percent (if (pi-context-usage? ctx) (pi-context-usage-percent ctx) #f))
             (auto (if (or (null? opt-auto) (car opt-auto)) " (auto)" ""))
             (window-str (if (and (number? window) (> window 0)) (pi-format-token-count window) "0")
             ) ;window-str
            ) ;
        (string-append
          (cond ((not known) "?")
                ((number? percent) (string-append (%fixed1 percent) "%"))
                (else "0.0%")
          ) ;cond
          "/"
          window-str
          auto
        ) ;string-append
      ) ;let*
    ) ;define

    ;; 从会话文件恢复上下文占用（对齐上游 getContextUsage）：
    ;; 取最后一次压缩之后的最后一条 assistant usage 换算占比；
    ;; 压缩之后尚无新响应时占比未知（显示 "?"）
    (define (pi-session-context-usage file-path window)
      (let* ((entries (if (and (string? file-path) (file-exists? file-path))
                        (pi-read-session-entries file-path)
                        '()
                      ) ;if
             ) ;entries
             (last-compaction
               (let loop
                 ((i (- (length entries) 1)))
                 (if (< i 0)
                   -1
                   (if (equal? (%ref (list-ref entries i) 'type "") "compaction") i (loop (- i 1)))
                 ) ;if
               ) ;let
             ) ;last-compaction
             (usage
               (let loop
                 ((i (- (length entries) 1)))
                 (if (<= i last-compaction)
                   #f
                   (let* ((entry (list-ref entries i))
                          (type (%ref entry 'type ""))
                          (msg (%ref entry 'message '()))
                          (msg-usage (%ref msg 'usage '()))
                         ) ;
                     (if (and (equal? type "message")
                           (equal? (%ref msg 'role "") "assistant")
                           (pair? msg-usage)
                         ) ;and
                       msg-usage
                       (loop (- i 1))
                     ) ;if
                   ) ;let*
                 ) ;if
               ) ;let
             ) ;usage
             (tokens (if usage (pi-calculate-context-tokens usage) 0))
            ) ;
        (if (and (>= last-compaction 0) (not usage))
          ;; 压缩后尚无新响应：占比未知
          (make-pi-context-usage #f window #f)
          (make-pi-context-usage (pi-context-percent tokens window) window #t)
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
