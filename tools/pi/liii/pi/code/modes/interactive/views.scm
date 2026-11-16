;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code modes interactive views)
  (export make-user-card make-tool-card make-assistant-card make-queued-card
    make-retry-card apply-card-bg format-tool-call format-tool-result-lines
    tool-card? tool-card-component? tool-card-expanded? tool-card-set-expanded!
    tool-card-set-output! tool-card-set-complete! make-thinking-card
    tool-card-set-started-at! tool-card-started-at tool-card-pending-bash?
    thinking-card-append-line! thinking-card-set-hidden!
    thinking-card-component? skill-card-component? skill-card-set-expanded!
    pi-pending-hint-line pi-clip-pending-lines
  ) ;export
  (import (scheme base)
    (liii list)
    (liii os)
    (liii string)
    (liii tui component)
    (liii tui box container)
    (liii tui text-width)
    (liii pi code utils shell)
    (liii pi code modes interactive renderers)
    (liii pi code modes interactive views assistant)
    (liii pi code modes interactive views cache)
    (liii pi code modes interactive views theme)
    (liii pi code modes interactive views thinking)
    (liii pi code modes interactive views tool-card)
    (liii pi code modes interactive views wrap)
    (liii pi code modes interactive views skill-card)
    (liii pi code core skills format)
  ) ;import
  (begin

    ;; 用户提问卡片：对齐 pi UserMessageComponent，正文 1 空格缩进；
    ;; 超宽行按内容宽度折行（左右各留 1 空格），避免 alt-screen 截断吞掉内容
    (define (%render-user-card text width)
      (if (<= width 0)
        '()
        (let ((rows
                (%card-body-lines text
                  width
                  1
                  1
                  (lambda (line)
                    (list (apply-card-bg (string-append " " line) width %bg-user %fg-user))
                  ) ;lambda
                ) ;%card-body-lines
              ) ;rows
             ) ;
          (append (list (apply-card-bg "" width %bg-user %fg-user))
            rows
            (list (apply-card-bg "" width %bg-user %fg-user) "")
          ) ;append
        ) ;let
      ) ;if
    ) ;define

    (define (%plain-user-card text)
      (%make-cached-component text (lambda (w) (%render-user-card text w)))
    ) ;define

    ;; 用户卡片：若文本为技能展开块，则以技能折叠卡片呈现（附参独立成卡）
    (define (make-user-card text)
      (let ((blk (parse-skill-block text)))
        (if (not blk)
          (%plain-user-card text)
          (let* ((name (cdr (assoc 'name blk)))
                 (content (cdr (assoc 'content blk)))
                 (user-msg (cdr (assoc 'user-message blk)))
                 (sc (make-skill-card name content))
                ) ;
            (if
              (and (string? user-msg) (not (string-null? (string-trim user-msg))))
              (let ((cnt (make-container)))
                (container-add! cnt sc)
                (container-add! cnt (%plain-user-card user-msg))
                cnt
              ) ;let
              sc
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 排队消息撤回提示行文案（对齐上游 pi interactive-mode dequeueHint）
    (define (pi-pending-hint-line . rest)
      (let* ((macos? (if (pair? rest) (car rest) (os-macos?)))
             (key (if macos? "Option+Up" "Alt+Up"))
            ) ;
        (string-append "↳ " key " to edit all queued messages")
      ) ;let*
    ) ;define

    ;; 等待队列提示卡片组件 (make-queued-card text)
    (define %queued-card-max-lines 8)

    (define (%utf8-safe-prefix str max-bytes)
      (let ((len (string-length str)))
        (if (<= len max-bytes)
          str
          (let loop
            ((i max-bytes))
            (if (<= i 0)
              ""
              (let ((b (char->integer (string-ref str i))))
                (if (and (>= b 128) (< b 192)) (loop (- i 1)) (substring str 0 i))
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%queued-normalize-text text max-bytes)
      (let* ((bounded (%utf8-safe-prefix text max-bytes))
             (clean (sanitize-display-text bounded))
            ) ;
        (string-replace (string-replace (string-replace clean "\r\n" "\n") "\r" "\n")
          "\t"
          " "
        ) ;string-replace
      ) ;let*
    ) ;define

    (define (%render-queued-card text width)
      (if (<= width 0)
        '()
        (let* ((indent (if (>= width 3) "  " ""))
               (content-w (max 1 (- width (string-length indent))))
               (max-bytes (max 256 (* content-w %queued-card-max-lines 4)))
               (normalized (%queued-normalize-text text max-bytes))
               (wrapped (wrap-text normalized content-w))
               (truncated? (> (length wrapped) %queued-card-max-lines))
               (visible
                 (if truncated?
                   (append (take wrapped (- %queued-card-max-lines 1)) (list "..."))
                   wrapped
                 ) ;if
               ) ;visible
              ) ;
          (map (lambda (line) (string-append %fg-dim indent line %ansi-reset)) visible)
        ) ;let*
      ) ;if
    ) ;define

    (define (make-queued-card text)
      (%make-cached-component text (lambda (width) (%render-queued-card text width)))
    ) ;define

    ;; 裁剪过长的待发送队列行，保证至少留出 1 行对话区与 3 行输入区，
    ;; 且队列区高度不超过终端高度的 1/3（最多 8 行）
    (define (pi-clip-pending-lines lines rows footer-count menu-count width)
      (let* ((available (max 0 (- rows footer-count menu-count 4)))
             (max-lines
               (min available (min 8 (max 1 (quotient rows 3))))
             ) ;max-lines
            ) ;
        (cond ((<= (length lines) max-lines) lines)
              ((<= max-lines 0) '())
              ((= max-lines 1)
               (let ((msg (string-append "  ... (" (number->string (length lines)) " lines queued)"))
                    ) ;
                 (list (string-append %fg-dim (truncate-to-width msg width "...") %ansi-reset))
               ) ;let
              ) ;
              (else
                (let* ((kept (take lines (- max-lines 1)))
                       (hidden (- (length lines) (- max-lines 1)))
                       (msg (string-append "  ... (+" (number->string hidden) " more lines)"))
                       (summary (string-append %fg-dim (truncate-to-width msg width "...") %ansi-reset)
                       ) ;summary
                      ) ;
                  (append kept (list summary))
                ) ;let*
              ) ;else
        ) ;cond
      ) ;let*
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
