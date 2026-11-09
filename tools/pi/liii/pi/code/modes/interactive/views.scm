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
    thinking-card-append-line! thinking-card-set-hidden!
    thinking-card-component? skill-card-component? skill-card-set-expanded!
    pi-pending-hint-line
  ) ;export
  (import (scheme base)
    (liii os)
    (liii string)
    (liii tui component)
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
    (define (%render-queued-card text width)
      (if (<= width 0) '() (list (string-append %fg-dim "  " text %ansi-reset)))
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
