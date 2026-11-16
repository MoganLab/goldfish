;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 助手纯文本回复卡片组件 (make-assistant-card text)
(define-library (liii pi code modes interactive views assistant)
  (export make-assistant-card assistant-card-append-text! assistant-card-text
    assistant-card? assistant-card-component?
  ) ;export
  (import (scheme base)
    (liii string)
    (liii tui component)
    (liii pi code modes interactive views cache)
    (liii pi code modes interactive views wrap)
  ) ;import
  (begin

    (define-record-type <assistant-card>
      (%make-assistant-card text render-cache)
      assistant-card?
      (text %assistant-card-text %assistant-card-text-set!)
      (render-cache %assistant-card-render-cache)
    ) ;define-record-type

    ;; 助手正文透明无背景、无前缀符号；
    ;; 超宽行按内容宽度折行（右侧留 1 列边界），避免 alt-screen 截断吞掉内容；
    ;; 末尾保留一行空行作为卡片间隔
    (define (%render-assistant-card st width)
      (%cached-render (%assistant-card-render-cache st)
        (list (%assistant-card-text st) width)
        (lambda ()
          (if (<= width 0)
            '()
            (append (%card-body-lines (%assistant-card-text st)
                      width
                      0
                      1
                      (lambda (line) (list line))
                    ) ;%card-body-lines
              (list "")
            ) ;append
          ) ;if
        ) ;lambda
      ) ;%cached-render
    ) ;define

    (define (make-assistant-card text)
      (let* ((st (%make-assistant-card text (%make-render-cache #f '())))
             (card (make-component :render (lambda (self width) (%render-assistant-card st width)))
             ) ;card
            ) ;
        (component-data-set! card st)
        card
      ) ;let*
    ) ;define

    (define (assistant-card-text card)
      (%assistant-card-text (component-data card))
    ) ;define

    (define (assistant-card-append-text! card delta)
      (when (and (string? delta) (not (string-null? delta)))
        (let ((st (component-data card)))
          (%assistant-card-text-set! st (string-append (%assistant-card-text st) delta))
        ) ;let
      ) ;when
    ) ;define

    (define (assistant-card-component? obj)
      (and (component? obj) (assistant-card? (component-data obj)))
    ) ;define

  ) ;begin
) ;define-library
