;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 助手纯文本回复卡片组件 (make-assistant-card text)
(define-library (liii pi code modes interactive views assistant)
  (export make-assistant-card)
  (import (scheme base)
    (liii tui component)
    (liii pi code modes interactive views cache)
    (liii pi code modes interactive views wrap)
  ) ;import
  (begin

    ;; 助手正文透明无背景、无前缀符号；
    ;; 超宽行按内容宽度折行（右侧留 1 列边界），避免 alt-screen 截断吞掉内容；
    ;; 末尾保留一行空行作为卡片间隔
    (define (%render-assistant-card text width)
      (if (<= width 0)
        '()
        (append (%card-body-lines text width 0 1 (lambda (line) (list line))) (list ""))
      ) ;if
    ) ;define

    (define (make-assistant-card text)
      (%make-cached-component text
        (lambda (width) (%render-assistant-card text width))
      ) ;%make-cached-component
    ) ;define

  ) ;begin
) ;define-library
