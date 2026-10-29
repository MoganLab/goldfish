;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 状态提示卡片组件：对齐上游 pi interactive-mode 的 showStatus ——
;; dim 文本行加前置空行，消息可原地更新（连续 Ctrl+O 不堆积卡片）
(define-library (liii pi code modes interactive status-card)
  (export make-status-card status-card? status-card-set-message!)
  (import (scheme base) (liii tui component))
  (begin

    (define-record-type <status-card>
      (%make-status-card message)
      %status-card?
      (message %status-card-message %status-card-message-set!)
    ) ;define-record-type

    ;; 组件级谓词：chat 容器中的子项是 component，其 data 才是 <status-card>
    (define (status-card? obj)
      (and (component? obj) (%status-card? (component-data obj)))
    ) ;define

    (define (status-card-set-message! card msg)
      (%status-card-message-set! (component-data card) msg)
    ) ;define

    (define (make-status-card msg)
      (let* ((st (%make-status-card msg))
             (card
               (make-component
                 :render
                 (lambda (self width)
                   (if (<= width 0)
                     '()
                     (list ""
                       (string-append "\x1b;[38;2;126;136;142m " (%status-card-message st) "\x1b;[0m")
                     ) ;list
                   ) ;if
                 ) ;lambda
               ) ;make-component
             ) ;card
            ) ;
        (component-data-set! card st)
        card
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
