;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code modes interactive views thinking)
  (export make-thinking-card thinking-card-append-line!)
  (import (scheme base)
    (liii list)
    (liii tui component)
    (liii tui text-width)
    (liii pi code modes interactive views cache)
    (liii pi code modes interactive views theme)
  ) ;import
  (begin

    ;; thinking 内容按完整行流式追加；缓存键包含行列表，追加后自然失效。
    (define-record-type <thinking-card>
      (%make-thinking-card lines render-cache)
      thinking-card?
      (lines %thinking-card-lines %thinking-card-lines-set!)
      (render-cache %thinking-card-render-cache)
    ) ;define-record-type

    ;; 渲染时对每个逻辑行用 wrap-text 折行（前导空格占 1 列，无背景可填满全宽），
    ;; 避免超宽行被 alt-screen 截断吞掉内容；中文按字符宽度逐字折行。
    (define (%render-thinking-card st width)
      (%cached-render (%thinking-card-render-cache st)
        (list (%thinking-card-lines st) width)
        (lambda ()
          (if (<= width 0)
            '()
            (append
              (append-map
                (lambda (line)
                  (map (lambda (seg) (string-append %fg-thinking %italic " " seg %ansi-reset))
                    (wrap-text line (max 1 (- width 1)))
                  ) ;map
                ) ;lambda
                (%thinking-card-lines st)
              ) ;append-map
              (list "")
            ) ;append
          ) ;if
        ) ;lambda
      ) ;%cached-render
    ) ;define

    (define (make-thinking-card first-line)
      (let* ((st (%make-thinking-card (list first-line) (%make-render-cache #f '())))
             (card (make-component :render (lambda (self width) (%render-thinking-card st width)))
             ) ;card
            ) ;
        (component-data-set! card st)
        card
      ) ;let*
    ) ;define

    (define (thinking-card-append-line! card line)
      (let ((st (component-data card)))
        (%thinking-card-lines-set! st (append (%thinking-card-lines st) (list line)))
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
