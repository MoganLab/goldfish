;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code modes interactive views thinking)
  (export make-thinking-card thinking-card-append-line!
    thinking-card-set-hidden! thinking-card? thinking-card-component?
  ) ;export
  (import (scheme base)
    (liii list)
    (liii tui component)
    (liii tui text-width)
    (liii pi code modes interactive views cache)
    (liii pi code modes interactive views theme)
  ) ;import
  (begin

    ;; thinking 内容按完整行流式追加；缓存键包含行列表，追加后自然失效。
    ;; hidden 记录 Ctrl+T 折叠态：隐藏时整卡只渲染单行 Thinking... 标签。
    (define-record-type <thinking-card>
      (%make-thinking-card lines hidden render-cache)
      thinking-card?
      (lines %thinking-card-lines %thinking-card-lines-set!)
      (hidden %thinking-card-hidden %thinking-card-hidden-set!)
      (render-cache %thinking-card-render-cache)
    ) ;define-record-type

    ;; 隐藏态标签：单行斜体灰字 Thinking...（对齐上游 hiddenThinkingLabel）
    (define %thinking-hidden-label "Thinking...")

    ;; 渲染时对每个逻辑行用 wrap-text 折行（前导空格占 1 列，无背景可填满全宽），
    ;; 避免超宽行被 alt-screen 截断吞掉内容；中文按字符宽度逐字折行。
    ;; 隐藏态只渲染标签行 + 尾部 spacer，hidden 进入缓存键。
    (define (%render-thinking-card st width)
      (%cached-render (%thinking-card-render-cache st)
        (list (%thinking-card-lines st) width (%thinking-card-hidden st))
        (lambda ()
          (if (<= width 0)
            '()
            (if (%thinking-card-hidden st)
              (list (string-append %fg-thinking %italic " " %thinking-hidden-label %ansi-reset)
                ""
              ) ;list
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
          ) ;if
        ) ;lambda
      ) ;%cached-render
    ) ;define

    (define* (make-thinking-card first-line (hidden #f))
      (let* ((st (%make-thinking-card (list first-line)
                   (and hidden #t)
                   (%make-render-cache #f '())
                 ) ;%make-thinking-card
             ) ;st
             (card (make-component :render (lambda (self width) (%render-thinking-card st width)))
             ) ;card
            ) ;
        (component-data-set! card st)
        card
      ) ;let*
    ) ;define*

    (define (thinking-card-append-line! card line)
      (let ((st (component-data card)))
        (%thinking-card-lines-set! st (append (%thinking-card-lines st) (list line)))
      ) ;let
    ) ;define

    ;; Ctrl+T 切换折叠态：只改 hidden 标志，流式行不受影响；
    ;; hidden 在渲染缓存键中，下次渲染自然按新状态输出
    (define (thinking-card-set-hidden! card hidden)
      (%thinking-card-hidden-set! (component-data card) (and hidden #t))
    ) ;define

    ;; 组件级谓词：chat 容器中的子项是 component，其 data 才是 <thinking-card> 状态
    (define (thinking-card-component? obj)
      (and (component? obj) (thinking-card? (component-data obj)))
    ) ;define

  ) ;begin
) ;define-library
