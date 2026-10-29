;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code modes interactive views cache)
  (export %make-render-cache %cached-render render-cache? %render-cache-key
    %render-cache-lines %cache-set-lines! %make-cached-component
  ) ;export
  (import (scheme base) (liii tui component))
  (begin

    ;; 相同键直接返回上次行列表，避免历史卡片逐帧重算。
    (define-record-type <render-cache>
      (%make-render-cache key lines)
      render-cache?
      (key %render-cache-key %render-cache-key-set!)
      (lines %render-cache-lines %render-cache-lines-set!)
    ) ;define-record-type

    (define (%cached-render cache key render-lines)
      (if (and (render-cache? cache) (equal? (%render-cache-key cache) key))
        (%render-cache-lines cache)
        (let ((lines (render-lines)))
          (%cache-set-lines! cache key lines)
        ) ;let
      ) ;if
    ) ;define

    (define (%cache-set-lines! cache key lines)
      (%render-cache-key-set! cache key)
      (%render-cache-lines-set! cache lines)
      lines
    ) ;define

    (define (%make-cached-component key render-lines)
      (let ((cache (%make-render-cache #f '())))
        (make-component
          :render
          (lambda (self width)
            (%cached-render cache (list key width) (lambda () (render-lines width)))
          ) ;lambda
        ) ;make-component
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
