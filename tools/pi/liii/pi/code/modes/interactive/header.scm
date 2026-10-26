;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code modes interactive header)
  (export make-pi-header pi-header?)
  (import (scheme base)
    (liii format base)
    (liii format color)
    (liii tui component)
    (liii pi code modes interactive common)
  ) ;import
  (begin

    (define (pi-header? obj)
      (and (component? obj)
        (let ((d (component-data obj)))
          (and (pair? d) (eq? (car d) 'pi-header))
        ) ;let
      ) ;and
    ) ;define

    ;; 页眉配色（24-bit RGB）
    (define %c-brand '(138 190 183))
    (define %c-dim '(102 102 102))
    (define %c-mid '(128 128 128))

    ;; 上色：parts 逐段拼接
    (define (%fg c . parts)
      (show #f (apply as-true-color (cons c parts)))
    ) ;define

    ;; 品牌色加粗
    (define (%fg-bold c . parts)
      (show #f (as-bold (apply as-true-color (cons c parts))))
    ) ;define

    ;; 页眉内容只随 version 变化，与宽度无关，故一次构造后复用
    (define (%header-lines version)
      (let* ((dot (%fg %c-mid " · "))
             (line1 (string-append " " (%fg-bold %c-brand "gf") (%fg %c-dim " v" version)))
             (line2 (string-append " "
                      (%fg %c-dim "escape")
                      (%fg %c-mid " interrupt")
                      dot
                      (%fg %c-dim "ctrl+c/ctrl+d")
                      (%fg %c-mid " clear/exit")
                      dot
                      (%fg %c-dim "/")
                      (%fg %c-mid " commands")
                      dot
                      (%fg %c-dim "!")
                      (%fg %c-mid " bash")
                    ) ;string-append
             ) ;line2
             (line4 (%fg %c-dim
                      "Goldfish Scheme Agent can explain its own features and look up its docs. Ask it how to use or extend it."
                    ) ;%fg
             ) ;line4
            ) ;
        (list "" line1 line2 "" line4 "")
      ) ;let*
    ) ;define

    (define (make-pi-header . args)
      (let* ((version (%keyword-value :version args (version)))
             (lines (%header-lines version))
             (comp (make-component :render (lambda (self width) lines)))
            ) ;
        (component-data-set! comp (list 'pi-header version))
        comp
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
