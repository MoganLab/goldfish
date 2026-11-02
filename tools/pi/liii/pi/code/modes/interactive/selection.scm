;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License.
;;

(define-library (liii pi code modes interactive selection)
  (export make-selection-range %make-selection-range selection-range?
    selection-anchor selection-focus selection-active? selection-normalize
    selection-start selection-end selection-extract-text
    highlight-selection-line highlight-selection-lines
  ) ;export
  (import (scheme base)
    (liii base)
    (liii error)
    (liii string)
    (liii unicode)
    (liii tui text-width)
    (liii tui text-width core)
  ) ;import
  (begin

    (define-record-type <selection-range>
      (%make-selection-range anchor focus active?)
      selection-range?
      (anchor selection-anchor)
      (focus selection-focus)
      (active? selection-active?)
    ) ;define-record-type

    (define (make-selection-range anchor focus active?)
      (%make-selection-range anchor focus (and active? #t))
    ) ;define

    ;; 选区坐标归一化：保证 start-coord 在 end-coord 之前（按行优先，同行按列优先）
    (define (selection-normalize arg1 . rest)
      (let* ((a (if (null? rest) (selection-anchor arg1) arg1))
             (f (if (null? rest) (selection-focus arg1) (car rest)))
             (ax (car a))
             (ay (cdr a))
             (fx (car f))
             (fy (cdr f))
            ) ;
        (if (or (< ay fy) (and (= ay fy) (<= ax fx)))
          (values a f)
          (values f a)
        ) ;if
      ) ;let*
    ) ;define

    (define (selection-start sel)
      (let-values (((s e) (selection-normalize sel)))
        s
      ) ;let-values
    ) ;define

    (define (selection-end sel)
      (let-values (((s e) (selection-normalize sel)))
        e
      ) ;let-values
    ) ;define

    (define (%char-in-sel? cur-w cw max-c2 c1)
      (and (<= cur-w max-c2) (> (+ cur-w cw) c1))
    ) ;define

    (define (%highlight-char-step step char-str cw cur-w max-c2 c1 in-sel? acc loop-fn)
      (if (zero? cw)
        (loop-fn step cur-w in-sel? (cons char-str acc))
        (let ((char-sel? (%char-in-sel? cur-w cw max-c2 c1)))
          (cond
           ((and char-sel? (not in-sel?))
            (loop-fn step (+ cur-w cw) #t (cons char-str (cons "\x1b;[7m" acc)))
           ) ;
           ((and (not char-sel?) in-sel?)
            (loop-fn step (+ cur-w cw) #f (cons char-str (cons "\x1b;[27m" acc)))
           ) ;
           (else (loop-fn step (+ cur-w cw) in-sel? (cons char-str acc)))
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    (define (%highlight-ansi-step ansi-end chunk in-sel? cur-w acc loop-fn)
      (let* ((is-reset? (%is-sgr-reset? chunk))
             (next-acc (if (and in-sel? is-reset?)
                         (cons "\x1b;[7m" (cons chunk acc))
                         (cons chunk acc)
                       ) ;if
             ) ;next-acc
            ) ;
        (loop-fn ansi-end cur-w in-sel? next-acc)
      ) ;let*
    ) ;define

    (define (%highlight-line-loop bv len c1 max-c2)
      (let loop
        ((i 0) (cur-w 0) (in-sel? #f) (acc '()))
        (cond
         ((>= i len)
          (let ((final-acc (if in-sel? (cons "\x1b;[27m" acc) acc)))
            (apply string-append (reverse final-acc))
          ) ;let
         ) ;
         ((%match-ansi bv i len)
          =>
          (lambda (ansi-end)
            (let ((chunk (utf8->string (bytevector-copy bv i ansi-end))))
              (%highlight-ansi-step ansi-end chunk in-sel? cur-w acc loop)
            ) ;let
          ) ;lambda
         ) ;
         (else
           (let* ((next (bytevector-advance-utf8 bv i len))
                  (step (if (= next i) (+ i 1) next))
                  (cp (utf8->codepoint-at bv i))
                  (cw (%codepoint-width cp))
                  (char-str (utf8->string (bytevector-copy bv i step)))
                 ) ;
             (%highlight-char-step step char-str cw cur-w max-c2 c1 in-sel? acc loop)
           ) ;let*
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

    ;; 选区反显高亮单行渲染：在 [c1, c2] 范围内包裹 SGR 7 (反显) 与 SGR 27 (关闭反显)
    (define (highlight-selection-line line c1 c2)
      (let ((max-c2 (if (and c2 (number? c2)) c2 99999999)))
        (if (> c1 max-c2)
          line
          (let* ((bv (string->byte-vector line)) (len (bytevector-length bv)))
            (%highlight-line-loop bv len c1 max-c2)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%highlight-line-at-row line y sy ey sx ex)
      (cond
       ((or (< y sy) (> y ey)) line)
       ((= sy ey) (highlight-selection-line line sx ex))
       ((= y sy) (highlight-selection-line line sx #f))
       ((= y ey) (highlight-selection-line line 0 ex))
       (else (highlight-selection-line line 0 #f))
      ) ;cond
    ) ;define

    ;; 选区反显高亮多行包裹：首行、中间行、末行切片高亮
    (define (highlight-selection-lines lines sel)
      (if (or (not sel) (not (selection-active? sel)))
        lines
        (let-values (((start end) (selection-normalize sel)))
          (let ((sx (car start)) (sy (cdr start)) (ex (car end)) (ey (cdr end)))
            (let loop
              ((y 0) (rest-lines lines) (acc '()))
              (if (null? rest-lines)
                (reverse acc)
                (let ((h-line (%highlight-line-at-row (car rest-lines) y sy ey sx ex)))
                  (loop (+ y 1) (cdr rest-lines) (cons h-line acc))
                ) ;let
              ) ;if
            ) ;let
          ) ;let
        ) ;let-values
      ) ;if
    ) ;define

    (define (%slice-loop bv len c1 max-c2)
      (let loop
        ((i 0) (cur-w 0) (acc '()))
        (if (>= i len)
          (string-trim-right (apply string-append (reverse acc)))
          (let* ((next (bytevector-advance-utf8 bv i len))
                 (step (if (= next i) (+ i 1) next))
                 (cp (utf8->codepoint-at bv i))
                 (cw (%codepoint-width cp))
                 (char-str (utf8->string (bytevector-copy bv i step)))
                ) ;
            (if (zero? cw)
              (loop step cur-w (cons char-str acc))
              (let ((char-sel? (%char-in-sel? cur-w cw max-c2 c1)))
                (loop step
                  (+ cur-w cw)
                  (if char-sel? (cons char-str acc) acc)
                ) ;loop
              ) ;let
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 从单行切片提取纯文本（基于视觉列，向前对齐宽字符，并去除行尾空格）
    (define (%extract-slice-from-line line c1 c2)
      (let* ((clean (strip-ansi line))
             (max-c2 (if (and c2 (number? c2)) c2 99999999))
             (bv (string->byte-vector clean))
            ) ;
        (if (> c1 max-c2)
          ""
          (%slice-loop bv (bytevector-length bv) c1 max-c2)
        ) ;if
      ) ;let*
    ) ;define

    (define (%resolve-selection-bounds arg1 rest)
      (if (null? rest)
        (if (and (selection-range? arg1) (selection-active? arg1))
          (let-values (((s e) (selection-normalize arg1)))
            (values s e #t)
          ) ;let-values
          (values #f #f #f)
        ) ;if
        (let-values (((s e) (selection-normalize arg1 (car rest))))
          (values s e #t)
        ) ;let-values
      ) ;if
    ) ;define

    (define (%extract-lines-range lines sx sy ex ey)
      (let ((num-lines (length lines)))
        (let loop
          ((y sy) (acc '()))
          (if (or (> y ey) (>= y num-lines))
            (string-join (reverse acc) "\n")
            (if (< y 0)
              (loop (+ y 1) acc)
              (let* ((line (list-ref lines y))
                     (c1 (if (= y sy) sx 0))
                     (c2 (if (= y ey) ex #f))
                     (sliced (%extract-slice-from-line line c1 c2))
                    ) ;
                (loop (+ y 1) (cons sliced acc))
              ) ;let*
            ) ;if
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 选区纯文本提取：按行列切片、剔除 ANSI、剔除行尾空格、多行 \n 连接
    (define (selection-extract-text lines arg1 . rest)
      (let-values (((start end active?) (%resolve-selection-bounds arg1 rest)))
        (if (not active?)
          ""
          (%extract-lines-range lines (car start) (cdr start) (car end) (cdr end))
        ) ;if
      ) ;let-values
    ) ;define

  ) ;begin
) ;define-library
