;;
;; Copyright (C) 2007 by Philip L. Bewig of Saint Louis, Missouri, USA. All
;; rights reserved.  Permission is hereby granted, free of charge, to any
;; person obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to permit
;; persons to whom the Software is furnished to do so, subject to the following
;; conditions: The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.  THE
;; SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;; IN THE SOFTWARE.
;;
;; 移植说明（Goldfish Scheme）：
;; - 基于 SRFI 41 参考实现 primitive.ss / derived.ss 移植，R6RS 改为 R7RS。
;; - SRFI 45 风格的 promise 嫁接原用 mutable pair（R6RS (rnrs mutable-pairs)），
;;   R7RS 无标准 mutable pair，改用记录类型可变字段，可观察行为等价。
;; - stream-match-pattern 原用 syntax-case 宏，改写为纯 syntax-rules
;;   （通配符 _ 用字面量匹配实现）。

(define-library (srfi srfi-41)
  (export stream-null stream-cons stream? stream-null? stream-pair? stream-car
    stream-cdr stream-lambda define-stream list->stream port->stream stream
    stream->list stream-append stream-concat stream-constant stream-drop
    stream-drop-while stream-filter stream-fold stream-for-each stream-from
    stream-iterate stream-length stream-let stream-map stream-match _ stream-of
    stream-range stream-ref stream-reverse stream-scan stream-take
    stream-take-while stream-unfold stream-unfolds stream-zip
  ) ;export
  (import (scheme base) (scheme char))
  (begin

    (define-record-type <stream>
      (make-stream promise)
      stream?
      (promise stream-promise stream-promise-set!)
    ) ;define-record-type

    ;; ==== promise 机制（SRFI 45 语义内联移植）====
    ;; 三态：eager（已求值）/ lazy（延迟值）/ recursive（延迟 promise）
    (define-record-type <stream-promise>
      (%make-promise tag thunk-or-value)
      %promise?
      (tag %promise-tag %promise-tag-set!)
      (thunk-or-value %promise-value %promise-value-set!)
    ) ;define-record-type

    (define-syntax stream-lazy
      (syntax-rules ()
        ((stream-lazy expr) (make-stream (%make-promise 'recursive (lambda () expr))))
      ) ;syntax-rules
    ) ;define-syntax

    (define (stream-eager expr)
      (make-stream (%make-promise 'eager expr))
    ) ;define

    (define-syntax stream-delay
      (syntax-rules ()
        ((stream-delay expr) (stream-lazy (stream-eager expr)))
      ) ;syntax-rules
    ) ;define-syntax

    ;; stream-force：流的求值（SRFI 45 蹦床 + 嫁接语义）
    ;; recursive 跳转时把外层流的 promise 字段直接替换为目标流的 promise，
    ;; 链上所有流别名到最终记忆化的 promise，保证二次 force 返回同一对象
    (define (stream-force strm)
      (let loop
        ((s strm))
        (let ((p (stream-promise s)))
          (if (eq? (%promise-tag p) 'eager)
            (%promise-value p)
            (call-with-values (lambda () ((%promise-value p)))
              (lambda x
                (cond ((eq? (%promise-tag p) 'eager) (%promise-value p))
                      ((and (eq? (%promise-tag p) 'recursive) (= (length x) 1) (stream? (car x)))
                       ;; 先递归 force 目标流使其记忆化，再嫁接其最终状态回 p
                       ;; （SRFI 45 共享 content 语义）
                       (let* ((ts (car x))
                              (tp (stream-promise ts))
                              (result (if (eq? (%promise-tag tp) 'eager) (%promise-value tp) (stream-force ts))
                              ) ;result
                             ) ;
                         ;; 记忆化目标流的 promise，再嫁接回 p
                         (%promise-tag-set! tp 'eager)
                         (%promise-value-set! tp result)
                         (%promise-tag-set! p 'eager)
                         (%promise-value-set! p result)
                         result
                       ) ;let*
                      ) ;
                      ((= (length x) 1)
                       (%promise-tag-set! p 'eager)
                       (%promise-value-set! p (car x))
                       (car x)
                      ) ;
                      (else (apply values x))
                ) ;cond
              ) ;lambda
            ) ;call-with-values
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; ==== 流类型 ====
    (define stream-null (stream-delay (cons 'stream 'null)))

    (define-record-type <stream-pare>
      (make-stream-pare kar kdr)
      stream-pare?
      (kar stream-kar)
      (kdr stream-kdr)
    ) ;define-record-type

    (define (stream-pair? obj)
      (and (stream? obj) (stream-pare? (stream-force obj)))
    ) ;define

    (define (stream-null? obj)
      (and (stream? obj) (eqv? (stream-force obj) (stream-force stream-null)))
    ) ;define

    (define-syntax stream-cons
      (syntax-rules ()
        ((stream-cons obj strm)
         (stream-eager (make-stream-pare (stream-delay obj) (stream-lazy strm)))
        ) ;
      ) ;syntax-rules
    ) ;define-syntax

    (define (stream-car strm)
      (cond ((not (stream? strm)) (error 'stream-car "non-stream"))
            ((stream-null? strm) (error 'stream-car "null stream"))
            (else (stream-force (stream-kar (stream-force strm))))
      ) ;cond
    ) ;define

    (define (stream-cdr strm)
      (cond ((not (stream? strm)) (error 'stream-cdr "non-stream"))
            ((stream-null? strm) (error 'stream-cdr "null stream"))
            (else (stream-kdr (stream-force strm)))
      ) ;cond
    ) ;define

    (define-syntax stream-lambda
      (syntax-rules ()
        ((stream-lambda formals body0 body1 ...)
         (lambda formals (stream-lazy (let () body0 body1 ...)))
        ) ;
      ) ;syntax-rules
    ) ;define-syntax

    ;; ==== derived：流的构造与变换 ====

    ;; SRFI 41 参考实现的多表 exists（R7RS 无标准 exists）
    (define (%stream-exists pred? . lists)
      (and (not (null? (car lists)))
        (or (apply pred? (map car lists)) (apply %stream-exists pred? (map cdr lists)))
      ) ;and
    ) ;define

    (define-syntax define-stream
      (syntax-rules ()
        ((define-stream (name . formal) body0 body1 ...)
         (define name (stream-lambda formal body0 body1 ...))
        ) ;
      ) ;syntax-rules
    ) ;define-syntax

    (define (list->stream objs)
      (define list->stream
        (stream-lambda (objs)
          (if (null? objs) stream-null (stream-cons (car objs) (list->stream (cdr objs))))
        ) ;stream-lambda
      ) ;define
      (if (not (list? objs))
        (error 'list->stream "non-list argument")
        (list->stream objs)
      ) ;if
    ) ;define

    (define (port->stream . port)
      (define port->stream
        (stream-lambda (p)
          (let ((c (read-char p)))
            (if (eof-object? c) stream-null (stream-cons c (port->stream p)))
          ) ;let
        ) ;stream-lambda
      ) ;define
      (let ((p (if (null? port) (current-input-port) (car port))))
        (if (not (input-port? p))
          (error 'port->stream "non-input-port argument")
          (port->stream p)
        ) ;if
      ) ;let
    ) ;define

    (define-syntax stream
      (syntax-rules ()
        ((stream) stream-null)
        ((stream x y ...) (stream-cons x (stream y ...)))
      ) ;syntax-rules
    ) ;define-syntax

    (define (stream->list . args)
      (let ((n (if (= 1 (length args)) #f (car args)))
            (strm (if (= 1 (length args)) (car args) (cadr args)))
           ) ;
        (cond ((not (stream? strm)) (error 'stream->list "non-stream argument"))
              ((and n (not (integer? n))) (error 'stream->list "non-integer count"))
              ((and n (negative? n)) (error 'stream->list "negative count"))
              (else
                (let loop
                  ((n (if n n -1)) (strm strm))
                  (if (or (zero? n) (stream-null? strm))
                    '()
                    (cons (stream-car strm) (loop (- n 1) (stream-cdr strm)))
                  ) ;if
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (stream-append . strms)
      (define stream-append
        (stream-lambda (strms)
          (cond ((null? (cdr strms)) (car strms))
                ((stream-null? (car strms)) (stream-append (cdr strms)))
                (else
                  (stream-cons (stream-car (car strms))
                    (stream-append (cons (stream-cdr (car strms)) (cdr strms)))
                  ) ;stream-cons
                ) ;else
          ) ;cond
        ) ;stream-lambda
      ) ;define
      (cond ((null? strms) stream-null)
            ((%stream-exists (lambda (x) (not (stream? x))) strms)
             (error 'stream-append "non-stream argument")
            ) ;
            (else (stream-append strms))
      ) ;cond
    ) ;define

    (define (stream-concat strms)
      (define stream-concat
        (stream-lambda (strms)
          (cond ((stream-null? strms) stream-null)
                ((not (stream? (stream-car strms)))
                 (error 'stream-concat "non-stream object in input stream")
                ) ;
                ((stream-null? (stream-car strms)) (stream-concat (stream-cdr strms)))
                (else
                  (stream-cons (stream-car (stream-car strms))
                    (stream-concat (stream-cons (stream-cdr (stream-car strms)) (stream-cdr strms)))
                  ) ;stream-cons
                ) ;else
          ) ;cond
        ) ;stream-lambda
      ) ;define
      (if (not (stream? strms))
        (error 'stream-concat "non-stream argument")
        (stream-concat strms)
      ) ;if
    ) ;define

    (define stream-constant
      (stream-lambda objs
        (cond ((null? objs) stream-null)
              ((null? (cdr objs)) (stream-cons (car objs) (stream-constant (car objs))))
              (else
                (stream-cons (car objs)
                  (apply stream-constant (append (cdr objs) (list (car objs))))
                ) ;stream-cons
              ) ;else
        ) ;cond
      ) ;stream-lambda
    ) ;define

    (define (stream-drop n strm)
      (define stream-drop
        (stream-lambda (n strm)
          (if (or (zero? n) (stream-null? strm))
            strm
            (stream-drop (- n 1) (stream-cdr strm))
          ) ;if
        ) ;stream-lambda
      ) ;define
      (cond ((not (integer? n)) (error 'stream-drop "non-integer argument"))
            ((negative? n) (error 'stream-drop "negative argument"))
            ((not (stream? strm)) (error 'stream-drop "non-stream argument"))
            (else (stream-drop n strm))
      ) ;cond
    ) ;define

    (define (stream-drop-while pred? strm)
      (define stream-drop-while
        (stream-lambda (strm)
          (if (and (stream-pair? strm) (pred? (stream-car strm)))
            (stream-drop-while (stream-cdr strm))
            strm
          ) ;if
        ) ;stream-lambda
      ) ;define
      (cond ((not (procedure? pred?)) (error 'stream-drop-while "non-procedural argument"))
            ((not (stream? strm)) (error 'stream-drop-while "non-stream argument"))
            (else (stream-drop-while strm))
      ) ;cond
    ) ;define

    (define (stream-filter pred? strm)
      (define stream-filter
        (stream-lambda (strm)
          (cond ((stream-null? strm) stream-null)
                ((pred? (stream-car strm))
                 (stream-cons (stream-car strm) (stream-filter (stream-cdr strm)))
                ) ;
                (else (stream-filter (stream-cdr strm)))
          ) ;cond
        ) ;stream-lambda
      ) ;define
      (cond ((not (procedure? pred?)) (error 'stream-filter "non-procedural argument"))
            ((not (stream? strm)) (error 'stream-filter "non-stream argument"))
            (else (stream-filter strm))
      ) ;cond
    ) ;define

    (define (stream-fold proc base strm)
      (cond ((not (procedure? proc)) (error 'stream-fold "non-procedural argument"))
            ((not (stream? strm)) (error 'stream-fold "non-stream argument"))
            (else
              (let loop
                ((base base) (strm strm))
                (if (stream-null? strm)
                  base
                  (loop (proc base (stream-car strm)) (stream-cdr strm))
                ) ;if
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    (define (stream-for-each proc . strms)
      (define (stream-for-each strms)
        (if (not (%stream-exists stream-null? strms))
          (begin
            (apply proc (map stream-car strms))
            (stream-for-each (map stream-cdr strms))
          ) ;begin
        ) ;if
      ) ;define
      (cond ((not (procedure? proc)) (error 'stream-for-each "non-procedural argument"))
            ((null? strms) (error 'stream-for-each "no stream arguments"))
            ((%stream-exists (lambda (x) (not (stream? x))) strms)
             (error 'stream-for-each "non-stream argument")
            ) ;
            (else (stream-for-each strms))
      ) ;cond
    ) ;define

    (define (stream-from first . step)
      (define stream-from
        (stream-lambda (first delta)
          (stream-cons first (stream-from (+ first delta) delta))
        ) ;stream-lambda
      ) ;define
      (let ((delta (if (null? step) 1 (car step))))
        (cond ((not (number? first)) (error 'stream-from "non-numeric starting number"))
              ((not (number? delta)) (error 'stream-from "non-numeric step size"))
              (else (stream-from first delta))
        ) ;cond
      ) ;let
    ) ;define

    (define (stream-iterate proc base)
      (define stream-iterate
        (stream-lambda (base) (stream-cons base (stream-iterate (proc base))))
      ) ;define
      (if (not (procedure? proc))
        (error 'stream-iterate "non-procedural argument")
        (stream-iterate base)
      ) ;if
    ) ;define

    (define (stream-length strm)
      (if (not (stream? strm))
        (error 'stream-length "non-stream argument")
        (let loop
          ((len 0) (strm strm))
          (if (stream-null? strm) len (loop (+ len 1) (stream-cdr strm)))
        ) ;let
      ) ;if
    ) ;define

    (define-syntax stream-let
      (syntax-rules ()
        ((stream-let tag ((name val) ...) body1 body2 ...)
         ((letrec ((tag (stream-lambda (name ...) body1 body2 ...))) tag) val ...)
        ) ;
      ) ;syntax-rules
    ) ;define-syntax

    (define (stream-map proc . strms)
      (define stream-map
        (stream-lambda (strms)
          (if (%stream-exists stream-null? strms)
            stream-null
            (stream-cons (apply proc (map stream-car strms))
              (stream-map (map stream-cdr strms))
            ) ;stream-cons
          ) ;if
        ) ;stream-lambda
      ) ;define
      (cond ((not (procedure? proc)) (error 'stream-map "non-procedural argument"))
            ((null? strms) (error 'stream-map "no stream arguments"))
            ((%stream-exists (lambda (x) (not (stream? x))) strms)
             (error 'stream-map "non-stream argument")
            ) ;
            (else (stream-map strms))
      ) ;cond
    ) ;define

    (define-syntax stream-of
      (syntax-rules (in is)
        ((stream-of expr rest ...) (stream-of-aux expr stream-null rest ...))
      ) ;syntax-rules
    ) ;define-syntax

    (define-syntax stream-of-aux
      (syntax-rules (in is)
        ((stream-of-aux expr base) (stream-cons expr base))
        ((stream-of-aux expr base (var in stream) rest ...)
         (stream-let loop
          ((strm stream))
          (if (stream-null? strm)
            base
            (let ((var (stream-car strm)))
              (stream-of-aux expr (loop (stream-cdr strm)) rest ...)
            ) ;let
          ) ;if
         ) ;stream-let
        ) ;
        ((stream-of-aux expr base (var is exp) rest ...)
         (let ((var exp))
           (stream-of-aux expr base rest ...)
         ) ;let
        ) ;
        ((stream-of-aux expr base pred? rest ...)
         (if pred? (stream-of-aux expr base rest ...) base)
        ) ;
      ) ;syntax-rules
    ) ;define-syntax

    (define (stream-range first past . step)
      (define stream-range
        (stream-lambda (first past delta lt?)
          (if (lt? first past)
            (stream-cons first (stream-range (+ first delta) past delta lt?))
            stream-null
          ) ;if
        ) ;stream-lambda
      ) ;define
      (cond ((not (number? first)) (error 'stream-range "non-numeric starting number"))
            ((not (number? past)) (error 'stream-range "non-numeric ending number"))
            (else
              (let ((delta (cond ((pair? step) (car step)) ((< first past) 1) (else -1))))
                (if (not (number? delta))
                  (error 'stream-range "non-numeric step size")
                  (let ((lt? (if (< 0 delta) < >)))
                    (stream-range first past delta lt?)
                  ) ;let
                ) ;if
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    (define (stream-ref strm n)
      (cond ((not (stream? strm)) (error 'stream-ref "non-stream argument"))
            ((not (integer? n)) (error 'stream-ref "non-integer argument"))
            ((negative? n) (error 'stream-ref "negative argument"))
            (else
              (let loop
                ((strm strm) (n n))
                (cond ((stream-null? strm) (error 'stream-ref "beyond end of stream"))
                      ((zero? n) (stream-car strm))
                      (else (loop (stream-cdr strm) (- n 1)))
                ) ;cond
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    (define (stream-reverse strm)
      (define stream-reverse
        (stream-lambda (strm rev)
          (if (stream-null? strm)
            rev
            (stream-reverse (stream-cdr strm) (stream-cons (stream-car strm) rev))
          ) ;if
        ) ;stream-lambda
      ) ;define
      (if (not (stream? strm))
        (error 'stream-reverse "non-stream argument")
        (stream-reverse strm stream-null)
      ) ;if
    ) ;define

    (define (stream-scan proc base strm)
      (define stream-scan
        (stream-lambda (base strm)
          (if (stream-null? strm)
            (stream base)
            (stream-cons base (stream-scan (proc base (stream-car strm)) (stream-cdr strm)))
          ) ;if
        ) ;stream-lambda
      ) ;define
      (cond ((not (procedure? proc)) (error 'stream-scan "non-procedural argument"))
            ((not (stream? strm)) (error 'stream-scan "non-stream argument"))
            (else (stream-scan base strm))
      ) ;cond
    ) ;define

    (define (stream-take n strm)
      (define stream-take
        (stream-lambda (n strm)
          (if (or (stream-null? strm) (zero? n))
            stream-null
            (stream-cons (stream-car strm) (stream-take (- n 1) (stream-cdr strm)))
          ) ;if
        ) ;stream-lambda
      ) ;define
      (cond ((not (stream? strm)) (error 'stream-take "non-stream argument"))
            ((not (integer? n)) (error 'stream-take "non-integer argument"))
            ((negative? n) (error 'stream-take "negative argument"))
            (else (stream-take n strm))
      ) ;cond
    ) ;define

    (define (stream-take-while pred? strm)
      (define stream-take-while
        (stream-lambda (strm)
          (cond ((stream-null? strm) stream-null)
                ((pred? (stream-car strm))
                 (stream-cons (stream-car strm) (stream-take-while (stream-cdr strm)))
                ) ;
                (else stream-null)
          ) ;cond
        ) ;stream-lambda
      ) ;define
      (cond ((not (stream? strm)) (error 'stream-take-while "non-stream argument"))
            ((not (procedure? pred?)) (error 'stream-take-while "non-procedural argument"))
            (else (stream-take-while strm))
      ) ;cond
    ) ;define

    (define (stream-unfold mapper pred? generator base)
      (define stream-unfold
        (stream-lambda (base)
          (if (pred? base)
            (stream-cons (mapper base) (stream-unfold (generator base)))
            stream-null
          ) ;if
        ) ;stream-lambda
      ) ;define
      (cond ((not (procedure? mapper)) (error 'stream-unfold "non-procedural mapper"))
            ((not (procedure? pred?)) (error 'stream-unfold "non-procedural pred?"))
            ((not (procedure? generator)) (error 'stream-unfold "non-procedural generator"))
            (else (stream-unfold base))
      ) ;cond
    ) ;define

    (define (stream-unfolds gen seed)
      (define (len-values gen seed)
        (call-with-values (lambda () (gen seed)) (lambda vs (- (length vs) 1)))
      ) ;define
      (define unfold-result-stream
        (stream-lambda (gen seed)
          (call-with-values (lambda () (gen seed))
            (lambda (next . results) (stream-cons results (unfold-result-stream gen next)))
          ) ;call-with-values
        ) ;stream-lambda
      ) ;define
      (define result-stream->output-stream
        (stream-lambda (result-stream i)
          (let ((result (list-ref (stream-car result-stream) (- i 1))))
            (cond
             ((pair? result)
              (stream-cons (car result)
                (result-stream->output-stream (stream-cdr result-stream) i)
              ) ;stream-cons
             ) ;
             ((not result) (result-stream->output-stream (stream-cdr result-stream) i))
             ((null? result) stream-null)
             (else (error 'stream-unfolds "can't happen"))
            ) ;cond
          ) ;let
        ) ;stream-lambda
      ) ;define
      (define (result-stream->output-streams result-stream)
        (let loop
          ((i (len-values gen seed)) (outputs '()))
          (if (zero? i)
            (apply values outputs)
            (loop (- i 1) (cons (result-stream->output-stream result-stream i) outputs))
          ) ;if
        ) ;let
      ) ;define
      (if (not (procedure? gen))
        (error 'stream-unfolds "non-procedural argument")
        (result-stream->output-streams (unfold-result-stream gen seed))
      ) ;if
    ) ;define

    (define (stream-zip . strms)
      (define stream-zip
        (stream-lambda (strms)
          (if (%stream-exists stream-null? strms)
            stream-null
            (stream-cons (map stream-car strms) (stream-zip (map stream-cdr strms)))
          ) ;if
        ) ;stream-lambda
      ) ;define
      (cond ((null? strms) (error 'stream-zip "no stream arguments"))
            ((%stream-exists (lambda (x) (not (stream? x))) strms)
             (error 'stream-zip "non-stream argument")
            ) ;
            (else (stream-zip strms))
      ) ;cond
    ) ;define

    ;; ==== stream-match：纯 syntax-rules 实现 ====
    ;; 通配符 _ 用字面量匹配；模式匹配器展开为逐元素解构
    ;; SRFI 41 要求导出 _（stream-match 通配符字面量）。
    ;; s7 的库导出需要运行时绑定，这里给出一个哑绑定；
    ;; 实际匹配由 syntax-rules 的字面量 _ 完成，不会引用该值。
    (define _ 'stream-match-wildcard)

    (define-syntax stream-match
      (syntax-rules ()
        ((stream-match strm-expr clause ...)
         (let ((strm strm-expr))
           (cond ((not (stream? strm)) (error 'stream-match "non-stream argument"))
                 ((stream-match-test strm clause) => car)
                 ...
                 (else (error 'stream-match "pattern failure"))
           ) ;cond
         ) ;let
        ) ;
      ) ;syntax-rules
    ) ;define-syntax

    (define-syntax stream-match-test
      (syntax-rules ()
        ((stream-match-test strm (pattern fender expr))
         (stream-match-pattern strm pattern () (and fender (list expr)))
        ) ;
        ((stream-match-test strm (pattern expr))
         (stream-match-pattern strm pattern () (list expr))
        ) ;
      ) ;syntax-rules
    ) ;define-syntax

    (define-syntax stream-match-pattern
      (syntax-rules (_)
        ((stream-match-pattern strm () (binding ...) body)
         (and (stream-null? strm) (let (binding ...) body))
        ) ;
        ((stream-match-pattern strm (_ . rest) (binding ...) body)
         (and (stream-pair? strm)
           (let ((strm (stream-cdr strm)))
             (stream-match-pattern strm rest (binding ...) body)
           ) ;let
         ) ;and
        ) ;
        ((stream-match-pattern strm (var . rest) (binding ...) body)
         (and (stream-pair? strm)
           (let ((temp (stream-car strm)) (strm (stream-cdr strm)))
             (stream-match-pattern strm rest ((var temp) binding ...) body)
           ) ;let
         ) ;and
        ) ;
        ((stream-match-pattern strm _ (binding ...) body) (let (binding ...) body))
        ((stream-match-pattern strm var (binding ...) body)
         (let ((var strm) binding ...)
           body
         ) ;let
        ) ;
      ) ;syntax-rules
    ) ;define-syntax

  ) ;begin
) ;define-library
