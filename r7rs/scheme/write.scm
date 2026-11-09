;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

;; (scheme write) - R7RS-small 6.13 Output: write / write-shared / write-simple
;;
;; 三者对 datum 标签（#n= / #n#）的使用区别：
;;   write        循环结构必须标注（s7 原生 write 即此行为）
;;   write-shared 所有共享与循环结构都标注（当前与 write 一致，见下）
;;   write-simple 绝不标注；循环结构输出占位符 #-1# 并终止该分支
;;
;; write-simple 实现参考 Guile 3：检测"祖先链上重复出现的对象"（真循环）
;; 时输出占位符 #-1#（非规范 datum 标签，不可读回），保证输出有限且确定。
;; R7RS 6.13 明言 write-simple 允许对循环结构不终止，占位符是该许可下的
;; 合规策略。共享（非循环）结构与 write 一致：展开重复、不加标签。
;;
;; write-shared 目前仍是 write 的别名：s7 原生 write 只标注循环结构，
;; 不标注非循环共享结构，与 R7RS 要求的"全部共享结构都标注"尚有差距，
;; 属已知偏差（Goldfish Issue #1263）。

(define-library (scheme write)
  (import (scheme base))
  (export display write write-shared write-simple)
  (begin

    ;; ---- write-shared：两遍实现 ----
    ;; alist 表工具
    (define (label= n)
      (string-append "#" (number->string n) "=")
    ) ;define

    (define (t-set! table key val)
      (let ((hit (assq key table)))
        (if hit (begin (set-cdr! hit val) table) (cons (cons key val) table))
      ) ;let
    ) ;define
    (define (t-ref table key default)
      (let ((hit (assq key table)))
        (if hit (cdr hit) default)
      ) ;let
    ) ;define

    ;; ---- pass1: 引用计数 + 循环标记 ----
    ;; 引用计数：对象作为 child 被访问到的次数（根不算）
    ;; visited：本图内已展开过的 pair/vector
    ;; cyclic：循环上的对象
    (define (wsh:scan root)
      (let ((count '()) (cyclic '()) (visited '()))
        (let visit
          ((obj root) (ancestors '()))
          (cond ((and (pair? obj) (memq obj ancestors))
                 ;; 循环回边：只标记回边目标（与 Guile 输出形态一致）
                 (set! cyclic (t-set! cyclic obj #t))
                ) ;
                ((and (pair? obj) (not (memq obj ancestors)))
                 (let ((seen (t-ref count obj #f)))
                   (cond
                     ;; 重复引用（非祖先链）：计一次引用
                     (seen (set! count (t-set! count obj (+ seen 1))))
                     ;; 首次访问：展开
                     (else (set! count (t-set! count obj 0))
                       (visit (car obj) (cons obj ancestors))
                       (let iter
                         ((rest (cdr obj)) (as (cons obj ancestors)))
                         (cond ((null? rest) #f)
                               ((pair? rest)
                                (cond ((memq rest as) (visit rest as))
                                      ((t-ref count rest #f)
                                       (set! count (t-set! count rest (+ (t-ref count rest 0) 1)))
                                      ) ;
                                      (else (set! count (t-set! count rest 0))
                                        (visit (car rest) (cons rest as))
                                        (iter (cdr rest) (cons rest as))
                                      ) ;else
                                ) ;cond
                               ) ;
                               (else (visit rest as))
                         ) ;cond
                       ) ;let
                     ) ;else
                   ) ;cond
                 ) ;let
                ) ;
                ((vector? obj)
                 (let ((seen (t-ref count obj #f)))
                   (cond
                     (seen (set! count (t-set! count obj (+ seen 1))))
                     ((memq obj ancestors) (set! cyclic (t-set! cyclic obj #t)))
                     (else (set! count (t-set! count obj 0))
                       (let loop
                         ((i 0) (as (cons obj ancestors)))
                         (when (< i (vector-length obj))
                           (visit (vector-ref obj i) as)
                           (loop (+ i 1) as)
                         ) ;when
                       ) ;let
                     ) ;else
                   ) ;cond
                 ) ;let
                ) ;
                (else #f)
          ) ;cond
        ) ;let
        (values count cyclic)
      ) ;let
    ) ;define

    ;; 需要标签 = count>=1 或 cyclic
    (define (wsh:needs-label? obj count cyclic)
      (or (> (t-ref count obj 0) 0) (t-ref cyclic obj #f))
    ) ;define

    ;; ---- pass2: 输出 ----
    (define (wsh:write obj port count cyclic)
      (let ((labels '()) (next 0))
        (define (label= n)
          (string-append "#" (number->string n) "=")
        ) ;define
        (define (label-ref n)
          (string-append "#" (number->string n) "#")
        ) ;define
        (define (ensure-label o)
          (let ((hit (assq o labels)))
            (if hit
              (values (cdr hit) #f)
              (let ((n (+ next 1)))
                (set! next n)
                (set! labels (cons (cons o n) labels))
                (values n #t)
              ) ;let
            ) ;if
          ) ;let
        ) ;define
        (define (emit-ref o)
          ;; 引用处：已定义则 #n#；未定义（仅循环回边先于定义出现）则先定义 #n= 再 #n#?
          ;; R7RS 中回边一定指向已开始定义的祖先，故 labels 必有
          (let ((hit (assq o labels)))
            (write-string (label-ref (if hit (cdr hit) 0)) port)
          ) ;let
        ) ;define
        ;; 引用判定：已在 ancestors（循环回边）或已贴标签（共享二次引用）
        (define (ref? o as)
          (or (memq o as) (assq o labels))
        ) ;define
        (define (emit o as)
          (cond ((and (pair? o) (ref? o as)) (emit-ref o))
                ((pair? o)
                 (if (wsh:needs-label? o count cyclic)
                   (let-values (((n fresh?) (ensure-label o)))
                     (write-string (label= n) port)
                     (emit-pair o (cons o as))
                   ) ;let-values
                   (emit-pair o (cons o as))
                 ) ;if
                ) ;
                ((vector? o)
                 (if (ref? o as)
                   (emit-ref o)
                   (let ((labeled? (wsh:needs-label? o count cyclic)))
                     (let-values (((n fresh?) (if labeled? (ensure-label o) (values #f #f))))
                       (when labeled?
                         (write-string (label= n) port)
                       ) ;when
                       (write-string "#(" port)
                       (when (> (vector-length o) 0)
                         (emit (vector-ref o 0) (cons o as))
                         (let loop
                           ((i 1))
                           (when (< i (vector-length o))
                             (write-string " " port)
                             (emit (vector-ref o i) (cons o as))
                             (loop (+ i 1))
                           ) ;when
                         ) ;let
                       ) ;when
                       (write-string ")" port)
                     ) ;let-values
                   ) ;let
                 ) ;if
                ) ;
                ((bytevector? o) (write o port))
                (else (write o port))
          ) ;cond
        ) ;define
        (define (emit-pair o as)
          (write-string "(" port)
          (emit (car o) as)
          (let iter
            ((rest (cdr o)) (as2 as))
            (cond ((null? rest) (write-string ")" port))
                  ((pair? rest)
                   (cond ((ref? rest as2)
                          ;; 中间或尾部位置的引用：尾部形态 " . #n#)"
                          (write-string " . " port)
                          (emit-ref rest)
                          (write-string ")" port)
                         ) ;
                         ((wsh:needs-label? rest count cyclic)
                          (write-string " " port)
                          (let-values (((n fresh?) (ensure-label rest)))
                            (write-string (label= n) port)
                          ) ;let-values
                          (emit-pair rest (cons rest as2))
                          (write-string ")" port)
                         ) ;
                         (else (write-string " " port)
                           (emit (car rest) (cons rest as2))
                           (iter (cdr rest) (cons rest as2))
                         ) ;else
                   ) ;cond
                  ) ;
                  (else (write-string " . " port) (emit rest as2) (write-string ")" port))
            ) ;cond
          ) ;let
        ) ;define
        (emit obj '())
      ) ;let
    ) ;define

    (define (write-shared obj . port+)
      (let ((port (if (null? port+) (current-output-port) (car port+))))
        (let-values (((count cyclic) (wsh:scan obj)))
          (wsh:write obj port count cyclic)
        ) ;let-values
      ) ;let
    ) ;define

    (define (write-shared obj . port+)
      (let ((port (if (null? port+) (current-output-port) (car port+))))
        (let-values (((count cyclic) (wsh:scan obj)))
          (wsh:write obj port count cyclic)
        ) ;let-values
      ) ;let
    ) ;define

    ;; R7RS 6.13 write-simple：外部表示，永不生成 datum 标签。
    ;; ancestors 为当前输出路径上的 pair/vector（仅检测真循环，
    ;; 不检测共享）。bytevector 无循环可能，直接交给 write。
    (define (write-simple obj . port+)
      (let ((port (if (null? port+) (current-output-port) (car port+))))
        (let loop
          ((obj obj) (ancestors '()))
          (cond ((and (pair? obj) (memq obj ancestors)) (write-string "#-1#" port))
                ((null? obj) (write-string "()" port))
                ((pair? obj)
                 (write-string "(" port)
                 (loop (car obj) (cons obj ancestors))
                 (let iter
                   ((rest (cdr obj)) (as (cons obj ancestors)))
                   (cond ((null? rest) (write-string ")" port))
                         ((pair? rest)
                          (if (memq rest as)
                            (begin
                              (write-string " . #-1#" port)
                              (write-string ")" port)
                            ) ;begin
                            (begin
                              (write-string " " port)
                              (loop (car rest) as)
                              (iter (cdr rest) (cons rest as))
                            ) ;begin
                          ) ;if
                         ) ;
                         (else (write-string " . " port) (loop rest as) (write-string ")" port))
                   ) ;cond
                 ) ;let
                ) ;
                ((bytevector? obj) (write obj port))
                ((vector? obj)
                 (if (memv obj ancestors)
                   (write-string "#-1#" port)
                   (begin
                     (write-string "#(" port)
                     (let ((n (vector-length obj)))
                       (when (> n 0)
                         (loop (vector-ref obj 0) (cons obj ancestors))
                         (do ((i 1 (+ i 1)))
                           ((>= i n))
                           (write-string " " port)
                           (loop (vector-ref obj i) (cons obj ancestors))
                         ) ;do
                       ) ;when
                     ) ;let
                     (write-string ")" port)
                   ) ;begin
                 ) ;if
                ) ;
                (else (write obj port))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
