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

    (define write-shared write)

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
