;; extract-shared-objects
;; 扫描对象结构，收集需要打标签的共享或循环子对象。
;;
;; 语法
;; ----
;; (extract-shared-objects x cyclic-only?)
;;
;; 参数
;; ----
;; x : any
;; 待扫描的对象。
;;
;; cyclic-only? : boolean?
;; 非假时只收集循环结构，忽略仅被引用多次的共享结构。
;;
;; 返回值
;; ----
;; pair?
;; `(cons hash-table count)`：表把每个待标注对象映射到序号
;; 占位对，count 是初始计数 0。
;;
;; 说明
;; ----
;; 返回值随后交给 call-with-shared-ref 等函数生成 `#n=` /
;; `#n#` 标签，是 written 系列的底层设施。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (liii hash-table) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(define shared (let ((p (list 1))) (list p p)))

(define cyclic (let ((c (list 1))) (set-cdr! c c) c))

(define (collected? shares obj)
  (not (not (hash-table-ref/default (car shares) obj #f)))
) ;define

(check-true (pair? (extract-shared-objects shared #f)))
(check-true (hash-table? (car (extract-shared-objects shared #f))))

;; 被引用多次的子对象会被收集
(check-true (collected? (extract-shared-objects shared #f) (car shared)))

;; cyclic-only? 为真时只保留循环结构
(check-true (collected? (extract-shared-objects cyclic #t) cyclic))
(check-false (collected? (extract-shared-objects shared #t) (car shared)))

;; 只引用一次的对象不收集
(check-false (collected? (extract-shared-objects '(1 2) #f) 1))

(check-report)
