;; stream-constant
;; 构造无限循环给定元素的流。
;;
;; 语法
;; ----
;; (stream-constant obj ...)
;;
;; 参数
;; ----
;; obj : any
;; 一个或多个元素。
;; 返回值
;; ----
;; stream?
;; 无限重复这些元素的流。
;;
;; 说明
;; ----
;; SRFI 41：元素循环出现，(stream-constant 1 2) 即 1 2 1 2 ...。

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check (stream-ref (stream-constant 1) 100) => 1)
(check (stream-ref (stream-constant 1 2) 100) => 1)
(check (stream-ref (stream-constant 1 2 3) 3) => 1)
(check (stream->list 5 (stream-constant 1 2)) => '(1 2 1 2 1))

(check-report)
