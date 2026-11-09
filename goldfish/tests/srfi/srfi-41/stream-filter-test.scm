;; stream-filter
;; 保留流中满足谓词的元素。
;;
;; 语法
;; ----
;; (stream-filter pred? strm)
;;
;; 参数
;; ----
;; pred? : procedure?
;; 谓词。
;; strm : stream?
;; 输入流。
;;
;; 返回值
;; ----
;; stream?
;; 仅含满足 pred? 元素的流。
;;
;; 说明
;; ----
;; 可安全用于无限流。
;;
;; 错误处理
;; ----
;; type-error
;; pred? 不是过程或 strm 不是流

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'type-error (stream-filter "four" (stream 1)))
(check-catch 'type-error (stream-filter odd? '()))
(check (stream-null? (stream-filter odd? (stream))) => #t)
(check (stream->list (stream-filter odd? (stream 1 2 3))) => '(1 3))
(check (stream->list (stream-filter even? (stream 1 2 3))) => '(2))
(check (stream->list 3 (stream-filter odd? (stream-from 0))) => '(1 3 5))

(check-report)
