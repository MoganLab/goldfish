;; call-with-shared-ref/cdr
;; 与 call-with-shared-ref 相同，但标签写在 `. ` 分隔之后。
;;
;; 语法
;; ----
;; (call-with-shared-ref/cdr obj shares each proc [sep])
;;
;; 参数
;; ----
;; obj : any
;; 当前要输出的 cdr 子对象。
;;
;; shares : pair?
;; extract-shared-objects 返回的共享表。
;;
;; each : format?
;; 用于拼接输出的组合子构造器。
;;
;; proc : format?
;; 对象首次出现时输出的内容。
;;
;; sep : string?
;; 可选，写在标签前的分隔串，默认为空。
;;
;; 返回值
;; ----
;; format?
;; 输出 `. #n=` 形式标签与内容的格式化组合子。
;;
;; 说明
;; ----
;; 专用于列表尾部的共享标注，例如 `(1 . #0=(2))`。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(define shared (let ((p (list 1))) (list p p)))

(define shares (extract-shared-objects shared #f))

(define part (car shared))

(check (show #f (call-with-shared-ref/cdr part shares each (each "X") " "))
  =>
  " . #0=(X)"
) ;check
(check (show #f (call-with-shared-ref/cdr part shares each (each "Y") " "))
  =>
  " . #0#"
) ;check
(check (show #f (call-with-shared-ref/cdr 42 shares each (each "Z") " "))
  =>
  " Z"
) ;check

(check-report)
