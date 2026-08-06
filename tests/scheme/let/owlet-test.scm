(import (liii check))
(import (scheme let))


(check-set-mode! 'report-failed)


;; owlet
;; 返回当前的 outward let（模块环境）。
;; owlet 反映当前正在求值的模块/库的环境。
;;
;; 语法
;; ----
;; (owlet)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; ------
;; let?
;; 当前的 outward（模块）环境。
;;
;; 说明
;; ----
;; owlet 与 curlet 不同：curlet 是最内层的词法环境，
;; owlet 是当前所在的模块/库层环境。
;; owlet 通常包含一些错误处理相关的元信息绑定。


;; owlet 是一个 let
(check (let? (owlet)) => #t)


;; owlet 中包含 error-position 等错误元信息绑定
(check (integer? (let-ref (owlet) 'error-position)) => #t)


;; owlet 包含 error-file 绑定（布尔或字符串）
(check (boolean? (let-ref (owlet) 'error-file)) => #t)


(check-report)
