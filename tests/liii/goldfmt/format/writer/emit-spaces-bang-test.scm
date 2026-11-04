;; emit-spaces!
;; 输出 n 个空格，列号相应增加。
;;
;; 语法
;; ----
;; (emit-spaces! writer n)
;;
;; 参数
;; ----
;; writer : vector?
;; 由 `make-writer` 创建的 writer。
;;
;; n : integer?
;; 空格数量，非正数时不输出任何内容。
;;
;; 返回值
;; ----
;; any?
;; 无返回值，该函数仅产生副作用。

(import (liii check)
  (liii goldfmt format writer)
  (liii goldfmt record)
  (liii goldfmt scan)
) ;import

(check-set-mode! 'report-failed)

;; 测试：输出指定数量的空格
(let ((w (make-writer 0)))
  (emit-spaces! w 4)
  (check (writer-result w) => "    ")
  (check (writer-column w) => 4)
) ;let

;; 测试：输出 0 个空格不产生任何输出
(let ((w (make-writer 0)))
  (emit-spaces! w 0)
  (check (writer-result w) => "")
  (check (writer-column w) => 0)
) ;let

;; 测试：列号在已有输出之后累加
(let ((w (make-writer 2)))
  (emit-string! w "ab")
  (emit-spaces! w 3)
  (check (writer-column w) => 7)
) ;let

(check-report)
