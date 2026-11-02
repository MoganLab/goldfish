;; %card-body-lines
;; 将卡片正文按指定宽度折行并应用样式函数生成最终渲染行。
;;
;; 语法
;; ----
;; (%card-body-lines text width left-pad right-pad style-fn)
;;
;; 参数
;; ----
;; text : string?
;; 卡片正文内容。
;; width : integer?
;; 终端可用宽度。
;; left-pad : integer?
;; 左侧留白列宽。
;; right-pad : integer?
;; 右侧留白列宽。
;; style-fn : procedure?
;; 对折行后每行文本应用样式的函数，接收 string 并返回 list of string。
;;
;; 返回值
;; ----
;; list?
;; 经样式函数处理后的行列表。

(import (scheme base) (liii check) (liii pi code modes interactive views wrap))

(check-set-mode! 'report-failed)

;; 1. 折行并应用包装函数
(let ((lines
        (%card-body-lines "line1\nline2"
          40
          1
          1
          (lambda (l) (list (string-append "> " l)))
        ) ;%card-body-lines
      ) ;lines
     ) ;
  (check lines => '("> line1" "> line2"))
) ;let

;; 2. 超长行折行
(let ((lines (%card-body-lines "0123456789" 6 1 1 (lambda (l) (list l)))))
  (check lines => '("0123" "4567" "89"))
) ;let

(check-report)
