(import (liii check))
(import (liii goldfmt format atom))

(check-set-mode! 'report-failed)

;; keyword-block-prefix
;; 计算首行可以内联摆放的连续 keyword 参数对数量与最大 key 长度。
;;
;; 语法
;; ----
;; (keyword-block-prefix pairs indent)
;;
;; 参数
;; ----
;; pairs : pair? 列表，元素为 (key-len . val-len)
;; indent : integer? 当前缩进列
;;
;; 返回值
;; ------
;; (values count max-key-len)
;; count 为可摆放的对数，max-key-len 为这些 key 的最大长度。

;; 测试：全部可摆放时返回全部对数
(call-with-values
  (lambda () (keyword-block-prefix '((3 . 5) (2 . 4)) 0))
  (lambda (count max-key-len)
    (check count => 2)
    (check max-key-len => 3)
  ) ;lambda
) ;call-with-values

;; 测试：超出行宽时截断（max-inline-length 为 80）
(call-with-values
  (lambda () (keyword-block-prefix '((2 . 2) (60 . 60)) 0))
  (lambda (count max-key-len)
    (check count => 1)
    (check max-key-len => 2)
  ) ;lambda
) ;call-with-values

;; 测试：空列表返回零
(call-with-values
  (lambda () (keyword-block-prefix '() 0))
  (lambda (count max-key-len)
    (check count => 0)
    (check max-key-len => 0)
  ) ;lambda
) ;call-with-values

(check-report)
