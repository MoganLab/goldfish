;; walk-env-dollar!
;; 将跨行 $ 管道节点树写入 writer 并返回带定位信息的新树。
;;
;; 语法
;; ----
;; (walk-env-dollar! node writer column walk!)
;;
;; 参数
;; ----
;; node : env?
;; 待输出的 $ 节点。
;;
;; writer : vector?
;; 由 `make-writer` 创建的 writer。
;;
;; column : integer?
;; 节点在当前行的起始列。
;;
;; walk! : procedure?
;; 递归游走子节点的过程。
;;
;; 返回值
;; ------
;; env?

(import (liii check))
(import (liii string)
  (liii goldfmt format dollar)
  (liii goldfmt format walk)
  (liii goldfmt format writer)
  (liii goldfmt scan)
  (liii goldfmt record)
) ;import

(check-set-mode! 'report-failed)

;; 测试：跨行 $ 管道输出且算子与数据对齐
(let* ((w (make-writer 0))
       (node (scan '($ xs :filter odd? :map (* _ 10) :take 3 :sum)))
       (new-node (walk-env-dollar! node w 0 walk!))
      ) ;
  (check (writer-result w)
    =>
    "($ xs\n   :filter odd?\n   :map (* _ 10)\n   :take 3\n   :sum\n) ;$"
  ) ;check
  (check (env? new-node) => #t)
) ;let*

;; 测试：带缩进的 $ 管道（在第 2 列开始）
(let* ((w (make-writer 2))
       (node (scan '($ nums :filter odd? :sum)))
       (new-node (walk-env-dollar! node w 2 walk!))
      ) ;
  (check (writer-result w) => "($ nums\n     :filter odd?\n     :sum\n  ) ;$")
  (check (env? new-node) => #t)
) ;let*

(check-report)
