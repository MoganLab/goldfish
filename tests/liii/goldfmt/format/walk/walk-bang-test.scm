(import (liii check))
(import (liii string) (liii goldfmt format walk) (liii goldfmt format writer) (liii goldfmt format atom) (liii goldfmt scan)
  (liii goldfmt record)
  (srfi srfi-13))

(check-set-mode! 'report-failed)

;; walk!
;; 将节点树写入 writer 并返回带定位信息的新树，是排版的核心游走过程。
;;
;; 语法
;; ----
;; (walk! node writer column)
;;
;; 返回值
;; ------
;; atom? 或 env?

;; 测试：atom 输出文本
(let* ((w (make-writer 0))
       (new-node (walk! (scan 42) w 0)))
  (check (writer-result w) => "42")
  (check (atom? new-node) => #t)
) ;let*

;; 测试：短 env 单行输出
(let* ((w (make-writer 0))
       (new-node (walk! (scan '(+ 1 2)) w 0)))
  (check (writer-result w) => "(+ 1 2)")
  (check (env? new-node) => #t)
) ;let*

;; 测试：长 env 多行输出且闭括号带注释
(let* ((w (make-writer 0))
       (new-node (walk! (scan '(list aaaaaa bbbbbb cccccc dddddd eeeeee ffffff gggggg hhhhhh iiiiii jjjjjj kkkkkk llllll mmmmmm nnnnnn oooooo pppppp)) w 0)))
  (check (string-contains? (writer-result w) "\n") => #t)
  (check (string-suffix? " ;list" (writer-result w)) => #t)
) ;let*

(check-report)
