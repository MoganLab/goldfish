(import (liii check) (liii syntax))

(check-set-mode! 'report-failed)

;; identifier=?
;; 判定两个标识符在各自词法环境中是否指向同一绑定。
;;
;; 语法
;; ----
;; (identifier=? e1 id1 e2 id2)
;;
;; 参数
;; ----
;; e1 : environment
;; 第一个标识符的词法环境。
;;
;; id1 : identifier
;; 第一个标识符（符号或包装符号的句法闭包；闭包携带的环境优先于 e1）。
;;
;; e2 : environment
;; 第二个标识符的词法环境。
;;
;; id2 : identifier
;; 第二个标识符。
;;
;; 返回值
;; ------
;; boolean
;; 判定规则：
;; - 两标识符在各自环境中均有绑定，且绑定值相同（eq?），返回 #t；
;; - 两标识符在各自环境中均无绑定（均为自由符号），且符号名相同，返回 #t；
;; - 其余情况返回 #f。
;;
;; 说明
;; ----
;; 这是 R7RS syntax-rules 模式匹配中字面量（literals）比对的底层原语，
;; 对应规范中 free-identifier=? 的语义。

;; 1. 同一环境中的同名绑定：同一槽位，#t
(let ((e (curlet)))
  (check-true (identifier=? e 'car e 'car))
) ;let

;; 2. 均为自由符号且同名：#t（即使环境不同）
(let ((e1 (sublet (rootlet)))
      (e2 (sublet (rootlet))))
  (check-true (identifier=? e1 'some-free-var-xyz e2 'some-free-var-xyz))
) ;let

;; 3. 均为自由符号但不同名：#f
(let ((e (curlet)))
  (check-false (identifier=? e 'some-free-var-xyz e 'another-free-var-xyz))
) ;let

;; 4. 不同环境中的同名局部绑定：绑定值不同，#f
(let ((e1 (sublet (rootlet) 'x 1))
      (e2 (sublet (rootlet) 'x 2)))
  (check-false (identifier=? e1 'x e2 'x))
) ;let

;; 5. 一方有绑定一方自由：#f
(let ((e1 (sublet (rootlet) 'x 1))
      (e2 (sublet (rootlet))))
  (check-false (identifier=? e1 'x e2 'x))
) ;let

;; 6. 句法闭包参数：闭包携带的环境优先于传入的 e
(let* ((def-env (sublet (rootlet)))
       (sc1 (make-syntactic-closure def-env '() 'car))
       (sc2 (make-syntactic-closure def-env '() 'car)))
  (check-true (identifier=? (curlet) sc1 (curlet) sc2))
) ;let*

;; 7. 非符号标识符退化为 equal? 判定
(let ((e (curlet)))
  (check-true (identifier=? e 42 e 42))
  (check-false (identifier=? e 42 e 43))
) ;let

(check-report)
