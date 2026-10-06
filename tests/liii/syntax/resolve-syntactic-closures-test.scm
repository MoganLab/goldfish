(import (liii check) (liii syntax))

(check-set-mode! 'report-failed)

;; resolve-syntactic-closures
;; 将含句法闭包的宏展开结果消解为干净的原生 S 表达式。
;;
;; 语法
;; ----
;; (resolve-syntactic-closures form)
;; (resolve-syntactic-closures form def-env)
;;
;; 参数
;; ----
;; form : any
;; 宏展开产出的表达式，可含句法闭包节点。
;;
;; def-env : environment（可选，默认为 (rootlet)）
;; 兜底环境：当句法闭包自身 env 为空时，用该环境判定符号是否有绑定。
;;
;; 返回值
;; ------
;; any
;; 消解后的纯 S 表达式：
;; - 有绑定的符号（在闭包环境、rootlet 或全局已定义）原样保留；
;; - 无绑定的符号（宏新引入的局部标识符）被重命名为唯一 gensym，
;;   且同一标识符在同一次消解中重命名结果一致（记忆化）；
;; - quote 形式的数据部分只做剥离、不做重命名；
;; - 结果中不再含有任何 syntactic-closure 节点，可直接送入求值器。

;; 1. 无闭包的表达式原样返回（有绑定的符号不重命名）
(check (resolve-syntactic-closures '(+ 1 2)) => '(+ 1 2))
(check (resolve-syntactic-closures 'car) => 'car)
(check (resolve-syntactic-closures 42) => 42)

;; 2. 闭包包装有绑定符号：消解为原符号
(let ((sc (make-syntactic-closure (rootlet) '() 'car)))
  (check (resolve-syntactic-closures sc) => 'car)
) ;let

;; 3. 闭包包装无绑定符号：消解为 gensym
(let* ((sc (make-syntactic-closure (curlet) '() 'my-tmp-var-xyz))
       (r (resolve-syntactic-closures sc)))
  (check-true (gensym? r))
  (check (strip-syntactic-closures (syntactic-closure-expr sc)) => 'my-tmp-var-xyz)
) ;let*

;; 4. 同一未绑定标识符在同一次消解中重命名一致
(let* ((env (curlet))
       (form (list (make-syntactic-closure env '() 'tmp-abc-xyz)
                   (make-syntactic-closure env '() 'tmp-abc-xyz)))
       (r (resolve-syntactic-closures form)))
  (check-true (gensym? (car r)))
  (check (eq? (car r) (cadr r)) => #t)
) ;let*

;; 5. quote 数据部分只剥离不重命名
(let* ((env (curlet))
       (form (list 'quote (make-syntactic-closure env '() 'unbound-xyz)))
       (r (resolve-syntactic-closures form)))
  (check r => '(quote unbound-xyz))
) ;let*

;; 6. 混合：有绑定符号保留，无绑定符号 gensym 化
(let* ((env (curlet))
       (form (list (make-syntactic-closure env '() 'car)
                   (make-syntactic-closure env '() 'fresh-xyz)))
       (r (resolve-syntactic-closures form)))
  (check (car r) => 'car)
  (check-true (gensym? (cadr r)))
) ;let*

;; 7. 向量模板也会被递归消解
(let* ((env (curlet))
       (form (vector (make-syntactic-closure env '() 'car)))
       (r (resolve-syntactic-closures form)))
  (check (vector-ref r 0) => 'car)
) ;let*

;; 8. def-env 参数：闭包 env 为空时用 def-env 判定绑定
(let* ((def (sublet (rootlet) 'my-bound-xyz 1))
       (sc (make-syntactic-closure '() '() 'my-bound-xyz)))
  (check (resolve-syntactic-closures sc def) => 'my-bound-xyz)
) ;let*

(check-report)
