;; good-env?
;; 检查一棵 env 树是否满足基本结构约束。
;;
;; 语法
;; ----
;; (good-env? env)
;;
;; 参数
;; ----
;; env : env?
;; 待检查的环境节点。
;;
;; 返回值
;; ----
;; boolean?
;; 返回 #t 表示 env 树满足约束；否则返回 #f。

(import (liii check) (liii goldfmt record))

(check-set-mode! 'report-failed)

;; 测试 1: 深度为 0，缩进为 0 - 合法
(check (good-env? (make-env :tag-name "root" :depth 0 :indent 0)) => #t)

;; 测试 2: 深度为 0，缩进为 -1 - 合法（缩进未确认）
(check (good-env? (make-env :tag-name "root" :depth 0 :indent -1)) => #t)

;; 测试 3: 深度为 0，缩进为 2 - 非法（深度 0 的节点缩进必须为 0 或 -1）
(check (good-env? (make-env :tag-name "root" :depth 0 :indent 2)) => #f)

;; 测试 4: 合法的嵌套环境 - 子环境深度正确
(let ((child (make-env :tag-name "child" :depth 1 :indent 0)))
  (check (good-env? (make-env :tag-name "parent" :depth 0 :indent 0 :children (vector child))
         ) ;good-env?
    =>
    #t
  ) ;check
) ;let

;; 测试 5: 非法的嵌套环境 - 子环境深度错误（应为 1，实际为 2）
(let ((child (make-env :tag-name "child" :depth 2 :indent 0)))
  (check (good-env? (make-env :tag-name "parent" :depth 0 :indent 0 :children (vector child))
         ) ;good-env?
    =>
    #f
  ) ;check
) ;let

;; 测试 6: 非法的嵌套环境 - 子环境缩进小于父环境
(let ((child (make-env :tag-name "child" :depth 1 :indent -1)))
  (check (good-env? (make-env :tag-name "parent" :depth 0 :indent 0 :children (vector child))
         ) ;good-env?
    =>
    #f
  ) ;check
) ;let

;; 测试 7: 多层嵌套 - 合法
(let* ((grandchild (make-env :tag-name "grandchild" :depth 2 :indent 2))
       (child (make-env :tag-name "child" :depth 1 :indent 0 :children (vector grandchild))
       ) ;child
      ) ;
  (check (good-env? (make-env :tag-name "parent" :depth 0 :indent 0 :children (vector child))
         ) ;good-env?
    =>
    #t
  ) ;check
) ;let*

;; 测试 8: 多个子环境 - 其中一个非法
(let ((child1 (make-env :tag-name "child1" :depth 1 :indent 0))
      (child2 (make-env :tag-name "child2" :depth 2 :indent 2))
     ) ;
  (check (good-env? (make-env
                      :tag-name "parent"
                      :depth    0
                      :indent   0
                      :children (vector child1 child2)
                    ) ;make-env
         ) ;good-env?
    =>
    #f
  ) ;check
) ;let

(check-report)
