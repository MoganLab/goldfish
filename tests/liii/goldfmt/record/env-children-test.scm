;; env-children
;; 获取 env 节点的子节点列表。
;;
;; 语法
;; ----
;; (env-children env)
;;
;; 参数
;; ----
;; env : env?
;; 目标环境节点。
;;
;; 返回值
;; ----
;; vector?
;; 子节点向量，元素为 env 记录。

(import (liii check) (liii goldfmt record))

(check-set-mode! 'report-failed)

;; 测试无子节点时返回空向量
(check (env-children (make-env)) => (vector))

;; 测试子节点按顺序保存在向量中
(let* ((child1 (make-env :tag-name "child1" :depth 1 :indent 0))
       (child2 (make-env :tag-name "child2" :depth 1 :indent 2))
       (parent (make-env
                 :tag-name "parent"
                 :depth    0
                 :indent   0
                 :children (vector child1 child2)
               ) ;make-env
       ) ;parent
      ) ;
  (check (vector-length (env-children parent)) => 2)
  (check (env-tag-name (vector-ref (env-children parent) 0)) => "child1")
  (check (env-tag-name (vector-ref (env-children parent) 1)) => "child2")
) ;let*

(check-report)
