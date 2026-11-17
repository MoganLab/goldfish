(import (liii check) (liii cmark node))

(check-set-mode! 'report-failed)

;; cmark-node-append-child!
;; 为父节点追加子节点，并保持父子关系一致。
;;
;; 语法
;; ----
;; (cmark-node-append-child! parent child)
;;
;; 返回值
;; ----
;; unspecified
;; 调用后 parent 的 children 增加 child，child 的 parent 指向 parent。

(let ((parent (make-cmark-paragraph))
      (first-child (make-cmark-text "Hello"))
      (second-child (make-cmark-text "World"))
     ) ;
  (cmark-node-append-child! parent first-child)
  (cmark-node-append-child! parent second-child)
  (check (length (cmark-node-children parent)) => 2)
  (check (cmark-node-parent first-child) => parent)
  (check (cmark-node-parent second-child) => parent)
  (check (cmark-node-first-child parent) => first-child)
  (check (cmark-node-last-child parent) => second-child)
) ;let

;; 测试追加第三个子节点
(let ((parent (make-cmark-paragraph))
      (c1 (make-cmark-text "A"))
      (c2 (make-cmark-text "B"))
      (c3 (make-cmark-text "C"))
     ) ;
  (cmark-node-append-child! parent c1)
  (cmark-node-append-child! parent c2)
  (cmark-node-append-child! parent c3)
  (check (length (cmark-node-children parent)) => 3)
  (check (cmark-node-first-child parent) => c1)
  (check (cmark-node-last-child parent) => c3)
  (check (caddr (cmark-node-children parent)) => c3)
) ;let

;; 测试子节点从旧父节点迁移到新父节点
(let ((old-parent (make-cmark-paragraph))
      (new-parent (make-cmark-paragraph))
      (child (make-cmark-text "Moved"))
     ) ;
  (cmark-node-append-child! old-parent child)
  (check (cmark-node-parent child) => old-parent)
  (check (length (cmark-node-children old-parent)) => 1)
  ;; 迁移到新父节点
  (cmark-node-append-child! new-parent child)
  (check (cmark-node-parent child) => new-parent)
  (check (length (cmark-node-children new-parent)) => 1)
  ;; 旧父节点仍保留原引用（当前实现不会自动移除）
  (check (length (cmark-node-children old-parent)) => 1)
) ;let

;; 测试空父节点首次追加后的 first-child 与 last-child 一致
(let ((parent (make-cmark-paragraph)) (child (make-cmark-text "Only")))
  (cmark-node-append-child! parent child)
  (check (cmark-node-first-child parent) => child)
  (check (cmark-node-last-child parent) => child)
  (check (cmark-node-first-child parent) => (cmark-node-last-child parent))
) ;let

(check-report)
