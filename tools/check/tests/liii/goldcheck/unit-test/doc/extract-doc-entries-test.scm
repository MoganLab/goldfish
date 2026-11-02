;; extract-doc-entries
;; 从源码行列表中提取顶部连续注释条目（包含 1-based 行号）。
;;
;; 语法
;; ----
;; (extract-doc-entries lines start-idx)
;;
;; 参数
;; ----
;; lines : list of string  源码全部行列表
;; start-idx : number      开始扫描的 0-based 索引
;;
;; 返回值
;; ------
;; list of pair
;; 每项为 (line-number . line) 对；遇到非注释且非空行即终止扫描。

(import (scheme base) (liii check) (liii goldcheck unit-test doc))

(check-set-mode! 'report-failed)

;; 1. 扫描连续双分号注释与空行
(let* ((lines '(";; fn" ";; 描述" "" ";; 语法" "(import ...)"))
       (entries (extract-doc-entries lines 0))
      ) ;
  (check (length entries) => 3)
  (check (car entries) => '(1 . ";; fn"))
  (check (cadr entries) => '(2 . ";; 描述"))
  (check (caddr entries) => '(4 . ";; 语法"))
) ;let*

;; 2. 从指定偏移开始扫描
(let* ((lines '("ignored" ";; fn" ";; desc" "(+ 1 2)"))
       (entries (extract-doc-entries lines 1))
      ) ;
  (check (length entries) => 2)
  (check (car entries) => '(2 . ";; fn"))
) ;let*

(check-report)
