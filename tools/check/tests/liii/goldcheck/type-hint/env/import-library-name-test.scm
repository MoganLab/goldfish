;; import-library-name
;; 从 import 子句中提取库名（剥去 except/only/prefix/rename 等修饰）。
;;
;; 语法
;; ----
;; (import-library-name clause)
;;
;; 参数
;; ----
;; clause : list  import 子句或库命名式
;;
;; 返回值
;; ------
;; list of symbol
;; 库名（如 (liii base)）。

(import (scheme base)
  (liii check)
  (liii goldcheck type-hint env)
)

(check-set-mode! 'report-failed)

;; 1. 裸库名原样返回
(check (import-library-name '(liii base)) => '(liii base))

;; 2. only 修饰
(check (import-library-name '(only (scheme base) car cdr)) => '(scheme base))

;; 3. except 修饰
(check (import-library-name '(except (scheme base) car)) => '(scheme base))

;; 4. prefix 修饰
(check (import-library-name '(prefix (liii list) list/)) => '(liii list))

;; 5. rename 修饰
(check (import-library-name '(rename (liii base) (old new))) => '(liii base))

;; 6. 嵌套修饰递归剥到底
(check (import-library-name '(only (except (scheme base) car) cdr)) => '(scheme base))

(check-report)
