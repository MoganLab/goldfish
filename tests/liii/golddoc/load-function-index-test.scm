;; 添加 tools/golddoc 到 load path，以便导入 (liii golddoc)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path* (cons "tools/golddoc" *load-path*))

(import (liii check)
        (liii golddoc)
        (liii path)
) ;import

(check-set-mode! 'report-failed)

;; load-function-index
;; 读取当前 *load-path* 可见测试根目录下的函数索引 JSON。
;;
;; 语法
;; ----
;; (load-function-index)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; alist
;; 返回形如 `((function-name . ("(org lib)" ...)) ...)` 的关联列表。
;;
;; 描述
;; ----
;; 该函数会沿着当前 *load-path* 推导关联的 `tests` 根目录，
;; 查找 `function-library-index.json`，再将多个索引合并为一个结果。

(define (contains-function-index-path? paths)
  (let loop ((remaining paths))
    (and (not (null? remaining))
         (or (and (path-file? (car remaining))
                  (string=? (path-name (car remaining)) "function-library-index.json"))
             (loop (cdr remaining))
         ) ;or
    ) ;and
  ) ;let
) ;define

(check (index-entry->library-query "(liii string)") => "liii/string")
(check (index-entry->library-query "(scheme char)") => "scheme/char")
(check (index-entry->library-query "(bad)") => #f)
(check (index-entry->library-query 1) => #f)

(let ((index-paths (find-function-index-paths)))
  (check-true (pair? index-paths))
  (check-true (contains-function-index-path? index-paths))
) ;let

(let ((index (load-function-index)))
  (check (cdr (assoc "string-split" index)) => '("(liii string)"))
  (check (cdr (assoc "+" index)) => '("(liii base)"))
  (check (cdr (assoc "char-ci=?" index)) => '("(scheme char)"))
  (check (cdr (assoc "option=?" index)) => '("(liii option)"))
  (check (cdr (assoc "alist->fxmapping" index)) => '("(liii fxmapping)"))
) ;let

(check-report)
