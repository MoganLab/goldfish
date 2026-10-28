(import (scheme base)
  (liii check)
  (liii string)
  (liii os)
  (liii path)
  (liii goldcheck type-hint)
) ;import

;; A7 校验：hint 库中签名声明的归属唯一性
;;
;; 语法
;; ----
;; (check-type-hint file-path)
;; (check-type-hint-duplicates hint-file-list)
;;
;; 参数
;; ----
;; file-path     : string  goldfish/hint/ 下的 hint 库文件路径
;; hint-file-list: list    多个 hint 库文件路径
;;
;; 返回值
;; ------
;; list of violation
;; 每项形如 (list 'type-hint rel-path line msg)。
;;
;; 校验项：
;; A7a 归属正确性：再导出的过程（在原库中无本地 define，实现在依赖库中）
;;     不得在再导出库的 hint 中本地声明；签名应声明在实现库的 hint 中。
;;     实现库没有对应 hint 库时放行（fallback 声明）。
;; A7b 库内重复声明：同一 hint 变量被多次 define；同一函数名在查询函数中
;;     被重复登记。
;; A7c 跨库重复声明：同一函数名在多个 hint 库中被声明。
;;
;; fixture 位于 tools/check/tests/resources/type-hint/ 下。

(check-set-mode! 'report-failed)

;; resources 解析：bin/gf test 会 chdir 到 tools/check，兼容两种 cwd

(define (resource-file rel)
  (let ((local (string-append "tests/resources/type-hint/" rel))
        (alt (string-append "tools/check/tests/resources/type-hint/" rel))
       ) ;
    (if (access local 'R_OK) local alt)
  ) ;let
) ;define

;; --------------------------------------------------------------------
;; 1. A7a 归属正确性
;; --------------------------------------------------------------------
;; 1a. dep-fn 实现在 (liii a7dep)，该库有 hint 库；
;;     (hint liii a7reexport) 不得声明 dep-fn -> 1 违规
(let ((viol (check-type-hint (resource-file "goldfish/hint/liii/a7reexport.scm"))))
  (check (length viol) => 1)
  (check (caar viol) => 'type-hint)
  (check-true (string-contains? (cadddr (car viol)) "dep-fn"))
  (check-true (string-contains? (cadddr (car viol)) "liii a7dep"))
) ;let

;; 1b. 实现库没有 hint 库时，允许在再导出库中声明 -> 无违规
(check (check-type-hint (resource-file "goldfish/hint/liii/a7fallback.scm"))
  =>
  '()
) ;check

;; 1c. 内置风格的过程（任何库都没有本地 define）-> 无违规
(check (check-type-hint (resource-file "goldfish/hint/liii/a7builtin.scm"))
  =>
  '()
) ;check

;; --------------------------------------------------------------------
;; 2. A7b 库内重复声明
;; --------------------------------------------------------------------
;; 2a. 同一 hint 变量被 define 两次 -> 1 违规
(let ((viol (check-type-hint (resource-file "goldfish/hint/liii/a7dupdef.scm"))))
  (check (length viol) => 1)
  (check-true (string-contains? (cadddr (car viol)) "hint-f1"))
) ;let

;; 2b. 同一函数名在查询函数中登记两次 -> 1 违规
(let ((viol (check-type-hint (resource-file "goldfish/hint/liii/a7dupreg.scm"))))
  (check (length viol) => 1)
  (check-true (string-contains? (cadddr (car viol)) "f1"))
) ;let

;; --------------------------------------------------------------------
;; 3. A7c 跨库重复声明
;; --------------------------------------------------------------------
(let ((viol (check-type-hint-duplicates
               (list (resource-file "goldfish/hint/liii/a7dupx.scm")
                     (resource-file "goldfish/hint/liii/a7dupy.scm")
               ) ;list
             ) ;viol
           ) ;
      ) ;
  (check (length viol) => 2)
  (check-true (string-contains? (cadddr (car viol)) "dup-fn"))
) ;let

;; 同一库内的重复不计入跨库重复
(check (check-type-hint-duplicates
         (list (resource-file "goldfish/hint/liii/a7dupdef.scm"))
       )
  =>
  '()
) ;check

(check-report)
