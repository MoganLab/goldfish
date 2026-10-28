(import (scheme base)
  (liii check)
  (liii string)
  (liii os)
  (liii path)
  (liii goldcheck type-hint)
  (hint scheme base)
  (hint scheme file)
  (hint srfi srfi-1)
  (hint srfi srfi-13)
  (hint liii error)
  (hint liii string)
  (hint liii path)
  (hint liii json)
  (hint liii goldcheck path)
  (hint liii goldfmt format writer)
) ;import

;; check-type-hint
;; 校验 hint 库的结构合法性，并与原库交叉校验（逐函数粒度）。
;;
;; 语法
;; ----
;; (check-type-hint file-path)
;;
;; 参数
;; ----
;; file-path : string  goldfish/hint/ 下的 hint 库文件路径
;;
;; 返回值
;; ------
;; list of violation
;; 每项形如 (list 'type-hint rel-path line msg)。
;;
;; 校验项：
;; A1 库名与文件路径对应（goldfish/hint/liii/clean.scm <=> (hint liii clean)）
;; A2 必须导出约定命名的标准查询函数 hint-liii/clean
;; A3 不得导入其他 hint 库
;; A4 签名语法合法：'(T1 ... Tn -> R)，-> 恰好一个、返回类型一个，
;;    类型为符号或 (or #f T ...)
;; A5 查询函数 case 分支与 hint 变量定义一一对应
;; A6 同一函数不得有完全相同的重复签名
;; B2 签名参数个数与原库定义一致（逐函数粒度）：实现是 lambda /
;;    case-lambda 才检查；rest 参数、绑定外部符号（如 g_every）、
;;    再导出或 s7 内置直通（无本地 define）均跳过
;;
;; B1（登记非导出名字）与 B3（登记再导出过程）需要 hint-ref 来源
;; 追踪才能准确区分再导出与内置直通，不在初版范围。
;; 原库导出函数没有 hint 时跳过该函数，不报缺失（逐函数，非整库）。
;;
;; fixture 位于 tools/check/tests/resources/type-hint/ 下，
;; 其中 goldfish/hint/ 与 goldfish/ 的目录结构对应原库推导规则。

(check-set-mode! 'report-failed)

;; resources 解析：bin/gf test 会 chdir 到 tools/check，兼容两种 cwd

(define (resource-file rel)
  (let ((local (string-append "tests/resources/type-hint/" rel))
        (alt (string-append "tools/check/tests/resources/type-hint/" rel))
       ) ;
    (if (access local 'R_OK) local alt)
  ) ;let
) ;define

;; 仓库根：从当前目录向上找 goldfish/ 与 gf_check.json
;; （bin/gf test 子进程中 g_project-root 未必可用）

(define (find-repo-root)
  (let loop
    ((dir (getcwd)))
    (if
      (and (path-dir? (path (string-append dir "/goldfish")))
        (path-file? (path (string-append dir "/gf_check.json")))
      ) ;and
      dir
      (let ((parent (path->string (path-parent (path dir)))))
        (if (or (string=? parent dir) (string=? parent "/")) "." (loop parent))
      ) ;let
    ) ;if
  ) ;let
) ;define

;; 仓库根下的真实文件路径

(define (repo-file rel)
  (string-append (find-repo-root) "/" rel)
) ;define

;; --------------------------------------------------------------------
;; 1. 干净：全标记 + 各形态合规（固定参数、case-lambda、再导出不登记）-> '()
;; --------------------------------------------------------------------
(check (check-type-hint (resource-file "goldfish/hint/liii/clean.scm")) => '())

;; 逐库状态：部分/全量覆盖，以及通过/失败。
(let ((lines (type-hint-status-lines (list (resource-file "goldfish/hint/liii/arity.scm")
                                       (resource-file "goldfish/hint/liii/clean.scm")
                                       (resource-file "goldfish/hint/liii/inferok.scm")
                                     ) ;list
               (check-type-hint (resource-file "goldfish/hint/liii/arity.scm"))
             ) ;type-hint-status-lines
      ) ;lines
     ) ;
  (check lines
    =>
    '("  [FULL FAIL\t] (liii arity)"
      "  [PARTIAL PASS\t] (liii clean)"
      "  [FULL PASS\t] (liii inferok)")
  ) ;check
) ;let

;; --------------------------------------------------------------------
;; 2. 真实仓库的 hint 库 -> 全部通过（当前基线）
;; --------------------------------------------------------------------
(for-each
  (lambda (f) (check (check-type-hint (repo-file f)) => '()))
  '("goldfish/hint/liii/alist.scm" "goldfish/hint/liii/error.scm"
    "goldfish/hint/liii/gfproject/config.scm" "goldfish/hint/scheme/base.scm"
    "goldfish/hint/srfi/srfi-1.scm"
    "tools/check/hint/liii/goldcheck/no-glue.scm"
    "goldfish/hint/scheme/file.scm" "tools/check/hint/liii/goldcheck/path.scm"
    "goldfish/hint/liii/string.scm" "goldfish/hint/liii/path.scm"
    "goldfish/hint/liii/json.scm" "goldfish/hint/srfi/srfi-13.scm"
    "goldfish/hint/liii/goldfmt/format.scm"
    "goldfish/hint/liii/goldfmt/format/writer.scm"
    "goldfish/hint/liii/base.scm")
) ;for-each

;; timestamp-iso 已修复：string->number 的结果先校验再参与乘法。
(check (check-type-hint (repo-file "goldfish/hint/liii/date-time/internal/timestamp-iso.scm")
       ) ;check-type-hint
  =>
  '()
) ;check

;; 未校验可能为 #f 的数值结果：确定的类型冲突要报告
(let ((viol (check-type-hint (resource-file "goldfish/hint/liii/numcheck.scm"))))
  (check (length viol) => 1)
  (check-true (string-contains? (cadddr (car viol)) "argument 2 of my-mul"))
  (check-true (string-contains? (cadddr (car viol)) "(or #f number?)"))
) ;let

(check (type-hint-status-lines (list (repo-file "goldfish/hint/liii/gfproject/config.scm")
                                 (repo-file "tools/check/hint/liii/goldcheck/path.scm")
                                 (repo-file "goldfish/hint/liii/alist.scm")
                                 (repo-file "goldfish/hint/liii/goldfmt/format.scm")
                               ) ;list
         '()
       ) ;type-hint-status-lines
  =>
  '("  [FULL PASS\t] (liii gfproject config)"
    "  [FULL PASS\t] (liii goldcheck path)" "  [FULL PASS\t] (liii alist)"
    "  [FULL PASS\t] (liii goldfmt format)")
) ;check

;; no-glue 全量部分检查涉及的外部过程均已登记 hint。
(for-each
  (lambda (name) (check-true (pair? (hint-scheme/base name))))
  '(car read open-input-string catch eof-object null? pair? pair-line-number
     symbol? symbol->string eq? eqv? equal? assq cons list cdr reverse cadr
     assoc caar append map cddr cadddr list-ref + dynamic-wind eof-object? caddr
     close-input-port string=? = not apply cadar values error)
) ;for-each
(check-true (pair? (hint-scheme/file 'open-input-file)))
(check-true (pair? (hint-srfi/srfi-1 'filter)))
(check-true (pair? (hint-liii/goldcheck/path 'relativize-path)))
(check-true (pair? (hint-liii/string 'string-ends?)))
(check-true (pair? (hint-liii/string 'string-contains?)))
(check-true (pair? (hint-liii/string 'string-split)))
(check-true (pair? (hint-liii/string 'string-replace)))
(check-true (pair? (hint-liii/path 'path->string)))
(check-true (pair? (hint-liii/path 'path-join)))
(check-true (pair? (hint-liii/path 'path-file?)))
(check-true (pair? (hint-liii/path 'path-read-text)))
(check-true (pair? (hint-liii/json 'string->json)))
(check-true (pair? (hint-liii/json 'json-object?)))
(check-true (pair? (hint-liii/json 'json-ref)))
(check-true (pair? (hint-liii/json 'json-set)))
(check-true (pair? (hint-liii/error 'type-error)))

;; goldfmt format 全量检查涉及的公开依赖签名。
(for-each
  (lambda (name) (check-true (pair? (hint-scheme/base name))))
  '(string-append string-length substring string-ref vector-ref vector-length
     number? char=? > >= < <= - zero? call-with-values)
) ;for-each
(check-true (pair? (hint-srfi/srfi-13 'string-join)))
(check-true (pair? (hint-liii/goldfmt/format/writer 'format-inline)))
(check-true (pair? (hint-liii/goldfmt/format/writer 'can-inline?)))

;; --------------------------------------------------------------------
;; 3. A1 库名与路径不符：wrongname.scm 内是 (hint liii other) -> 1 违规
;; --------------------------------------------------------------------
(let ((viol (check-type-hint (resource-file "goldfish/hint/liii/wrongname.scm"))))
  (check (length viol) => 1)
  (check (caar viol) => 'type-hint)
  (check-true (string-ends? (cadr (car viol)) "/wrongname.scm"))
  (check (caddr (car viol)) => 1)
  (check-true (string-contains? (cadddr (car viol)) "hint liii other"))
) ;let

;; --------------------------------------------------------------------
;; 4. A2 未导出约定查询函数 hint-liii/noquery -> 1 违规
;; --------------------------------------------------------------------
(let ((viol (check-type-hint (resource-file "goldfish/hint/liii/noquery.scm"))))
  (check (length viol) => 1)
  (check-true (string-contains? (cadddr (car viol)) "hint-liii/noquery"))
) ;let

;; --------------------------------------------------------------------
;; 5. A3 导入其他 hint 库 -> 1 违规
;; --------------------------------------------------------------------
(let ((viol (check-type-hint (resource-file "goldfish/hint/liii/importhint.scm"))))
  (check (length viol) => 1)
  (check-true (string-contains? (cadddr (car viol)) "hint scheme base"))
) ;let

;; --------------------------------------------------------------------
;; 6. A4 签名语法：缺 ->、双返回类型、非符号类型 -> 3 违规
;; --------------------------------------------------------------------
(let ((viol (check-type-hint (resource-file "goldfish/hint/liii/badsig.scm"))))
  (check (length viol) => 3)
  (check-true (string-contains? (cadddr (car viol)) "f1"))
  (check-true (string-contains? (cadddr (cadr viol)) "f2"))
  (check-true (string-contains? (cadddr (caddr viol)) "f3"))
) ;let

;; --------------------------------------------------------------------
;; 7. A5 case 分支与变量不一致
;; --------------------------------------------------------------------
;; 7a. 定义了 hint-f2 但 case 未登记 -> 1 违规
(let ((viol (check-type-hint (resource-file "goldfish/hint/liii/unreg.scm"))))
  (check (length viol) => 1)
  (check-true (string-contains? (cadddr (car viol)) "hint-f2"))
) ;let

;; 7b. case 登记了 f2 但 hint-f2 未定义 -> 1 违规
(let ((viol (check-type-hint (resource-file "goldfish/hint/liii/unbound.scm"))))
  (check (length viol) => 1)
  (check-true (string-contains? (cadddr (car viol)) "hint-f2"))
) ;let

;; --------------------------------------------------------------------
;; 8. A6 完全相同的重复签名 -> 1 违规
;; --------------------------------------------------------------------
(let ((viol (check-type-hint (resource-file "goldfish/hint/liii/dupsig.scm"))))
  (check (length viol) => 1)
  (check-true (string-contains? (cadddr (car viol)) "f1"))
) ;let

;; --------------------------------------------------------------------
;; 9. B 类逐函数粒度：f2 未标记 -> 跳过 f2 不报缺失，f1 仍被检查，'()
;; --------------------------------------------------------------------
(check (check-type-hint (resource-file "goldfish/hint/liii/partial.scm"))
  =>
  '()
) ;check

;; 9b. f1 参数数量错误 + f2 无 hint -> 只报 f1 的违规（逐函数，不整库跳过）
(let ((viol (check-type-hint (resource-file "goldfish/hint/liii/partial2.scm"))))
  (check (length viol) => 1)
  (check-true (string-contains? (cadddr (car viol)) "f1"))
) ;let

;; --------------------------------------------------------------------
;; 10. B2 参数数量
;; --------------------------------------------------------------------
;; 10a+10b. 固定参数与 case-lambda 数量不符 -> 2 违规
(let ((viol (check-type-hint (resource-file "goldfish/hint/liii/arity.scm"))))
  (check (length viol) => 2)
  (check-true (string-contains? (cadddr (car viol)) "f1"))
  (check-true (string-contains? (cadddr (cadr viol)) "f2"))
) ;let

;; 10c+10d. rest 参数与绑定非 lambda -> 保守跳过，'()
(check (check-type-hint (resource-file "goldfish/hint/liii/restskip.scm"))
  =>
  '()
) ;check

;; --------------------------------------------------------------------
;; 12. 有 hint 的函数自身做部分类型检查（spec 8.4 最小子集）：
;;     根据 import 依赖的 hint 检查确定的调用实参类型冲突；
;;     不递归检查没有 hint 的本地依赖函数。
;; --------------------------------------------------------------------
;; 12a. (cdr (assoc k b))：assoc 返回 (or #f pair?)，cdr 需要 pair? -> 1 违规
(let ((viol (check-type-hint (resource-file "goldfish/hint/liii/infercheck.scm"))))
  (check (length viol) => 1)
  (check (caar viol) => 'type-hint)
  (check-true (string-contains? (cadddr (car viol)) "cdr"))
  (check-true (string-contains? (cadddr (car viol)) "pair?"))
) ;let

;; 12b. good1/good2 均有 hint，覆盖全部导出函数：
;;      内部函数 internal-bad 的确定冲突也应报告。
(let ((viol (check-type-hint (resource-file "goldfish/hint/liii/inferok.scm"))))
  (check (length viol) => 1)
  (check-true (string-contains? (cadddr (car viol)) "my-cdr"))
) ;let

;; 12c. tools/check 的局部 hint 根缺少基础签名时，回退到 goldfish/hint
(let ((viol (check-type-hint (resource-file "tools/check/hint/liii/base-fallback.scm"))
      ) ;viol
     ) ;
  (check (length viol) => 1)
  (check-true (string-contains? (cadddr (car viol)) "cadr"))
) ;let

;; --------------------------------------------------------------------
;; 13. 类型收窄：and / if / cond 的守卫让 (or #f T) 收窄为 T
;; --------------------------------------------------------------------
;; 13a. 有守卫的调用不报；无守卫的 unguarded 仍报 1 条
(let ((viol (check-type-hint (resource-file "goldfish/hint/liii/narrow.scm"))))
  (check (length viol) => 1)
  (check-true (string-contains? (cadddr (car viol)) "my-cdr"))
  (check-true (string-contains? (cadddr (car viol)) "pair?"))
) ;let

;; --------------------------------------------------------------------
;; 14. 非 define-library 文件 -> '()
;; --------------------------------------------------------------------
(check (check-type-hint (resource-file "script.scm")) => '())

(check-report)
