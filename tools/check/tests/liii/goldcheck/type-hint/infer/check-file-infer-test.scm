;; check-file-infer
;; 按文件 import 依赖的 hint 检查任意 define-library 源码文件的函数体调用。
;;
;; 语法
;; ----
;; (check-file-infer file)
;;
;; 参数
;; ----
;; file : string  源码文件路径
;;
;; 返回值
;; ------
;; list of violation
;; 检测到的类型冲突违规列表；无冲突返回空列表 '()。
;;
;; 说明
;; ----
;; hint 库自身与已有对应 hint 的原库跳过（由 check-infer 覆盖）；
;; 非 define-library 文件返回 '()。

(import (scheme base)
  (liii check)
  (liii string)
  (liii os)
  (liii path)
  (liii goldcheck type-hint infer)
) ;import

(check-set-mode! 'report-failed)

(define (resource-file rel)
  (let ((local (string-append "tests/resources/type-hint/" rel))
        (alt (string-append "tools/check/tests/resources/type-hint/" rel))
       ) ;
    (if (access local 'R_OK) local alt)
  ) ;let
) ;define

;; 仓库根：测试子进程 chdir 到 tools/check，需向上探测
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

;; 1. 非 define-library 文件返回 '()
(check (check-file-infer (resource-file "script.scm")) => '())

;; 2. hint 库自身跳过（由 check-infer 负责，避免重复报告）
(check (check-file-infer (resource-file "goldfish/hint/liii/clean.scm")) => '())

;; 3. 已有对应 hint 的原库跳过（如 goldfish/liii/string.scm 对应 (hint liii string)）
(let ((repo-file
        (string-append (find-repo-root) "/goldfish/liii/string.scm")
      ) ;repo-file
     ) ;
  (check (check-file-infer repo-file) => '())
) ;let

;; 4. 普通源码文件：按 import 的 hint 检查
;;    filedemo.scm import (scheme base)，调用 (< n 10) 时
;;    n 为 string->number 的结果 (or #f number?)，未守卫 -> 确定冲突
(let ((viols (check-file-infer (resource-file "goldfish/liii/filedemo.scm"))))
  (check (length viols) => 1)
  (check-true (string-contains? (cadddr (car viols)) "argument 1 of <"))
) ;let

(check-report)
