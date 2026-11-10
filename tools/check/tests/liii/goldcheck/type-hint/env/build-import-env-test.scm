;; build-import-env
;; 由源码文件的 import 列表构建类型环境（依赖穿透 + scheme base 内建层）。
;;
;; 语法
;; ----
;; (build-import-env imports)
;;
;; 参数
;; ----
;; imports : list of list-of-symbol  import 库名符号列表
;;
;; 返回值
;; ------
;; alist
;; 类型环境：公开绑定名 -> 签名列表。

(import (scheme base)
  (liii check)
  (liii os)
  (liii path)
  (liii goldcheck type-hint env)
) ;import

(check-set-mode! 'report-failed)

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

;; 1. (scheme base) 内建层：核心过程均有签名
(let ((env (build-import-env '((scheme base)))))
  (check-true (pair? (assq 'car env)))
  (check-true (pair? (assq 'string-append env)))
) ;let

;; 2. 依赖穿透：真实仓库库 (liii list) 再导出 srfi-1 的 every
(let* ((info (cached-library-info (string-append (find-repo-root) "/goldfish/liii/list.scm")))
       (env (build-import-env (cdr (assq 'imports info))))
      ) ;let*
  (check-true (pair? (assq 'every env)))
) ;let*

;; 3. 空导入仍有内建层
(let ((env (build-import-env '())))
  (check-true (pair? (assq 'car env)))
) ;let

(check-report)
