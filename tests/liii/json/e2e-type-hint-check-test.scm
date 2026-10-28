(import (scheme base)
  (liii check)
  (liii os)
  (liii path)
  (liii string)
  (liii subprocess run)
) ;import

;; e2e：gf check --only type-hint 的端到端检查
;;
;; 场景 1（真实仓库）：在仓库根运行 gf check --only type-hint，
;;   所有 hint 库通过检查，(liii json) 等库出现在状态列表中，
;;   没有任何 type-hint 违规，退出状态为 0。
;;
;; 场景 2（签名归属违规）：在 e2e fixture 项目中运行同一条命令，
;;   再导出的过程在再导出库的 hint 中声明签名时，检查报告归属违规，
;;   并以非零状态退出。
;;
;; fixture 位于 tests/liii/json/resources/type-hint-e2e/ 下，
;; 其中 gfproject.json 使 gfproject-root 指向 fixture 目录，
;; gf_check.json 启用 type-hint 规则。

(check-set-mode! 'report-failed)

;; 仓库根：从当前目录向上找 goldfish/ 与 gfproject.json

(define (find-repo-root)
  (let loop
    ((dir (getcwd)))
    (if
      (and (path-dir? (path (string-append dir "/goldfish")))
        (path-file? (path (string-append dir "/gfproject.json")))
      ) ;and
      dir
      (let ((parent (path->string (path-parent (path dir)))))
        (if (or (string=? parent dir) (string=? parent "/")) "." (loop parent))
      ) ;let
    ) ;if
  ) ;let
) ;define

;; e2e fixture 项目目录

(define (fixture-dir)
  (string-append (find-repo-root) "/tests/liii/json/resources/type-hint-e2e")
) ;define

;; 在 dir 中运行 gf check --only type-hint，返回 (退出状态 输出)

(define (run-gf-check dir)
  (let-values (((out err code)
                 (%run-values
                   (list (string-append (find-repo-root) "/bin/gf")
                         "check" "--only" "type-hint"
                   ) ;list
                   :cwd dir
                 ) ;%run-values
               ) ;binding
             ) ;bindings
    (list code (string-append out err))
  ) ;let-values
) ;define

;; --------------------------------------------------------------------
;; 场景 1：真实仓库的类型检查通过
;; --------------------------------------------------------------------
(let ((result (run-gf-check (find-repo-root))))
  (check (car result) => 0)
  (check-true (string-contains? (cadr result) "(liii json)"))
  (check-true (not (string-contains? (cadr result) "type-hint:")))
  (check-true (not (string-contains? (cadr result) "violation")))
) ;let

;; --------------------------------------------------------------------
;; 场景 2：签名归属违规被检出
;; --------------------------------------------------------------------
(let ((result (run-gf-check (fixture-dir))))
  (check-true (not (= (car result) 0)))
  (check-true (string-contains? (cadr result) "e2e-shared-fn"))
  (check-true (string-contains? (cadr result) "signature belongs to (srfi srfi-1)"))
) ;let

(check-report)
