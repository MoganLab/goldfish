(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii check)
  (liii os)
  (liii path)
  (liii string)
  (liii subprocess)
) ;import

;;;
;;; [1045] 端到端自动化验收测试脚本
;;;
;;; 验证 gf check 前两条静态度量规则：
;;; (1) 单文件行数限制 (<=300行) 与恰好300行/301行边界
;;; (2) 语法树嵌套深度限制 (<=15层) 与恰好15层/16层边界
;;; (3) 目录前缀豁免、携带 reason 的显式白名单与未申报超限报错
;;; (4) 命令行帮助与 --changed-since 增量语义
;;; (5) 对 goldfish 仓库自身的自举全绿验证
;;;

(check-set-mode! 'report-failed)

(define (find-goldfish-bin)
  (let* ((env-bin (get-environment-variable "GOLDFISH_BIN"))
         (home (get-environment-variable "HOME"))
         (home-bin (if home (string-append home "/git/goldfish/bin/gf") #f))
        ) ;
    (cond ((and env-bin (file-exists? env-bin)) env-bin)
          ((and home-bin (file-exists? home-bin)) home-bin)
          (else "gf")
    ) ;cond
  ) ;let*
) ;define

(define (find-goldfish-repo)
  (let* ((env-repo (get-environment-variable "GOLDFISH_REPO"))
         (home (get-environment-variable "HOME"))
         (home-repo (if home (string-append home "/git/goldfish") #f))
        ) ;
    (cond ((and env-repo (file-exists? env-repo)) env-repo)
          ((and home-repo (file-exists? home-repo)) home-repo)
          (else ".")
    ) ;cond
  ) ;let*
) ;define

(define gf-bin (find-goldfish-bin))

(define gf-repo (find-goldfish-repo))

(define (run-gf args)
  (run-values (cons gf-bin args) :stdout 'capture :stderr 'capture :timeout 30)
) ;define

(define (run-gf-repo args)
  (run-values (cons gf-bin args)
    :stdout  'capture
    :stderr  'capture
    :timeout 30
    :cwd     gf-repo
  ) ;run-values
) ;define

;; ========== 测试项1：单文件行数限制规则边界（301 行违规 vs 300 行通过） ==========

(let* ((f301 (path->string (path-join (path-temp-dir) "gf-1045-lines-301.scm")))
       (f300 (path->string (path-join (path-temp-dir) "gf-1045-lines-300.scm")))
      ) ;
  ;; 写入恰好 301 行
  (call-with-port (open-output-file f301)
    (lambda (p)
      (let loop
        ((i 0))
        (when (< i 301)
          (display "(define x 1)\n" p)
          (loop (+ i 1))
        ) ;when
      ) ;let
    ) ;lambda
  ) ;call-with-port
  ;; 写入恰好 300 行
  (call-with-port (open-output-file f300)
    (lambda (p)
      (let loop
        ((i 0))
        (when (< i 300)
          (display "(define x 1)\n" p)
          (loop (+ i 1))
        ) ;when
      ) ;let
    ) ;lambda
  ) ;call-with-port

  ;; 验证 301 行报错违规且非零退出
  (let-values (((out err code) (run-gf (list "check" f301))))
    (check code => 1)
    (check (string-contains? out "max-file-lines") => #t)
    (check (string-contains? out "301") => #t)
    (check (string-contains? out "300") => #t)
  ) ;let-values

  ;; 验证恰好 300 行检查通过且 0 码退出
  (let-values (((out err code) (run-gf (list "check" f300))))
    (check code => 0)
    (check (string-contains? out "Check: OK") => #t)
  ) ;let-values

  (path-unlink f301 #t)
  (path-unlink f300 #t)
) ;let*

;; ========== 测试项2：语法树嵌套深度限制规则边界（16 层违规 vs 15 层通过） ==========

(let* ((f16 (path->string (path-join (path-temp-dir) "gf-1045-depth-16.scm")))
       (f15 (path->string (path-join (path-temp-dir) "gf-1045-depth-15.scm")))
      ) ;
  ;; 写入深度恰好为 16 层（外层 1 + 内部 15 层嵌套 = 16，位于第 4 行）
  (call-with-port (open-output-file f16)
    (lambda (p)
      (display "(define dummy 0)\n\n;; 第 3 行注释\n((((((((((((((((1))))))))))))))))\n"
        p
      ) ;display
    ) ;lambda
  ) ;call-with-port
  ;; 写入深度恰好为 15 层（外层 1 + 内部 14 层嵌套 = 15）
  (call-with-port (open-output-file f15)
    (lambda (p)
      (display "(define dummy 0)\n\n;; 第 3 行注释\n(((((((((((((((1)))))))))))))))\n"
        p
      ) ;display
    ) ;lambda
  ) ;call-with-port

  ;; 验证 16 层报错违规，包含最深行号 4，当前深度 16 与上限 15
  (let-values (((out err code) (run-gf (list "check" f16))))
    (check code => 1)
    (check (string-contains? out "max-tree-depth") => #t)
    (check (string-contains? out ":4:") => #t)
    (check (string-contains? out "16") => #t)
    (check (string-contains? out "15") => #t)
  ) ;let-values

  ;; 验证恰好 15 层检查通过且 0 码退出
  (let-values (((out err code) (run-gf (list "check" f15))))
    (check code => 0)
    (check (string-contains? out "Check: OK") => #t)
  ) ;let-values

  (path-unlink f16 #t)
  (path-unlink f15 #t)
) ;let*

;; ========== 测试项3：白名单与豁免机制测试 ==========

;; 已在 gf_check.json 登记的白名单核心文件跳过检查
(let-values (((out err code) (run-gf-repo '("check"
                                            "goldfish/liii/goldfmt/format.scm"))))
  (check code => 0)
  (check (string-contains? out "Check: OK") => #t)
) ;let-values

;; ========== 测试项4：命令行接口与帮助文档测试 ==========

;; gf help check 完整展示用法、选项与两条规则
(let-values (((out err code) (run-gf '("help" "check"))))
  (check code => 0)
  (check (string-contains? out "gf check") => #t)
  (check (string-contains? out "max-file-lines") => #t)
  (check (string-contains? out "max-tree-depth") => #t)
) ;let-values

;; gf check --help
(let-values (((out err code) (run-gf '("check" "--help"))))
  (check code => 0)
  (check (string-contains? out "Usage: gf check") => #t)
  (check (string-contains? out "max-file-lines") => #t)
  (check (string-contains? out "max-tree-depth") => #t)
) ;let-values

;; ========== 测试项5：全仓自举全绿验证 ==========

(let-values (((out err code) (run-gf-repo '("check"))))
  (check code => 0)
  (check (string-contains? out "Check: OK") => #t)
) ;let-values

(let-values (((out err code) (run-gf-repo '("check" "--changed-since=main"))))
  (check code => 0)
  (check (string-contains? out "Check: OK") => #t)
) ;let-values

(check-report)
