(import (liii check) (liii os) (liii path) (liii string) (liii sys))

(check-set-mode! 'report-failed)

;; gf doc 缺少函数索引时的提示
;; 当单参数查询同时可能是函数名时，如果没有任何可见的
;; `function-library-index.json`，应提示执行 `gf doc --build-json`，
;; 而不是误报 library not found。
;;
;; 注意：测试运行目录可能是项目根（手动）或 tools/doc（测试 runner
;; 切目录），索引路径据此定位；所有可见索引文件都要隔离（doc 自身
;; 与 tools/common 各一份），否则子进程仍能看见索引，走不到 hint。

(define (run-shell-command command)
  (os-call (string-append "sh -c \"" command "\""))
) ;define

(define (candidate-index-paths)
  ;; 运行目录可能是项目根（手动）或 tools/doc（测试 runner 切目录）；
  ;; 收集两种布局下真实存在的索引文件（`..` 多段路径不可靠，见 path 实现，
  ;; 这里用 path-parent 逐级枚举）。
  (let* ((cwd (path (getcwd)))
         (up1 (path-parent cwd))
         (up2 (path-parent up1))
         (roots (list (getcwd) (path->string up1) (path->string up2))))
    (let loop ((rs roots) (acc '()))
      (if (null? rs)
        (reverse acc)
        (let ((paths (list (path-join (car rs) "tools" "doc" "tests" "function-library-index.json")
                       (path-join (car rs) "tools" "common" "tests" "function-library-index.json"))))
          (loop (cdr rs)
            (let inner ((ps paths) (a acc))
              (cond ((null? ps) a)
                ((path-file? (car ps)) (inner (cdr ps) (cons (car ps) a)))
                (else (inner (cdr ps) a))
              ) ;cond
            ) ;let
          ) ;let
        ) ;if
      ) ;let
    ) ;let
  ) ;let
) ;define

(when (not (os-windows?))
  (let* ((index-paths (candidate-index-paths))
         ;; 仅隔离真实存在的文件；逐个备份文本，测后原样恢复。
         (saved (let loop ((paths index-paths) (acc '()))
                  (cond ((null? paths) (reverse acc))
                    ((path-file? (car paths))
                     (loop (cdr paths)
                       (cons (cons (car paths) (path-read-text (car paths))) acc)))
                    (else (loop (cdr paths) acc))
                  ) ;cond
                ) ;let
         )
         (output-path (path-join (path-temp-dir)
                        (string-append "golddoc-missing-index-" (number->string (getpid)) ".log")
                      ) ;path-join
         ) ;output-path
        ) ;
    (path-unlink output-path #t)
    (dynamic-wind (lambda () (for-each (lambda (entry) (path-unlink (car entry) #t)) saved))
      (lambda ()
        (run-shell-command (string-append (executable)
                             " doc 'alist->fxmapping/combinator' > "
                             (path->string output-path)
                             " 2>&1"
                           ) ;string-append
        ) ;run-shell-command
        (let ((output (path-read-text output-path)))
          (check-true (string-contains? output
                        "Error: function index not found for query: alist->fxmapping/combinator"
                      ) ;string-contains?
          ) ;check-true
          (check-true (string-contains? output
                        "Hint: run `gf doc --build-json` to build function index."
                      ) ;string-contains?
          ) ;check-true
        ) ;let
      ) ;lambda
      (lambda ()
        (path-unlink output-path #t)
        (for-each (lambda (entry) (path-write-text (car entry) (cdr entry))) saved)
      ) ;lambda
    ) ;dynamic-wind
  ) ;let*
) ;when

(check-report)
