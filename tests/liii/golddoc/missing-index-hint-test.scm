;; 添加 tools/golddoc 到 load path，以便导入 (liii golddoc)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path* (cons "tools/golddoc" *load-path*))

(import (liii check)
        (liii os)
        (liii path)
        (liii string)
        (liii sys)
) ;import

(check-set-mode! 'report-failed)

;; gf doc 缺少函数索引时的提示
;; 当单参数查询同时可能是函数名时，如果当前测试根目录下没有
;; `function-library-index.json`，应提示执行 `gf doc --build-json`，
;; 而不是误报 library not found。

(define (run-shell-command command)
  (os-call (string-append "sh -c \"" command "\""))
) ;define

(when (not (os-windows?))
  (let*
    ((index-path (path-join "tests" "function-library-index.json"))
     (output-path
       (path-join (path-temp-dir)
                  (string-append "golddoc-missing-index-"
                                 (number->string (getpid))
                                 ".log")
                  ) ;string-append
     ) ;output-path
     (saved-index-text (and (path-file? index-path)
                            (path-read-text index-path))
     ) ;saved-index-text
    ) ;
    (path-unlink output-path #t)
    (dynamic-wind
      (lambda ()
        (if saved-index-text
            (path-unlink index-path #t)
            #f
        ) ;if
      ) ;lambda
      (lambda ()
        (run-shell-command (string-append (executable)
                                          " doc 'alist->fxmapping/combinator' > "
                                          (path->string output-path)
                                          " 2>&1")
        ) ;run-shell-command
        (let ((output (path-read-text output-path)))
          (check-true
            (string-contains? output
                              "Error: function index not found for query: alist->fxmapping/combinator"
            ) ;string-contains?
          ) ;check-true
          (check-true
            (string-contains? output
                              "Hint: run `gf doc --build-json` to build function index."
            ) ;string-contains?
          ) ;check-true
        ) ;let
      ) ;lambda
      (lambda ()
        (path-unlink output-path #t)
        (if saved-index-text
            (path-write-text index-path saved-index-text)
            #f
        ) ;if
      ) ;lambda
    ) ;dynamic-wind
  ) ;let*
) ;when

(check-report)
