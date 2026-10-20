(import (liii check)
        (liii string)
        (liii path)
        (liii os)
        (liii pi tools types)
        (liii pi compass project)
        (liii pi compass store)
        (liii pi compass tools)
) ;import

(check-set-mode! 'report-failed)

(define dir (path-join (path-temp-dir) "gf-compass-map-tool-test"))
(define dir-str (path->string dir))
(define codemap-file (pi-compass-default-codemap-path dir-str))
(define src-a (path->string (path-join dir "src" "a.js")))
(define src-b (path->string (path-join dir "src" "b.js")))
(define readme (path->string (path-join dir "README.md")))

(define (cleanup!)
  (for-each
    (lambda (f) (when (path-file? (path f)) (path-unlink (path f) #t)))
    (list codemap-file src-a src-b readme)
  ) ;for-each
  (for-each
    (lambda (d) (when (path-dir? (path d)) (path-rmdir (path d))))
    (list (path->string (path-join dir ".pi"))
      (path->string (path-join dir "src"))
      dir-str
    ) ;list
  ) ;for-each
) ;define

(define (res-text res)
  (cdr (assoc 'text (vector-ref (pi-tool-result-content res) 0)))
) ;define

(define (setup!)
  (cleanup!)
  (path-mkdir dir :parents #t :exist-ok #t)
  (path-mkdir (path (path->string (path-join dir "src"))) :parents #t :exist-ok #t)
  (path-write-text (path src-a) "// a\n")
  (path-write-text (path readme) "# demo\n")
) ;define

(dynamic-wind (lambda () #f)
  (lambda ()
    (setup!)
    (let ((orig (getcwd)))
      (dynamic-wind (lambda () (chdir dir-str))
        (lambda ()
          ;; codebase_map：返回 Markdown 并顺带落盘缓存。
          (let ((res (pi-execute-codebase-map '())))
            (check (pi-tool-result? res) => #t)
            (check (pi-tool-result-is-error res) => #f)
            (check (string-starts? (res-text res)
                     "## Codebase Map:"
                   ) ;string-starts?
              =>
              #t
            ) ;check
            (check (path-file? (path codemap-file)) => #t)
          ) ;let

          ;; code_tour 无 topic：主题列表。
          (let ((res (pi-execute-code-tour '())))
            (check (pi-tool-result-is-error res) => #f)
            (check (string-starts? (res-text res)
                     "Available tour topics: "
                   ) ;string-starts?
              =>
              #t
            ) ;check
          ) ;let

          ;; code_tour 有 topic：tour Markdown。
          (let ((res (pi-execute-code-tour '(("topic" . "src")))))
            (check (string-starts? (res-text res)
                     "## Code Tour: src"
                   ) ;string-starts?
              =>
              #t
            ) ;check
            (check (string-contains (res-text res) "src/a.js")
              =>
              #t
            ) ;check
          ) ;let

          ;; 未知 topic 降级为可读文案（非错误）。
          (let ((res (pi-execute-code-tour '(("topic" . "nope")))))
            (check (pi-tool-result-is-error res) => #f)
            (check (res-text res)
              =>
              "No files found for topic \"nope\"."
            ) ;check
          ) ;let

          ;; codebase_map 刷新缓存：新增深度 2 文件后内容出现。
          (path-write-text (path src-b) "// b\n")
          (let ((res (pi-execute-codebase-map '())))
            (check (string-contains (res-text res) "b.js")
              =>
              #t
            ) ;check
          ) ;let
        ) ;lambda
        (lambda () (chdir orig))
      ) ;dynamic-wind
    ) ;let
  ) ;lambda
  (lambda () (cleanup!))
) ;dynamic-wind

;; 工具定义形态。
(check (pi-tool-definition-name (pi-codebase-map-definition)) => "codebase_map")
(check (pi-tool-definition-name (pi-code-tour-definition)) => "code_tour")
(check (pair? (pi-tool-definition-guidelines (pi-codebase-map-definition)))
  =>
  #t
) ;check

(check-report)
