(set! *load-path* (cons "tools/fmt" *load-path*))
(set! *load-path* (cons "tools/common" *load-path*))

(import (liii check)
  (liii goldfmt)
  (liii cpp-fmt)
  (liii goldtool-changed)
  (liii list)
  (liii os)
  (liii path)
  (liii string)
) ;import

(check-set-mode! 'report-failed)

(define (contains? item xs)
  (if (member item xs) #t #f)
) ;define

(define (must command)
  (let ((status (os-call command)))
    (unless (zero? status)
      (error "Command failed" command)
    ) ;unless
  ) ;let
) ;define

;; 检测 clang-format 是否可用；不可用时（如未安装 clang-format 的 CI）相关 C++ 端到端断言跳过。

(define (clang-format-available?)
  (zero? (os-call (string-append (clang-format-binary) " --version")))
) ;define

(define (remove-tree target)
  (cond ((path-file? target) (path-unlink target #t))
        ((path-dir? target)
         (let ((entries (path-list-path target)))
           (let loop
             ((i 0))
             (if (< i (vector-length entries))
               (begin
                 (remove-tree (vector-ref entries i))
                 (loop (+ i 1))
               ) ;begin
               #t
             ) ;if
           ) ;let
         ) ;let
         (path-rmdir target)
        ) ;
  ) ;cond
) ;define

;; ---- 静态分组测试 -------------------------------------------------------

(let ((exts (all-registered-extensions)))
  (check (contains? ".scm" exts) => #t)
  (check (contains? ".cpp" exts) => #t)
  (check (contains? ".hpp" exts) => #t)
) ;let

(let ((groups (group-files-by-lang '("a.scm" "b.cpp" "c.hpp" "d.scm"))))
  (check (length groups) => 2)
  (check (contains? 'scheme (map car groups)) => #t)
  (check (contains? 'cpp (map car groups)) => #t)
  (let ((scheme-files (cdr (assq 'scheme groups)))
        (cpp-files (cdr (assq 'cpp groups)))
       ) ;
    (check (length scheme-files) => 2)
    (check (length cpp-files) => 2)
    (check (contains? "a.scm" scheme-files) => #t)
    (check (contains? "d.scm" scheme-files) => #t)
    (check (contains? "b.cpp" cpp-files) => #t)
    (check (contains? "c.hpp" cpp-files) => #t)
  ) ;let
) ;let

;; ---- 真实 git 仓库测试：Scheme 增量格式化 -------------------------------
;; 为避免依赖 clang-format，端到端测试只验证 Scheme 分支。

(define original-cwd (getcwd))

(define repo-dir
  (path->string (path-join (path-temp-dir)
                  (string-append "goldfmt-changed-since-test-" (number->string (getpid)))
                ) ;path-join
  ) ;path->string
) ;define

(dynamic-wind (lambda () (remove-tree repo-dir) (mkdir repo-dir) (chdir repo-dir))
  (lambda ()
    (must "git init -q")
    (must "git config user.email goldfish-test@example.com")
    (must "git config user.name Goldfish Test")

    (path-write-text (path "a.scm") "( define a 1 )\n")
    (path-write-text (path "b.cpp") "int b = 1;\n")
    (must "git add .")
    (must "git commit -q -m initial")

    (path-write-text (path "a.scm") "( define a 2 )\n")
    (path-write-text (path "b.cpp") "int b = 2;\n")

    ;; -e scm 只处理 Scheme 文件
    (let ((result (format-changed-since "HEAD" "" '(".scm") '() #f)))
      (check result => #t)
    ) ;let
    (check (string=? (path-read-text (path "a.scm")) "(define a 2)\n") => #t)
    ;; C++ 文件不应被 Scheme 格式化器改动
    (check (string=? (path-read-text (path "b.cpp")) "int b = 2;\n") => #t)

    ;; 默认使用所有语言时，应识别到 .scm 和 .cpp 两个变更文件
    ;; 这里不实际调用 clang-format，只检查分组结果。
    (let* ((all-files (changed-existing-files-since "HEAD"))
           (filtered (filter (lambda (f) (or (string-ends? f ".scm") (string-ends? f ".cpp")))
                       all-files
                     ) ;filter
           ) ;filtered
           (groups (group-files-by-lang filtered))
          ) ;
      (check (= (length filtered) 2) => #t)
      (check (contains? "a.scm" filtered) => #t)
      (check (contains? "b.cpp" filtered) => #t)
      (check (length groups) => 2)
    ) ;let*

    ;; 再次修改，测试默认所有语言的端到端格式化（依赖 clang-format）
    ;; 无 clang-format 的环境（如 macos/fedora/debian CI）跳过本段。
    (when (clang-format-available?)
      (path-write-text (path "a.scm") "( define a 3 )\n")
      (path-write-text (path "b.cpp") "int  b  =  3;\n")
      (let ((result (format-changed-since "HEAD" "" (all-registered-extensions) '() #f)))
        (check result => #t)
      ) ;let
      (check (string=? (path-read-text (path "a.scm")) "(define a 3)\n") => #t)
      (check (string=? (path-read-text (path "b.cpp")) "int b = 3;\n") => #t)
    ) ;when
  ) ;lambda
  (lambda () (chdir original-cwd) (remove-tree repo-dir))
) ;dynamic-wind

(check-report)
