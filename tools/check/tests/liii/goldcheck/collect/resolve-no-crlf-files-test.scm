;; resolve-no-crlf-files
;; 为 no-crlf 规则解析匹配配置后缀与路径的目标文件列表。
;;
;; 语法
;; ----
;; (resolve-no-crlf-files parser cfg)
;;
;; 参数
;; ----
;; parser : procedure  命令行参数解析器对象
;; cfg : list          检查配置（JSON alist 结构）
;;
;; 返回值
;; ------
;; list of string
;; 符合后缀及路径范围要求的待检测文件列表。

(import (scheme base) (liii check) (liii string) (liii os) (liii path)
        (liii goldcheck collect)
) ;import

(check-set-mode! 'report-failed)

;; 1. 指定位置参数时的过滤
(let* ((target (if (file-exists? "tools/check/liii/goldcheck/collect.scm")
                 "tools/check/liii/goldcheck/collect.scm"
                 "liii/goldcheck/collect.scm"
               ) ;if
       ) ;target
       (mock-parser
         (lambda (msg)
           (cond ((eq? msg 'changed-since) #f)
                 ((eq? msg :positionals) (list target))
                 (else #f)
           ) ;cond
         ) ;lambda
       ) ;mock-parser
       (cfg
         '(("rules" ("no-crlf" ("suffix" . #("scm" "json")))))
       ) ;cfg
       (files (resolve-no-crlf-files mock-parser cfg))
      ) ;
  (check (length files) => 1)
  (check-true (string-ends? (car files) "collect.scm"))
) ;let*

;; 2. --changed-since 场景：已删除的文件不得进入待检测列表
(define original-cwd (getcwd))

(define repo-dir
  (path->string
    (path-join (path-temp-dir)
      (string-append "goldcheck-no-crlf-test-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(define (must command)
  (let ((status (os-call command)))
    (unless (zero? status)
      (error "Command failed" command)
    ) ;unless
  ) ;let
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
        (else #t)
  ) ;cond
) ;define

(dynamic-wind (lambda () (remove-tree repo-dir) (mkdir repo-dir) (chdir repo-dir))
  (lambda ()
    (must "git init -q")
    (must "git config user.email goldfish-test@example.com")
    (must "git config user.name Goldfish Test")
    (path-write-text (path "keep.scm") "(define keep 1)\n")
    (path-write-text (path "gone.scm") "(define gone 1)\n")
    (path-write-text (path "notes.txt") "hello\n")
    (must "git add .")
    (must "git commit -q -m initial")

    ;; 删除 gone.scm 并修改 keep.scm，制造一个含不存在路径的变更集
    (path-unlink (path "gone.scm") #t)
    (path-write-text (path "keep.scm") "(define keep 2)\n")
    (must "git add -A")
    (must "git commit -q -m drop-gone")

    (let* ((mock-parser
             (lambda (msg)
               (cond ((eq? msg 'changed-since) "HEAD~1")
                     ((eq? msg :positionals) '())
                     (else #f)
               ) ;cond
             ) ;lambda
            ) ;mock-parser
            (cfg '(("rules" ("no-crlf" ("suffix" . #("scm" "json"))))))
            (files (resolve-no-crlf-files mock-parser cfg))
           ) ;
      ;; 已删除的文件不参与检测（回归 #1498）
      (check (member "gone.scm" files) => #f)
      ;; 存在且后缀匹配的文件仍然被收集（member 返回尾表，故归一为布尔）
      (check-true (if (member "keep.scm" files) #t #f))
      ;; 后缀不匹配的文件被排除
      (check (member "notes.txt" files) => #f)
      (check files => '("keep.scm"))
    ) ;let*
  ) ;lambda
  (lambda () (chdir original-cwd) (remove-tree repo-dir))
) ;dynamic-wind

(check-report)
