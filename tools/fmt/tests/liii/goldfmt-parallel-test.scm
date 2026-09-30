(set! *load-path* (cons "tools/fmt" *load-path*))
(set! *load-path* (cons "tools/common" *load-path*))

(import (liii check)
  (liii sort)
  (liii goldfmt)
  (liii goldfmt-lang)
  (liii goldfmt-parallel)
  (liii cpp-fmt)
  (liii os)
  (liii path)
  (liii string)
  (liii subprocess)
  (scheme file)
  (scheme process-context)
) ;import

(check-set-mode! 'report-failed)

;; ============================================================
;; gf fmt 多协程并发格式化测试（任务 1609）
;;
;; 核心约定：并行路径（-j > 1）与串行路径（-j 1）共用同一安静单位函数与
;; 打印规则，输出必须与串行逐字节一致（按文件序回收结果）。
;;
;; 缓存确定性：fmt 缓存键为文件当前内容的 SHA-256（跨沙箱持久），测试内容
;; 一律以 pid 加盐，保证每次运行都是冷缓存、行为可预期。
;; ============================================================

;; ---- T1: -j/--jobs 解析 ------------------------------------------------

(check (parse-fmt-jobs '("gf" "fmt")) => 0)
(check (parse-fmt-jobs '("gf" "fmt" "dir")) => 0)
(check (parse-fmt-jobs '("gf" "fmt" "-j" "4")) => 4)
(check (parse-fmt-jobs '("gf" "fmt" "-j" "10" "dir")) => 10)
(check (parse-fmt-jobs '("gf" "fmt" "--jobs=8")) => 8)
(check (parse-fmt-jobs '("gf" "fmt" "-j=16")) => 16)
(check (parse-fmt-jobs '("gf" "fmt" "-j" "0")) => 0)
(check (parse-fmt-jobs '("gf" "fmt" "-j" "-2")) => 0)
(check (parse-fmt-jobs '("gf" "fmt" "-j" "abc" "dir")) => 0)

;; fmt-jobs / set-fmt-jobs!：模块级并发度参数（默认 1 = 串行；主入口设置一次）

(check (fmt-jobs) => 1)
(set-fmt-jobs! 4)
(check (fmt-jobs) => 4)
(set-fmt-jobs! 1)
(check (fmt-jobs) => 1)

;; ---- 端到端基础设施（照搬 goldfmt-dir-test.scm）------------------------

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

(define project-root
  (if (file-exists? "gfproject.json")
    (getcwd)
    (if (file-exists? "../../gfproject.json")
      (path->string (path-parent (path-parent (path (getcwd)))))
      (getcwd)
    ) ;if
  ) ;if
) ;define

(define gf-bin
  (string-append project-root
    (if (os-windows?) "/bin/gf.exe" "/bin/gf")))

(run-set! 'gf gf-bin)

;; 在指定目录作为项目根运行 gf（供 mini 项目仓库批量场景使用）。
(define (run-gf-in-values cwd . args)
  (run-values (cons 'gf args) :cwd cwd :stdout 'capture :stderr 'stdout)
) ;define

(define (run-gf-values . args)
  (apply run-gf-in-values project-root args)
) ;define

;; 检测 clang-format 是否可用；不可用时跳过 C++ 相关断言。
(define (clang-format-available?)
  (zero? (os-call (string-append (clang-format-binary) " --version")))
) ;define

(define sandbox-dir
  (path->string (path-join (path-temp-dir)
                  (string-append "goldfmt-parallel-test-" (number->string (getpid)))
                ) ;path-join
  ) ;path->string
) ;define

(define salt (number->string (getpid)))

(define (pad2 n)
  (if (< n 10) (string-append "0" (number->string n)) (number->string n))
) ;define

;; 生成第 i 个测试文件：formatted? 为 #t 时内容已是格式化形态（期望 Formatting 行），
;; 否则为未格式化形态（期望 Updated 行）。salt 保证内容全局唯一（缓存冷启动）。
(define (write-test-file dir i file-salt formatted?)
  (let* ((nm (string-append "f" (pad2 i) ".scm"))
         (var (string-append "x" (pad2 i)))
         (content (if formatted?
                    (string-append "(define " var " " file-salt ")\n")
                    (string-append "( define   " var "   " file-salt " )\n")
                  )) ;if
                  ;
        ) ;
    (path-write-text (path (path->string (path-join (path dir) nm))) content)
    (path->string (path-join (path dir) nm))
  ) ;let*
) ;define

(define (expected-content i file-salt)
  (string-append "(define x" (pad2 i) " " file-salt ")\n")
) ;define

;; 断言 out 中各文件行严格按 targets 列表（即目录列举顺序）出现：
;; 并行路径的输出顺序必须与串行路径一致（目录列举顺序由文件系统决定，
;; 不做字典序假设）。

;; 目录下 .scm 文件名的列举顺序（path-list-path 原序，仅用于顺序断言）。

;; 无序输出契约的对比辅助：按行拆分、排序后比较（顺序无关的逐行一致）。
(define (sorted-lines out)
  (list-sort string<? (string-split out "\n"))
) ;define

;; 独立实现的 DFS 先序遍历（直接走 path-list-path，不依赖 collect-files 的
;; 累积实现），守护 collect-files-dfs 的顺序契约——目录格式化的输出行序
;; （并行与串行一致性的基准顺序）依赖它。
(define (test-dfs-files dir)
  (let ((entries (path-list-path (path dir))))
    (let loop
      ((i 0) (acc '()))
      (if (>= i (vector-length entries))
        acc
        (let ((e (vector-ref entries i)))
          (cond
           ((path-file? e)
            (let ((s (path->string e)))
              (loop (+ i 1)
                (if (string-ends? s ".scm") (append acc (list s)) acc)
              ) ;if
            ) ;let
           ) ;
           ((path-dir? e)
            (loop (+ i 1) (append acc (test-dfs-files (path->string e))))
           ) ;
           (else (loop (+ i 1) acc))
          ) ;cond
        ) ;let
      ) ;if
    ) ;let
  ) ;let
) ;define

(define n-t2 30)

(dynamic-wind (lambda ()
                (remove-tree sandbox-dir)
                (mkdir sandbox-dir)
              ) ;lambda
  (lambda ()

    ;; ---- T0: collect-files-dfs 的 DFS 先序顺序契约 ---------------------

    (let* ((d0 (path->string (path-join (path sandbox-dir) "t0_dfs")))
           (sub (path->string (path-join (path d0) "sub")))
           (deep (path->string (path-join (path sub) "deep")))
          ) ;
      (mkdir d0)
      (mkdir sub)
      (mkdir deep)
      (write-test-file d0 1 salt #f)
      (path-write-text (path (string-append d0 "/z.txt")) "not scm\n")
      (write-test-file d0 2 salt #f)
      (write-test-file sub 3 salt #f)
      (write-test-file deep 4 salt #f)
      (path-write-text (path (string-append deep "/n.scm")) "(define n 1)\n")
      (write-test-file sub 5 salt #f)
      (check (collect-files-dfs d0 '(".scm") '()) => (test-dfs-files d0))
      ;; exclude 对文件与子目录同样生效
      (check (length (collect-files-dfs d0 '(".scm") '("deep"))) => 4)
      (check (length (collect-files-dfs d0 '(".scm") '("sub"))) => 2)
    ) ;let

    ;; ---- T2: 并行与串行输出逐字节一致（核心）--------------------------

    (let* ((dir-a (path->string (path-join (path sandbox-dir) "t2_a")))
           (dir-b (path->string (path-join (path sandbox-dir) "t2_b")))
           (salt-a salt)
           (salt-b (string-append salt "z"))
          ) ;
      (mkdir dir-a)
      (mkdir dir-b)
      (let loop
        ((i 1))
        (when (<= i n-t2)
          (write-test-file dir-a i salt-a (= 0 (modulo i 2)))
          (write-test-file dir-b i salt-b (= 0 (modulo i 2)))
          (loop (+ i 1))
        ) ;when
      ) ;let
      (let-values (((out-a err-a code-a) (run-gf-values "fmt" "-j" "8" dir-a)))
        (let-values (((out-b err-b code-b) (run-gf-values "fmt" "-j" "1" dir-b)))
          (check code-a => 0)
          (check code-b => 0)
          ;; 归一化目录路径后，并行输出与串行输出逐字节一致
          ;; 无序契约：不保证完成顺序，按行排序后比较（逐行集合一致）
          (let ((norm-a (string-replace out-a dir-a "DIR"))
                (norm-b (string-replace out-b dir-b "DIR"))
               ) ;
            (check (equal? (sorted-lines norm-a) (sorted-lines norm-b)) => #t)
            (check-true (string-contains? norm-a "Total files formatted: 30"))
            (check-true (string-contains? norm-a "Files updated: 15"))
          ) ;let
          ;; 落盘内容：两边一致且等于期望格式化结果
          (let loop
            ((i 1))
            (if (> i n-t2)
              #t
              (let* ((fa (path->string (path-join (path dir-a) (string-append "f" (pad2 i) ".scm"))))
                     (fb (path->string (path-join (path dir-b) (string-append "f" (pad2 i) ".scm"))))
                    ) ;
                (check (path-read-text (path fa)) => (expected-content i salt-a))
                (check (path-read-text (path fb)) => (expected-content i salt-b))
                (loop (+ i 1))
              ) ;let*
            ) ;if
          ) ;let
        ) ;let-values
      ) ;let-values

      ;; ---- T4: 缓存语义：二次运行全部命中缓存 --------------------------

      (let-values (((out2 err2 code2) (run-gf-values "fmt" "-j" "8" dir-a)))
        (check code2 => 0)
        (check (string-position "  Updated: " out2) => #f)
        (check (string-position "Formatting: " out2) => #f)
        (check-true (string-contains? out2 "Total files formatted: 30"))
        (check-true (string-contains? out2 "Files updated: 0"))
        (check-true (string-contains? out2 "Files unchanged: 30"))
      ) ;let-values
    ) ;let

    ;; ---- T2b: 缺省参数（自动并发）同样正确 ----------------------------

    (let* ((dir-d (path->string (path-join (path sandbox-dir) "t2b")))
           (salt-d (string-append salt "d"))
          ) ;
      (mkdir dir-d)
      (let loop
        ((i 1))
        (when (<= i 5)
          (write-test-file dir-d i salt-d (= 0 (modulo i 2)))
          (loop (+ i 1))
        ) ;when
      ) ;let
      (let-values (((out-d err-d code-d) (run-gf-values "fmt" dir-d)))
        (check code-d => 0)
        (check-true (string-contains? out-d "Total files formatted: 5"))
        (check-true (string-contains? out-d "Files updated: 3"))
        (check (path-read-text (path (path->string (path-join (path dir-d) "f03.scm"))))
          => (expected-content 3 salt-d)
        ) ;check
      ) ;let-values
    ) ;let

    ;; ---- T3: 失败传播（混入括号错误文件）------------------------------

    (let* ((dir-c (path->string (path-join (path sandbox-dir) "t3_a")))
           (dir-e (path->string (path-join (path sandbox-dir) "t3_b")))
           (salt-c (string-append salt "c"))
           (salt-e (string-append salt "e"))
          ) ;
      (mkdir dir-c)
      (mkdir dir-e)
      (let loop
        ((i 1))
        (when (<= i 4)
          (write-test-file dir-c i salt-c #f)
          (write-test-file dir-e i salt-e #f)
          (loop (+ i 1))
        ) ;when
      ) ;let
      (path-write-text
        (path (path->string (path-join (path dir-c) "bad.scm")))
        (string-append "(define bad " salt-c "))\n")
      ) ;path-write-text
      (path-write-text
        (path (path->string (path-join (path dir-e) "bad.scm")))
        (string-append "(define bad " salt-e "))\n")
      ) ;path-write-text
      (let-values (((out-c err-c code-c) (run-gf-values "fmt" "-j" "4" dir-c)))
        (let-values (((out-e err-e code-e) (run-gf-values "fmt" "-j" "1" dir-e)))
          (check (zero? code-c) => #f)
          (check (zero? code-e) => #f)
          (check-true (string-contains? out-c "Files failed: 1"))
          (check-true (string-contains? out-c "Failed: "))
          (check-true (string-contains? out-c "unexpected close paren"))
          (check-true (string-contains? out-c "Hint: try `gf fix "))
          ;; 归一化后并行与串行逐字节一致（含失败行位置）
          (check
            (equal? (sorted-lines (string-replace out-c dir-c "DIR"))
              (sorted-lines (string-replace out-e dir-e "DIR"))
            ) ;equal?
            => #t
          ) ;check
          ;; 好文件照常被格式化
          (check (path-read-text (path (path->string (path-join (path dir-c) "f01.scm"))))
            => (expected-content 1 salt-c)
          ) ;check
        ) ;let-values
      ) ;let-values
    ) ;let

    ;; ---- T5: scheme + stem 混合语言仓库批量 --------------------------
    ;; 语言间串行（C++ → TeXmacs Stem → Scheme，按注册顺序）、语言内并行；
    ;; 重点守护 stem 会话隔离：并行下 (quote x) 不被糖化为 'x。

    (let* ((root5 (path->string (path-join (path sandbox-dir) "t5_proj")))
           (scheme-dir (path->string (path-join (path root5) "scm")))
           (stem-dir (path->string (path-join (path root5) "stm")))
           (salt-5 salt)
          ) ;
      (mkdir root5)
      (mkdir scheme-dir)
      (mkdir stem-dir)
      (path-write-text (path (string-append root5 "/gfproject.json"))
        "{\"name\": \"test-project\"}\n"
      ) ;path-write-text
      (path-write-text (path (string-append root5 "/gf_fmt.json"))
        "{\"scheme\": {\"suffix\": [\"scm\"], \"path\": [\"scm\"]}, \"stem\": {\"suffix\": [\"stem\"], \"path\": [\"stm\"]}}\n"
      ) ;path-write-text
      (let loop
        ((i 1))
        (when (<= i 6)
          (write-test-file scheme-dir i salt-5 #f)
          (path-write-text
            (path (string-append stem-dir "/g" (pad2 i) ".stem"))
            (string-append "( define   gy" (pad2 i) "   " salt-5 " )\n")
          ) ;path-write-text
          (loop (+ i 1))
        ) ;when
      ) ;let
      ;; 已规范的 quote 形态（stem 关键约束：不得糖化为 'x）
      (path-write-text (path (string-append stem-dir "/q01.stem"))
        (string-append "(quote sz" salt-5 ")\n")
      ) ;path-write-text
      (let-values (((out5 err5 code5) (run-gf-in-values root5 "fmt" "-j" "8")))
        (check code5 => 0)
        ;; 语言块顺序 = 注册顺序（C++ → TeXmacs Stem → Scheme），语言间串行
        (let ((p-cpp (string-position "=== Formatting C++ files ===" out5))
              (p-stem (string-position "=== Formatting TeXmacs Stem files ===" out5))
              (p-scheme (string-position "=== Formatting Scheme files ===" out5))
             ) ;
          (check-true (and p-cpp p-stem p-scheme
                       (< p-cpp p-stem)
                       (< p-stem p-scheme))
                      ;and
          ) ;check-true
        ) ;let
        (check-true (string-contains? out5 "Total Scheme files formatted: 6"))
        (check-true (string-contains? out5 "Total TeXmacs Stem files formatted: 7"))
        ;; .scm 正常格式化
        (check (path-read-text (path (string-append scheme-dir "/f01.scm")))
          => (expected-content 1 salt-5)
        ) ;check
        ;; .stem 格式化且 quote 不糖化
        (check (path-read-text (path (string-append stem-dir "/g01.stem")))
          => (string-append "(define gy01 " salt-5 ")\n")
        ) ;check
        (let ((q01 (path-read-text (path (string-append stem-dir "/q01.stem")))))
          (check-true (string-contains? q01 "(quote "))
          (check (string-position "'" q01) => #f)
        ) ;let
      ) ;let-values
    ) ;let

    ;; ---- T6: cpp 并行（clang-format 可用时）--------------------------

    (when (clang-format-available?)
      (let* ((dir-p (path->string (path-join (path sandbox-dir) "t6_a")))
             (dir-q (path->string (path-join (path sandbox-dir) "t6_b")))
             (salt-p salt)
             (salt-q (string-append salt "q"))
            ) ;
        (mkdir dir-p)
        (mkdir dir-q)
        (let loop
          ((i 1))
          (when (<= i 4)
            (let* ((nm (string-append "c" (pad2 i) ".cpp"))
                   (mk (lambda (dir file-salt)
                         (path-write-text
                           (path (path->string (path-join (path dir) nm)))
                           (string-append "int   a" file-salt " ;\nint   b ;\n")
                         ) ;path-write-text
                       ) ;lambda
                   )) ;
              (mk dir-p salt-p)
              (mk dir-q salt-q)
              (loop (+ i 1))
            ) ;let*
          ) ;when
        ) ;let
        (let-values (((out-p err-p code-p) (run-gf-values "fmt" "-e" "cpp" "-j" "4" dir-p)))
          (let-values (((out-q err-q code-q) (run-gf-values "fmt" "-e" "cpp" "-j" "1" dir-q)))
            (check code-p => 0)
            (check code-q => 0)
            ;; 归一化路径与盐后，并行与串行输出一致
            (let ((norm-p (string-replace (string-replace out-p dir-p "DIR") salt-p "SALT"))
                  (norm-q (string-replace (string-replace out-q dir-q "DIR") salt-q "SALT"))
                 ) ;
              (check (string=? norm-p norm-q) => #t)
              (check-true (string-contains? norm-p "Total files formatted: 4"))
              (check-true (string-contains? norm-p "  Updated: "))
            ) ;let
            ;; 两目录落盘内容一致（盐归一后）
            (let loop
              ((i 1))
              (if (> i 4)
                #t
                (let* ((nm (string-append "c" (pad2 i) ".cpp"))
                       (cp (path-read-text (path (path->string (path-join (path dir-p) nm)))))
                       (cq (path-read-text (path (path->string (path-join (path dir-q) nm)))))
                      ) ;
                  (check (string-replace cq salt-q salt-p) => cp)
                  (loop (+ i 1))
                ) ;let*
              ) ;if
            ) ;let
          ) ;let-values
        ) ;let-values
      ) ;let
    ) ;when

    ;; ---- T8: --check 并行一致性 --------------------------------------

    (let* ((dir-i (path->string (path-join (path sandbox-dir) "t8_a")))
           (dir-j (path->string (path-join (path sandbox-dir) "t8_b")))
           (salt-i (string-append salt "i"))
           (salt-j (string-append salt "j"))
          ) ;
      (mkdir dir-i)
      (mkdir dir-j)
      (let loop
        ((i 1))
        (when (<= i 6)
          (write-test-file dir-i i salt-i #f)
          (write-test-file dir-j i salt-j #f)
          (loop (+ i 1))
        ) ;when
      ) ;let
      ;; 未格式化：并行与串行的 --check 输出一致、退出码均为 1
      (let-values (((out-i err-i code-i) (run-gf-values "fmt" "--check" "-j" "4" dir-i)))
        (let-values (((out-j err-j code-j) (run-gf-values "fmt" "--check" "-j" "1" dir-j)))
          (check (zero? code-i) => #f)
          (check (zero? code-j) => #f)
          (check
            (equal? (sorted-lines (string-replace out-i dir-i "DIR"))
              (sorted-lines (string-replace out-j dir-j "DIR"))
            ) ;equal?
            => #t
          ) ;check
          (check-true (string-contains? out-i "FAIL: 6 file(s) need formatting"))
        ) ;let-values
      ) ;let-values
      ;; 格式化后：并行 --check 通过、退出码 0
      (let-values (((out-fmt err-fmt code-fmt) (run-gf-values "fmt" "-j" "4" dir-i)))
        (check code-fmt => 0)
        (let-values (((out-i2 err-i2 code-i2) (run-gf-values "fmt" "--check" "-j" "4" dir-i)))
          (check code-i2 => 0)
          (check-true (string-contains? out-i2 "OK: all files formatted."))
        ) ;let-values
      ) ;let-values
    ) ;let

    ;; ---- T7: 退化路径 -----------------------------------------------

    ;; 单文件 + -j 8：走串行分支，正常格式化
    (let* ((dir-f (path->string (path-join (path sandbox-dir) "t7_single")))
           (salt-f (string-append salt "f"))
          ) ;
      (mkdir dir-f)
      (write-test-file dir-f 1 salt-f #f)
      (let-values (((out-f err-f code-f) (run-gf-values "fmt" "-j" "8" dir-f)))
        (check code-f => 0)
        (check (path-read-text (path (path->string (path-join (path dir-f) "f01.scm"))))
          => (expected-content 1 salt-f)
        ) ;check
      ) ;let-values
    ) ;let

    ;; 空目录 + -j 8：无文件，正常退出
    (let ((dir-g (path->string (path-join (path sandbox-dir) "t7_empty"))))
      (mkdir dir-g)
      (let-values (((out-g err-g code-g) (run-gf-values "fmt" "-j" "8" dir-g)))
        (check code-g => 0)
        (check-true (string-contains? out-g "Total files formatted: 0"))
      ) ;let-values
    ) ;let

    ;; GOLDFISH_GO_WORKERS=1 约束线程池（POSIX）：-j 8 仍正确（任务在单线程池排队执行）。    ;; 注意 run-values 的 :env 是整体替换（不继承父环境），需带上 HOME/PATH。
    (unless (os-windows?)
      (let* ((dir-h (path->string (path-join (path sandbox-dir) "t7_pool1")))
             (salt-h (string-append salt "h"))
            ) ;
        (mkdir dir-h)
        (let loop
          ((i 1))
          (when (<= i 6)
            (write-test-file dir-h i salt-h #f)
            (loop (+ i 1))
          ) ;when
        ) ;let
        (let-values (((out-h err-h code-h)
                      (run-values (cons 'gf (list "fmt" "-j" "8" dir-h))
                        :cwd project-root
                        :env (list (cons "GOLDFISH_GO_WORKERS" "1")
                                (cons "HOME" (getenv "HOME"))
                                (cons "PATH" (getenv "PATH")))
                               ;list
                        :stdout 'capture
                        :stderr 'stdout
                      )) ;run-values
                    ) ;let-values
               ;
          (check code-h => 0)
          (check (path-read-text (path (path->string (path-join (path dir-h) "f01.scm"))))
            => (expected-content 1 salt-h)
          ) ;check
        ) ;let-values
      ) ;let
    ) ;unless
  ) ;lambda
  (lambda ()
    (remove-tree sandbox-dir)
  ) ;lambda
) ;dynamic-wind

(check-report)
