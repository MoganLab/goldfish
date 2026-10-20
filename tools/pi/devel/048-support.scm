;; [048] 验收脚本辅助函数：blueprint 式外部命令驱动 + compass 各验收点断言。

(define %root (path->string (path-cwd)))

(define %fixture
  (string-append %root "/tools/pi/tests/fixtures/compass/project")
) ;define

(define %scheme-fixture
  (string-append %root "/tools/pi/tests/fixtures/compass/scheme")
) ;define

(define %tmp-root (path->string (path-join (path-temp-dir) "gf-048-accept")))

(define %home (string-append %tmp-root "/home"))

(define %work (string-append %tmp-root "/work"))

(define %work-file (string-append %tmp-root "/work-file"))

(define %work-scheme (string-append %tmp-root "/work-scheme"))

(define %work-empty (string-append %tmp-root "/work-empty"))

(define %tmp-files '())

(define (%setup-dir! dir)
  (unless (path-exists? (path dir))
    (path-mkdir (path dir) :parents #t :exist-ok #t)
  ) ;unless
) ;define

(define (%sh tag cmd timeout)
  (%setup-dir! %tmp-root)
  (let* ((out (string-append %tmp-root "/" tag ".out"))
         (err (string-append %tmp-root "/" tag ".err"))
        ) ;
    (set! %tmp-files (append (list out err) %tmp-files))
    (let* ((rc (run (list (pi-find-bash-path) "-c" (string-append "(" cmd ") >" out " 2>" err))
                 :stdin   'null
                 :timeout timeout
               ) ;run
           ) ;rc
           (o (if (path-file? (path out)) (path-read-text out) ""))
           (e (if (path-file? (path err)) (path-read-text err) ""))
          ) ;
      (list rc o e)
    ) ;let*
  ) ;let*
) ;define

(define (%ok? tag cmd timeout)
  (car (%sh tag cmd timeout))
) ;define

(define (%cleanup!)
  (for-each
    (lambda (f) (when (path-file? (path f)) (path-unlink (path f) #t)))
    %tmp-files
  ) ;for-each
  (when (path-exists? (path %tmp-root))
    (run (list (pi-find-bash-path) "-c" (string-append "rm -rf '" %tmp-root "'"))
      :stdin   'null
      :timeout 60
    ) ;run
  ) ;when
  (set! %tmp-files '())
) ;define

(define (%copy-dir! src dst)
  (%setup-dir! dst)
  (let ((r (%sh "copy" (string-append "cp -R '" src "/.' '" dst "'") 120)))
    (check (car r) => 0)
  ) ;let
) ;define

(define (%env)
  (list (cons "HOME" %home)
    (cons "PI_CODING_AGENT_DIR" (string-append %home "/.pi/agent"))
    (cons "PATH" (or (get-environment-variable "PATH") "/usr/bin:/bin"))
    (cons "LANG" (or (get-environment-variable "LANG") "C.UTF-8"))
    (cons "LC_ALL" (or (get-environment-variable "LC_ALL") "C.UTF-8"))
    (cons "https_proxy" "")
    (cons "http_proxy" "")
    (cons "all_proxy" "")
    (cons "HTTPS_PROXY" "")
    (cons "HTTP_PROXY" "")
    (cons "ALL_PROXY" "")
  ) ;list
) ;define

(define (%run-pi dir . args)
  (let ((p (process-spawn (cons (string-append %root "/bin/gf") (cons "pi" args))
             :cwd    dir
             :env    (%env)
             :stdout 'pipe
             :stderr 'pipe
           ) ;process-spawn
        ) ;p
       ) ;
    (let* ((code (process-wait p 240000))
           (out (process-read p :stream 'stdout))
           (err (process-read p :stream 'stderr))
          ) ;
      (process-close p)
      (list code (if (string? out) out "") (if (string? err) err ""))
    ) ;let*
  ) ;let
) ;define

(define (%cache-path dir)
  (string-append dir "/.pi/codemap.json")
) ;define

(define (%read-entry file)
  (string->json (path-read-text (path file)))
) ;define

(define (%field obj key)
  (let ((pair (assoc key obj)))
    (if pair (cdr pair) #f)
  ) ;let
) ;define

(define (%data entry)
  (%field entry "data")
) ;define

(define (%data-field entry key)
  (%field (%data entry) key)
) ;define

(define (%stamp entry)
  (%data-field entry "generatedAt")
) ;define

(define (%hash entry)
  (%field entry "contentHash")
) ;define

;; 写入「哈希正确、数据陈旧」的缓存，用于区分缓存命中与 --refresh。

(define (%write-stale! dir)
  (let* ((file (%cache-path dir))
         (entry (%read-entry file))
         (data (%data entry))
         (stale
           (cons (cons "projectName" "STALE-MARKER")
             (filter
               (lambda (pair) (not (string=? (car pair) "projectName")))
               data
             ) ;filter
           ) ;cons
         ) ;stale
         (hash (pi-compass-content-hash dir))
        ) ;
    (pi-compass-save-codemap! file
      (list (cons "data" stale) (cons "contentHash" hash) (cons "createdAt" "STALE"))
    ) ;pi-compass-save-codemap!
  ) ;let*
) ;define

(define (%check-arg-parsing!)
  (display "=== 验收点 1: CLI 参数解析与互斥 ===\n")
  (check (pi-args-ref (parse-pi-args '("--codemap")) 'codemap) => #t)
  (check (pi-args-ref (parse-pi-args '("--tour")) 'tour) => #t)
  (check (pi-args-ref (parse-pi-args '("--tour")) 'tour-topic) => #f)
  (check (pi-args-ref (parse-pi-args '("--tour" "src")) 'tour) => "src")
  (check (pi-args-ref (parse-pi-args '("--tour" "--no-tools")) 'tour) => #t)
  (check (pi-args-ref (parse-pi-args '("--refresh")) 'refresh) => #t)
  (check (pi-args-ref (parse-pi-args '("--codemap-file" "x.json")) 'codemap-file)
    =>
    "x.json"
  ) ;check
  (check (pi-args-ref (parse-pi-args '("--codemap-file=x.json")) 'codemap-file)
    =>
    "x.json"
  ) ;check
  (check (pi-args-ref (parse-pi-args '("--no-codemap-inject")) 'no-codemap-inject)
    =>
    #t
  ) ;check
  (check (pi-compass-tour-topic-arg (parse-pi-args '("--tour" "src"))) => "src")
  (check (pi-validate-review-flags (parse-pi-args '("--codemap"))) => #t)
  (check (pi-validate-review-flags (parse-pi-args '("--codemap" "--refresh")))
    =>
    #t
  ) ;check
  (check (pi-validate-review-flags (parse-pi-args '("--tour" "src"))) => #t)
  (for-each
    (lambda (args)
      (check-catch 'pi-arg-error (pi-validate-review-flags (parse-pi-args args)))
    ) ;lambda
    '(("--codemap" "--tour")
      ("--codemap" "--code-review")
      ("--codemap" "--simplify")
      ("--tour" "--red-green" "x")
      ("--codemap" "--blueprint" "x")
      ("--tour" "--plan-status")
      ("--refresh")
      ("--tour" "--refresh")
      ("--codemap-file" "x.json"))
  ) ;for-each
) ;define

(define (%check-codemap-cli!)
  (display "=== 验收点 2-3: codemap 离线生成 / 缓存新鲜度 ===\n")
  (%copy-dir! %fixture %work)
  (let ((r1 (%run-pi %work "--print" "--codemap")))
    (check (car r1) => 0)
    (for-each
      (lambda (s) (check-true (string-contains? (cadr r1) s)))
      '("## Codebase Map: work" "### Directory Structure" "### Packages"
        "### Frameworks" "### Entry Points" "### Build / Test / Deploy"
        "### Conventions" "### Key Files" "- **compass-sandbox** v0.1.0 (npm)"
        "Dependencies: express, react, vitest" "- React" "`src/index.js` (main)"
        "**build**: `xmake build` (xmake.lua)" "... (truncated)"
        "`README.md` -- Project documentation")
    ) ;for-each
  ) ;let
  (check (path-file? (path (%cache-path %work))) => #t)
  (let* ((e1 (%read-entry (%cache-path %work))) (stamp1 (%stamp e1)) (hash1 (%hash e1)))
    (check (string? hash1) => #t)
    (check (string-length hash1) => 16)
    (check (%field e1 "createdAt") => stamp1)
    ;; 二次运行命中缓存：Generated 时间戳不变。
    (let ((r2 (%run-pi %work "--print" "--codemap")))
      (check (car r2) => 0)
      (check-true (string-contains? (cadr r2) (string-append "Generated: " stamp1)))
    ) ;let
    ;; 深度 2 的新增文件 → 哈希变化 → 重新生成。
    (path-write-text (path (string-append %work "/src/brand-new.js")) "// new\n")
    (let* ((r3 (%run-pi %work "--print" "--codemap"))
           (e3 (%read-entry (%cache-path %work)))
          ) ;
      (check (car r3) => 0)
      (check-true (string-contains? (cadr r3) "brand-new.js"))
      (check (not (string=? (%hash e3) hash1)) => #t)
      (check (not (string=? (%stamp e3) stamp1)) => #t)
    ) ;let*
    ;; 哈希一致但数据陈旧：无 --refresh 命中缓存；--refresh 强制重生成。
    (%write-stale! %work)
    (let ((r4 (%run-pi %work "--print" "--codemap")))
      (check (car r4) => 0)
      (check-true (string-contains? (cadr r4) "STALE-MARKER"))
    ) ;let
    (let ((r5 (%run-pi %work "--print" "--codemap" "--refresh")))
      (check (car r5) => 0)
      (check-false (string-contains? (cadr r5) "STALE-MARKER"))
      (check-true (string-contains? (cadr r5) "## Codebase Map: work"))
    ) ;let
  ) ;let*
  ;; --codemap-file 覆盖路径，且不写默认 .pi/codemap.json。
  (%copy-dir! %fixture %work-file)
  (let ((r (%run-pi %work-file "--print" "--codemap" "--codemap-file" "custom.json")))
    (check (car r) => 0)
    (check (path-file? (path (string-append %work-file "/custom.json"))) => #t)
    (check-false (path-exists? (path (%cache-path %work-file))))
  ) ;let
  ;; JSON 模式：Markdown 摘要写 stderr，stdout 不混入。
  (let ((r (%run-pi %work "--mode" "json" "--codemap")))
    (check (car r) => 0)
    (check (string-null? (string-trim-both (cadr r))) => #t)
    (check-true (string-contains? (caddr r) "## Codebase Map:"))
  ) ;let
  ;; 不写 ~/.pi/compass（HOME 已隔离到临时目录）。
  (check-false (path-exists? (path (string-append %home "/.pi/compass"))))
) ;define

(define (%check-mutual-exclusion!)
  (display "=== 验收点 1b: CLI 互斥错误与退出码 ===\n")
  (for-each
    (lambda (pair)
      (let ((r (apply %run-pi %work (append (list "--print") (car pair)))))
        (check (car r) => 1)
        (check-true (string-contains? (caddr r) (cdr pair)))
      ) ;let
    ) ;lambda
    '((("--codemap" "--tour") . "cannot be used together")
      (("--codemap" "--simplify") . "--code-review, --simplify")
      (("--tour" "--red-green" "x") . "--code-review, --simplify")
      (("--refresh") . "--refresh requires --codemap")
      (("--tour" "--refresh") . "--refresh requires --codemap")
      (("--codemap-file" "x.json") . "--codemap-file requires --codemap"))
  ) ;for-each
) ;define
