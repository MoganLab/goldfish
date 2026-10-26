(import (liii check)
  (liii path)
  (liii os)
  (only (liii list) find)
  (liii string)
  (liii shutil)
  (liii subprocess)
  (liii sys)
  (liii pi code core extensions)
) ;import

(check-set-mode! 'report-failed)

(define (get-fixtures-dir)
  (cond
    ((file-exists? "tests/fixtures/extensions") "tests/fixtures/extensions")
    ((file-exists? "tools/pi/tests/fixtures/extensions") "tools/pi/tests/fixtures/extensions")
    (else "../../tools/pi/tests/fixtures/extensions")
  ) ;cond
) ;define

;; 1. 默认参数调用安全返回列表
(let ((discovered (pi-discover-extensions)))
  (check (list? discovered) => #t)
) ;let

;; 2. 指定显式扩展文件，返回解析后的路径
(let* ((explicit-file (string-append (get-fixtures-dir) "/valid-ext1.scm"))
       (discovered (pi-discover-extensions
                     :builtin-dir "/non/existent/builtin"
                     :agent-dir "/non/existent/agent"
                     :cwd "/non/existent/project"
                     :explicit-extensions (list explicit-file))))
  (check (pair? discovered) => #t)
  (check (length discovered) => 1)
  (check (string=? (path-name (path (car discovered))) "valid-ext1.scm") => #t)
) ;let*

;; 3. 模拟层级顺序：builtin -> global -> project -> explicit
(let* ((test-base (path->string (path-resolve (path (get-fixtures-dir)))))
       (builtin-dir (string-append test-base "/test-layers/builtin"))
       (global-agent-dir (string-append test-base "/test-layers/global-agent"))
       (project-dir (string-append test-base "/test-layers/project"))
       (explicit-file (string-append test-base "/valid-ext1.scm")))

  ;; 准备临时目录层级
  (catch #t (lambda ()
              (mkdir (string-append test-base "/test-layers"))
              (mkdir builtin-dir)
              (mkdir global-agent-dir)
              (mkdir (string-append global-agent-dir "/extensions"))
              (mkdir project-dir)
              (mkdir (string-append project-dir "/.pi"))
              (mkdir (string-append project-dir "/.pi/extensions")))
            (lambda args #f))

  ;; 写入测试文件
  (path-write-text (path (string-append builtin-dir "/01-builtin.scm")) "(lambda (api) 'builtin)")
  (path-write-text (path (string-append global-agent-dir "/extensions/02-global.scm")) "(lambda (api) 'global)")
  (path-write-text (path (string-append project-dir "/.pi/extensions/03-project.scm")) "(lambda (api) 'project)")

  (let ((discovered (pi-discover-extensions
                      :builtin-dir builtin-dir
                      :agent-dir global-agent-dir
                      :cwd project-dir
                      :explicit-extensions (list explicit-file))))
    (check (length discovered) => 4)
    (check (string=? (path-name (path (list-ref discovered 0))) "01-builtin.scm") => #t)
    (check (string=? (path-name (path (list-ref discovered 1))) "02-global.scm") => #t)
    (check (string=? (path-name (path (list-ref discovered 2))) "03-project.scm") => #t)
    (check (string=? (path-name (path (list-ref discovered 3))) "valid-ext1.scm") => #t)
  ) ;let

  ;; 4. 重复路径去重保持顺序
  (let ((discovered-dup (pi-discover-extensions
                          :builtin-dir builtin-dir
                          :agent-dir global-agent-dir
                          :cwd project-dir
                          :explicit-extensions (list explicit-file
                                                     explicit-file
                                                     (string-append builtin-dir "/01-builtin.scm")))))
    (check (length discovered-dup) => 4)
    (check (string=? (path-name (path (list-ref discovered-dup 0))) "01-builtin.scm") => #t)
    (check (string=? (path-name (path (list-ref discovered-dup 3))) "valid-ext1.scm") => #t)
  ) ;let

  ;; 清理临时目录层级
  (catch #t (lambda ()
              (remove (string-append builtin-dir "/01-builtin.scm"))
              (remove (string-append global-agent-dir "/extensions/02-global.scm"))
              (remove (string-append project-dir "/.pi/extensions/03-project.scm"))
              (rmdir builtin-dir)
              (rmdir (string-append global-agent-dir "/extensions"))
              (rmdir global-agent-dir)
              (rmdir (string-append project-dir "/.pi/extensions"))
              (rmdir (string-append project-dir "/.pi"))
              (rmdir project-dir)
              (rmdir (string-append test-base "/test-layers")))
            (lambda args #f))
) ;let*

;; 5. 同名扩展冲突：用户级可覆盖内置，项目级不可覆盖内置，
;; 但项目级可以覆盖普通用户级扩展
(let* ((test-base (path->string (path-resolve (path (get-fixtures-dir)))))
       (conflict-base (string-append test-base "/test-conflicts"))
       (builtin-dir (string-append conflict-base "/builtin"))
       (global-ext-dir (string-append conflict-base "/global-agent/extensions"))
       (project-dir (string-append conflict-base "/project"))
       (project-ext-dir (string-append project-dir "/.pi/extensions"))
      ) ;
  (for-each
    (lambda (dir)
      (path-mkdir (path dir) :parents #t :exist-ok #t)
    ) ;lambda
    (list builtin-dir global-ext-dir project-ext-dir)
  ) ;for-each

  (for-each
    (lambda (dir)
      (path-write-text (path (string-append dir "/shared.scm")) "(lambda (api) 'shared)")
    ) ;lambda
    (list builtin-dir global-ext-dir project-ext-dir)
  ) ;for-each
  (path-write-text (path (string-append builtin-dir "/builtin-only.scm"))
    "(lambda (api) 'builtin-only)"
  ) ;path-write-text
  (for-each
    (lambda (dir)
      (path-write-text (path (string-append dir "/custom.scm")) "(lambda (api) 'custom)")
    ) ;lambda
    (list global-ext-dir project-ext-dir)
  ) ;for-each

  (let ((discovered (pi-discover-extensions
                      :builtin-dir builtin-dir
                      :agent-dir (string-append conflict-base "/global-agent")
                      :cwd project-dir)))
    (define (discovered-named name)
      (find (lambda (p) (string=? (path-name (path p)) name)) discovered)
    ) ;define

    (check (length discovered) => 3)
    ;; 内置名称受保护：用户级覆盖内置，项目级不能再覆盖
    (check (discovered-named "shared.scm") => (string-append global-ext-dir "/shared.scm"))
    ;; 非内置名称：项目级覆盖用户级
    (check (discovered-named "custom.scm") => (string-append project-ext-dir "/custom.scm"))
    (check (discovered-named "builtin-only.scm")
      => (string-append builtin-dir "/builtin-only.scm")
    ) ;check
  ) ;let

  ;; 实际 CLI 已单独加载内置扩展：外部发现仍保护内置名称，但不重复返回内置文件
  (let ((discovered (pi-discover-extensions
                      :builtin-dir builtin-dir
                      :include-builtin? #f
                      :agent-dir (string-append conflict-base "/global-agent")
                      :cwd project-dir)))
    (define (discovered-named name)
      (find (lambda (p) (string=? (path-name (path p)) name)) discovered)
    ) ;define

    (check (length discovered) => 2)
    (check (discovered-named "shared.scm") => (string-append global-ext-dir "/shared.scm"))
    (check (discovered-named "custom.scm") => (string-append project-ext-dir "/custom.scm"))
    (check (discovered-named "builtin-only.scm") => #f)
  ) ;let

  (catch #t (lambda () (rmtree conflict-base #t)) (lambda args #f))
) ;let*

;; 6. 当前目录存在 tools/pi/extensions 时，仍优先使用程序自身的内置目录
(let* ((test-base (path->string (path-resolve (path (get-fixtures-dir)))))
       (priority-base (string-append test-base "/test-builtin-priority"))
       (decoy-cwd (string-append priority-base "/cwd"))
       (decoy-ext-dir (string-append decoy-cwd "/tools/pi/extensions"))
       (empty-agent-dir (string-append priority-base "/agent"))
      ) ;
  (path-mkdir (path decoy-ext-dir) :parents #t :exist-ok #t)
  (path-mkdir (path empty-agent-dir) :parents #t :exist-ok #t)
  (path-write-text (path (string-append decoy-ext-dir "/decoy.scm"))
    "(import (liii pi definitely-missing-library))"
  ) ;path-write-text

  (let* ((proc (process-spawn (list (executable) "pi" "--help")
                 :cwd decoy-cwd
                 :env (list (cons "PI_CODING_AGENT_DIR" empty-agent-dir))
                 :stdout 'pipe
                 :stderr 'pipe
               ) ;process-spawn
         ) ;proc
         (code (process-wait proc 10000))
        ) ;
    (unless code
      (process-kill proc)
      (process-wait proc 2000)
    ) ;unless
    (define (read-all stream)
      (let loop ((acc ""))
        (let ((chunk (process-read proc :stream stream)))
          (if (eof-object? chunk) acc (loop (string-append acc chunk)))
        ) ;let
      ) ;loop
    ) ;define
    (let ((err (read-all 'stderr)))
      (process-close proc)
      (check code => 0)
      (check (string-contains? err "decoy.scm") => #f)
      (check (string-contains? err "definitely-missing-library") => #f)
    ) ;let
  ) ;let*

  (catch #t (lambda () (rmtree priority-base #t)) (lambda args #f))
) ;let*

(check-report)
