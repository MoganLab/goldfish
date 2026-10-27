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

;; pi-discover-extensions
;; 按内置、用户级、项目级和显式路径的顺序发现并合并扩展文件。
;;
;; 语法
;; ----
;; (pi-discover-extensions . options) -> list
;;
;; 参数
;; ----
;; options : list
;; 支持 :builtin-dir、:agent-dir、:cwd、:explicit-extensions 和 :include-builtin?。
;;
;; 返回值
;; ----
;; list
;; 规范化后的 .scm 文件路径列表。同名扩展由后层覆盖前层，
;; 但项目级扩展不能覆盖内置名称；无效显式路径项会被忽略。
;;
;; 示例
;; ----
;; (pi-discover-extensions :cwd "/project" :include-builtin? #f)

(define (get-fixtures-dir)
  (cond ((file-exists? "tests/fixtures/extensions") "tests/fixtures/extensions")
        ((file-exists? "tools/pi/tests/fixtures/extensions")
         "tools/pi/tests/fixtures/extensions"
        ) ;
        (else "../../tools/pi/tests/fixtures/extensions")
  ) ;cond
) ;define

(define (%norm dir)
  ;; Windows 上 path-resolve 会把 / 规范化为 \，期望路径需做同样处理
  (path->string (path-resolve (path dir)))
) ;define

;; 1. 默认参数调用安全返回列表
(let ((discovered (pi-discover-extensions)))
  (check (list? discovered) => #t)
) ;let

;; 2. 指定显式扩展文件，返回解析后的路径
(let* ((explicit-file (string-append (get-fixtures-dir) "/valid-ext1.scm"))
       (discovered (pi-discover-extensions
                     :builtin-dir         "/non/existent/builtin"
                     :agent-dir           "/non/existent/agent"
                     :cwd                 "/non/existent/project"
                     :explicit-extensions (list explicit-file)
                   ) ;pi-discover-extensions
       ) ;discovered
      ) ;
  (check (pair? discovered) => #t)
  (check (length discovered) => 1)
  (check
    (string=? (path-name (path (car discovered))) "valid-ext1.scm")
    =>
    #t
  ) ;check
) ;let*

;; 3. 模拟层级顺序：builtin -> global -> project -> explicit
(let* ((test-base
         (path->string (path-resolve (path (get-fixtures-dir))))
       ) ;test-base
       (builtin-dir (%norm (string-append test-base "/test-layers/builtin")))
       (global-agent-dir (%norm (string-append test-base "/test-layers/global-agent")))
       (project-dir (%norm (string-append test-base "/test-layers/project")))
       (explicit-file (%norm (string-append test-base "/valid-ext1.scm")))
      ) ;

  ;; 准备临时目录层级
  (catch #t
    (lambda ()
      (mkdir (string-append test-base "/test-layers"))
      (mkdir builtin-dir)
      (mkdir global-agent-dir)
      (mkdir (string-append global-agent-dir "/extensions"))
      (mkdir project-dir)
      (mkdir (string-append project-dir "/.pi"))
      (mkdir (string-append project-dir "/.pi/extensions"))
    ) ;lambda
    (lambda args #f)
  ) ;catch

  ;; 写入测试文件
  (path-write-text (path (string-append builtin-dir "/01-builtin.scm"))
    "(lambda (api) 'builtin)"
  ) ;path-write-text
  (path-write-text (path (string-append global-agent-dir "/extensions/02-global.scm"))
    "(lambda (api) 'global)"
  ) ;path-write-text
  (path-write-text (path (string-append project-dir "/.pi/extensions/03-project.scm"))
    "(lambda (api) 'project)"
  ) ;path-write-text

  (let ((discovered (pi-discover-extensions
                      :builtin-dir         builtin-dir
                      :agent-dir           global-agent-dir
                      :cwd                 project-dir
                      :explicit-extensions (list explicit-file)
                    ) ;pi-discover-extensions
        ) ;discovered
       ) ;
    (check (length discovered) => 4)
    (check
      (string=? (path-name (path (list-ref discovered 0))) "01-builtin.scm")
      =>
      #t
    ) ;check
    (check
      (string=? (path-name (path (list-ref discovered 1))) "02-global.scm")
      =>
      #t
    ) ;check
    (check
      (string=? (path-name (path (list-ref discovered 2))) "03-project.scm")
      =>
      #t
    ) ;check
    (check
      (string=? (path-name (path (list-ref discovered 3))) "valid-ext1.scm")
      =>
      #t
    ) ;check
  ) ;let

  ;; 4. 重复路径去重保持顺序
  (let ((discovered-dup (pi-discover-extensions
                          :builtin-dir builtin-dir
                          :agent-dir   global-agent-dir
                          :cwd         project-dir
                          :explicit-extensions
                          (list explicit-file explicit-file (string-append builtin-dir "/01-builtin.scm"))
                        ) ;pi-discover-extensions
        ) ;discovered-dup
       ) ;
    (check (length discovered-dup) => 4)
    (check
      (string=? (path-name (path (list-ref discovered-dup 0))) "01-builtin.scm")
      =>
      #t
    ) ;check
    (check
      (string=? (path-name (path (list-ref discovered-dup 3))) "valid-ext1.scm")
      =>
      #t
    ) ;check
  ) ;let

  ;; 清理临时目录层级
  (catch #t
    (lambda ()
      (remove (string-append builtin-dir "/01-builtin.scm"))
      (remove (string-append global-agent-dir "/extensions/02-global.scm"))
      (remove (string-append project-dir "/.pi/extensions/03-project.scm"))
      (rmdir builtin-dir)
      (rmdir (string-append global-agent-dir "/extensions"))
      (rmdir global-agent-dir)
      (rmdir (string-append project-dir "/.pi/extensions"))
      (rmdir (string-append project-dir "/.pi"))
      (rmdir project-dir)
      (rmdir (string-append test-base "/test-layers"))
    ) ;lambda
    (lambda args #f)
  ) ;catch
) ;let*

;; 5. 同名扩展冲突：用户级可覆盖内置，项目级不可覆盖内置，
;; 但项目级可以覆盖普通用户级扩展
(let* ((test-base
         (path->string (path-resolve (path (get-fixtures-dir))))
       ) ;test-base
       (conflict-base (%norm (string-append test-base "/test-conflicts")))
       (builtin-dir (%norm (string-append conflict-base "/builtin")))
       (global-ext-dir (%norm (string-append conflict-base "/global-agent/extensions"))
       ) ;global-ext-dir
       (project-dir (%norm (string-append conflict-base "/project")))
       (project-ext-dir (%norm (string-append project-dir "/.pi/extensions")))
      ) ;
  (for-each (lambda (dir) (path-mkdir (path dir) :parents #t :exist-ok #t))
    (list builtin-dir global-ext-dir project-ext-dir)
  ) ;for-each

  (for-each
    (lambda (dir)
      (path-write-text (path (string-append dir "/shared.scm"))
        "(lambda (api) 'shared)"
      ) ;path-write-text
    ) ;lambda
    (list builtin-dir global-ext-dir project-ext-dir)
  ) ;for-each
  (path-write-text (path (string-append builtin-dir "/builtin-only.scm"))
    "(lambda (api) 'builtin-only)"
  ) ;path-write-text
  (for-each
    (lambda (dir)
      (path-write-text (path (string-append dir "/custom.scm"))
        "(lambda (api) 'custom)"
      ) ;path-write-text
    ) ;lambda
    (list global-ext-dir project-ext-dir)
  ) ;for-each

  (let ((discovered (pi-discover-extensions
                      :builtin-dir builtin-dir
                      :agent-dir   (string-append conflict-base "/global-agent")
                      :cwd         project-dir
                    ) ;pi-discover-extensions
        ) ;discovered
       ) ;
    (define (discovered-named name)
      (find
        (lambda (p) (string=? (path-name (path p)) name))
        discovered
      ) ;find
    ) ;define

    (check (length discovered) => 3)
    ;; 内置名称受保护：用户级覆盖内置，项目级不能再覆盖
    (check (discovered-named "shared.scm")
      =>
      (%norm (string-append global-ext-dir "/shared.scm"))
    ) ;check
    ;; 非内置名称：项目级覆盖用户级
    (check (discovered-named "custom.scm")
      =>
      (%norm (string-append project-ext-dir "/custom.scm"))
    ) ;check
    (check (discovered-named "builtin-only.scm")
      =>
      (%norm (string-append builtin-dir "/builtin-only.scm"))
    ) ;check
  ) ;let

  ;; 实际 CLI 已单独加载内置扩展：外部发现仍保护内置名称，但不重复返回内置文件
  (let ((discovered (pi-discover-extensions
                      :builtin-dir      builtin-dir
                      :include-builtin? #f
                      :agent-dir (string-append conflict-base "/global-agent")
                      :cwd       project-dir
                    ) ;pi-discover-extensions
        ) ;discovered
       ) ;
    (define (discovered-named name)
      (find
        (lambda (p) (string=? (path-name (path p)) name))
        discovered
      ) ;find
    ) ;define

    (check (length discovered) => 2)
    (check (discovered-named "shared.scm")
      =>
      (%norm (string-append global-ext-dir "/shared.scm"))
    ) ;check
    (check (discovered-named "custom.scm")
      =>
      (%norm (string-append project-ext-dir "/custom.scm"))
    ) ;check
    (check (discovered-named "builtin-only.scm") => #f)
  ) ;let

  (catch #t (lambda () (rmtree conflict-base #t)) (lambda args #f))
) ;let*

;; 6. 当前目录存在 tools/pi/extensions 时，仍优先使用程序自身的内置目录
(let* ((test-base
         (path->string (path-resolve (path (get-fixtures-dir))))
       ) ;test-base
       (priority-base (%norm (string-append test-base "/test-builtin-priority")))
       (decoy-cwd (%norm (string-append priority-base "/cwd")))
       (decoy-ext-dir (%norm (string-append decoy-cwd "/tools/pi/extensions")))
       (empty-agent-dir (%norm (string-append priority-base "/agent")))
      ) ;
  (path-mkdir (path decoy-ext-dir) :parents #t :exist-ok #t)
  (path-mkdir (path empty-agent-dir) :parents #t :exist-ok #t)
  (path-write-text (path (string-append decoy-ext-dir "/decoy.scm"))
    "(import (liii pi definitely-missing-library))"
  ) ;path-write-text

  (let* ((proc (process-spawn (list (executable) "pi" "--help")
                 :cwd    decoy-cwd
                 :env    (list (cons "PI_CODING_AGENT_DIR" empty-agent-dir))
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
      (let loop
        ((acc ""))
        (let ((chunk (process-read proc :stream stream)))
          (if (eof-object? chunk) acc (loop (string-append acc chunk)))
        ) ;let
      ) ;let
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
