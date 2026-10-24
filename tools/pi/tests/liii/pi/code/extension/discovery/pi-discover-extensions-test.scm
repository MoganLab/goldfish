(import (liii check)
  (liii path)
  (liii os)
  (liii pi code extension)
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

(check-report)
