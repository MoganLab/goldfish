(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii check)
  (liii error)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii os)
  (liii pi bash)
  (liii pi extension install)
  (liii pi extension discovery)
) ;import

(check-set-mode! 'report-failed)

;; [077] 验收脚本：pi install 插件安装命令实现

(define (%clean-dir-rec d)
  (let ((p (if (path? d) d (path d))))
    (when (path-dir? p)
      (for-each
        (lambda (e)
          (let ((full (path-join p e)))
            (if (path-dir? full) (%clean-dir-rec full) (remove (path->string full)))
          ) ;let
        ) ;lambda
        (vector->list (path-list p))
      ) ;for-each
      (rmdir (path->string p))
    ) ;when
  ) ;let
) ;define

(define (%run-ok? tag cmd timeout)
  (let ((tmp (string-append "tmp-077-" tag ".txt")))
    (let ((ret (run (list (pi-find-bash-path) "-c" (string-append cmd " > " tmp " 2>&1"))
                 :stdin   'null
                 :timeout timeout
               ) ;run
          ) ;ret
         ) ;
      (when (file-exists? tmp)
        (delete-file tmp)
      ) ;when
      ret
    ) ;let
  ) ;let
) ;define

(display "=== 验收点 1: 门禁检查（构建 / fmt / check / doc） ===\n")
(check (%run-ok? "build" "xmake b goldfish" 300) => 0)
(check (%run-ok? "fmt-chk" "bin/gf fmt --dry-run" 120) => 0)
(check (%run-ok? "check" "bin/gf check" 120) => 0)
(check (%run-ok? "doc" "bin/gf doc --build-json" 120) => 0)

(display "=== 验收点 2: 任务 077 代码与测试文件行数约束 (<= 300 行) ===\n"
) ;display
(for-each
  (lambda (f)
    (let ((lines (length (string-split (path-read-text (path f)) "\n"))))
      (check-true (<= lines 300))
    ) ;let
  ) ;lambda
  '("tools/pi/liii/pi/extension/install.scm" "tools/pi/liii/pi/cli.scm"
    "tools/pi/tests/liii/pi/extension/install/pi-parse-git-extension-name-test.scm"
    "tools/pi/tests/liii/pi/extension/install/pi-valid-extension-name-p-test.scm"
    "tools/pi/tests/liii/pi/extension/install/pi-resolve-install-target-test.scm"
    "tools/pi/tests/liii/pi/extension/install/pi-git-clone-extension-test.scm"
    "tools/pi/tests/liii/pi/extension/install/pi-install-extension-test.scm"
    "tools/pi/devel/077.scm")
) ;for-each

(display "=== 验收点 3: 扩展名称推断与安全性验证 ===\n")
;; 3.1 提取扩展名
(check (pi-parse-git-extension-name "https://github.com/alice/pi-super-plugin.git")
  =>
  "pi-super-plugin"
) ;check
(check (pi-parse-git-extension-name "git@github.com:bob/my-tool.git/")
  =>
  "my-tool"
) ;check
(check (pi-parse-git-extension-name "C:\\repos\\win_ext.git") => "win_ext")
(check (pi-parse-git-extension-name "just-name") => "just-name")

;; 3.2 安全名称校验（防路径穿透）
(check-true (pi-valid-extension-name? "good-tool_123"))
(check-false (pi-valid-extension-name? "../bad-tool"))
(check-false (pi-valid-extension-name? "tool/sub"))
(check-false (pi-valid-extension-name? "tool\\sub"))
(check-false (pi-valid-extension-name? "tool;rm"))

(display "=== 验收点 4: 安装目录推导与隔离 ===\n")
(let ((global-dir (pi-resolve-install-target "demo-plugin"
                    :local?    #f
                    :agent-dir "/mock/agent"
                    :cwd       "/mock/project"
                  ) ;pi-resolve-install-target
      ) ;global-dir
      (local-dir (pi-resolve-install-target "demo-plugin"
                   :local?    #t
                   :agent-dir "/mock/agent"
                   :cwd       "/mock/project"
                 ) ;pi-resolve-install-target
      ) ;local-dir
     ) ;
  (check
    (string=? global-dir
      (path->string (path-resolve (path "/mock/agent/extensions/demo-plugin")))
    ) ;string=?
    =>
    #t
  ) ;check
  (check
    (string=? local-dir
      (path->string (path-resolve (path "/mock/project/.pi/extensions/demo-plugin")))
    ) ;string=?
    =>
    #t
  ) ;check
) ;let

(display "=== 验收点 5: 真实 Git 克隆、目录发现与冲突防范实测 ===\n"
) ;display
(let* ((work-dir (path-join (path (getcwd)) "tmp-077-e2e-test"))
       (agent-dir (path-join work-dir "agent"))
       (proj-dir (path-join work-dir "project"))
       (repo-dir (path-join work-dir "remote-repo"))
      ) ;
  (when (path-exists? work-dir)
    (%clean-dir-rec work-dir)
  ) ;when
  (path-mkdir agent-dir :parents #t :exist-ok #t)
  (path-mkdir proj-dir :parents #t :exist-ok #t)
  (path-mkdir repo-dir :parents #t :exist-ok #t)

  ;; 准备一个模拟的远程 git 扩展仓库
  (run (list "git" "init" (path->string repo-dir)) :timeout 10)
  (path-write-text (path-join repo-dir "index.scm")
    "(define-library (test-plugin) (import (scheme base)) (export hello) (begin (define (hello) 'ok)))\n"
  ) ;path-write-text
  (run (list "git"
         "-C"
         (path->string repo-dir)
         "config"
         "user.email"
         "test@example.com"
       ) ;list
    :timeout 10
  ) ;run
  (run (list "git" "-C" (path->string repo-dir) "config" "user.name" "Test")
    :timeout 10
  ) ;run
  (run (list "git" "-C" (path->string repo-dir) "add" ".") :timeout 10)
  (run (list "git" "-C" (path->string repo-dir) "commit" "-m" "init") :timeout 10)

  ;; 5.1 全局安装测试
  (let ((installed (pi-install-extension (path->string repo-dir)
                     :local?    #f
                     :agent-dir (path->string agent-dir)
                     :cwd       (path->string proj-dir)
                   ) ;pi-install-extension
        ) ;installed
       ) ;
    (check-true (string? installed))
    (check-true (path-exists? (path-join (path installed) "index.scm")))
    ;; 验证可被发现
    (let ((disc (pi-discover-extensions
                  :agent-dir (path->string agent-dir)
                  :cwd       (path->string proj-dir)
                ) ;pi-discover-extensions
          ) ;disc
         ) ;
      (check-true
        (pair?
          (member
            (path->string (path-resolve (path-join (path installed) "index.scm")))
            disc
          ) ;member
        ) ;pair?
      ) ;check-true
    ) ;let
    ;; 5.2 冲突检测：重复安装必须报错
    (check-catch 'value-error
      (pi-install-extension (path->string repo-dir)
        :local?    #f
        :agent-dir (path->string agent-dir)
        :cwd       (path->string proj-dir)
      ) ;pi-install-extension
    ) ;check-catch
  ) ;let

  ;; 5.3 局部 (--local) 安装测试
  (let ((local-installed (pi-install-extension (path->string repo-dir)
                           :local?    #t
                           :agent-dir (path->string agent-dir)
                           :cwd       (path->string proj-dir)
                         ) ;pi-install-extension
        ) ;local-installed
       ) ;
    (check-true (string? local-installed))
    (check-true (path-exists? (path-join (path local-installed) "index.scm")))
    ;; 验证局部扩展被发现
    (let ((disc (pi-discover-extensions
                  :agent-dir (path->string agent-dir)
                  :cwd       (path->string proj-dir)
                ) ;pi-discover-extensions
          ) ;disc
         ) ;
      (check-true
        (pair?
          (member
            (path->string (path-resolve (path-join (path local-installed) "index.scm")))
            disc
          ) ;member
        ) ;pair?
      ) ;check-true
    ) ;let
  ) ;let

  ;; 5.4 异常情况清理验证：克隆失败不留残留
  (let ((bad-target (path-join agent-dir "extensions" "bad-clone")))
    (check-catch 'install-error
      (pi-git-clone-extension "file:///non/existent/repo.git"
        (path->string bad-target)
        :timeout 10
      ) ;pi-git-clone-extension
    ) ;check-catch
    (check-false (path-exists? bad-target))
  ) ;let

  ;; 清理临时目录
  (%clean-dir-rec work-dir)
) ;let*

(display "=== 验收点 6: CLI install 子命令端到端调用验证 ===\n")
;; 6.1 缺省参数报错
(check-false (= 0 (%run-ok? "cli-no-url" "bin/gf pi install" 30)))
;; 6.2 非法参数报错
(check-false (= 0
               (%run-ok? "cli-bad-opt"
                 "bin/gf pi install --bad-opt https://github.com/org/ext.git"
                 30
               ) ;%run-ok?
             ) ;=
) ;check-false

(display "=== 验收点 7: 任务 077 专属单测全量通过 ===\n")
(check (%run-ok? "install-test"
         "bin/gf test tools/pi/tests/liii/pi/extension/install/"
         60
       ) ;%run-ok?
  =>
  0
) ;check

(display "=== 验收点 8: tools/pi 全量单测套件回归全绿 ===\n")
(check (%run-ok? "all-tests" "bin/gf test tools/pi/tests/liii/" 180) => 0)

(display "=== [077] 验收脚本执行完成 ===\n")

(check-report)
