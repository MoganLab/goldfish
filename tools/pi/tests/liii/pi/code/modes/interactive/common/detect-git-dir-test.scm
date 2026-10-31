(import (scheme base)
  (scheme time)
  (liii check)
  (liii string)
  (liii path)
  (liii os)
  (liii tempfile)
  (liii shutil)
  (liii pi code modes interactive common))

(check-set-mode! 'report-failed)

;; detect-git-dir
;; 纯文件读取探测当前工作区所属 git 仓库的 .git 目录（零子进程）。
;;
;; 语法
;; ----
;; (detect-git-dir) → string? | #f
;;
;; 返回值
;; -----
;; string?
;; .git 目录的绝对路径；worktree/submodule 场景返回解析后的真实 gitdir。
;; #f
;; 非 git 仓库（向上到根都没有 .git）、.git 文件内容无法解析时返回 #f。
;;
;; 说明
;; ----
;; 1. 从 cwd 向上逐级查找 .git：是目录 → 普通仓库；是文件 → 读
;;    "gitdir: <path>" 解析 worktree/submodule 的 gitdir（相对路径相对
;;    .git 文件所在目录解析）。
;; 2. 与 git rev-parse --absolute-git-dir 的差异：不支持 GIT_DIR 环境
;;    变量与 bare repo，这些场景返回 #f（footer 不显示分支、branch
;;    watcher 不启动，纯装饰性降级）。
;; 3. 启动路径（gf pi 首帧）与 branch_changed 事件路径统一为零子进程。

(define (%write-git-head! git-dir content)
  (path-mkdir git-dir #t)
  (path-write-text (path->string (path-join git-dir "HEAD")) content)
) ;define

(define %orig-cwd (getcwd))

(define (%with-cwd dir thunk)
  (dynamic-wind
    (lambda () (chdir dir))
    thunk
    (lambda () (chdir %orig-cwd))
  ) ;dynamic-wind
) ;define

;; 普通仓库：cwd 自身有 .git 目录
(let ((tmp (make-temp-dir "gf-gitdir-normal-")))
  (let ((git-dir (path-join tmp ".git")))
    (%write-git-head! git-dir "ref: refs/heads/main\n")
    (%with-cwd tmp
      (lambda ()
        (let ((d (detect-git-dir)))
          (check-true (string? d))
          (check-true (path-dir? (path d)))
          (check-true (string=? (path->string (path-name (path d))) ".git"))
        ) ;let
      ) ;lambda
    ) ;%with-cwd
  ) ;let
  (rmtree tmp #t)
) ;let

;; 子目录：向上查找父目录的 .git
(let ((tmp (make-temp-dir "gf-gitdir-parent-")))
  (let* ((git-dir (path-join tmp ".git"))
         (sub (path-join (path-join tmp "a") "b"))
        ) ;
    (%write-git-head! git-dir "ref: refs/heads/main\n")
    (path-mkdir sub #t)
    (%with-cwd (path->string sub)
      (lambda ()
        (let ((d (detect-git-dir)))
          (check-true (string? d))
          (check-true (string=? (path->string (path-name (path d))) ".git"))
        ) ;let
      ) ;lambda
    ) ;%with-cwd
  ) ;let*
  (rmtree tmp #t)
) ;let

;; worktree：.git 是文件，内容 "gitdir: <绝对路径>"
(let ((tmp (make-temp-dir "gf-gitdir-worktree-")))
  (let* ((real-gitdir (path-join tmp "real-git"))
         (wt (path-join tmp "wt"))
        ) ;
    (path-mkdir real-gitdir #t)
    (path-mkdir wt #t)
    (path-write-text (path->string (path-join wt ".git"))
      (string-append "gitdir: " (path->string real-gitdir) "\n")
    ) ;path-write-text
    (%with-cwd (path->string wt)
      (lambda ()
        (let ((d (detect-git-dir)))
          (check-true (string? d))
          (check-true (string=? (path->string (path-resolve (path d)))
                        (path->string (path-resolve real-gitdir))
                      ) ;string=?
          ) ;check-true
        ) ;let
      ) ;lambda
    ) ;%with-cwd
  ) ;let*
  (rmtree tmp #t)
) ;let

;; 非 git 仓库：向上到根都没有 .git → #f
(let ((tmp (make-temp-dir "gf-gitdir-none-")))
  (%with-cwd tmp
    (lambda ()
      (let ((d (detect-git-dir)))
        (check-true (or (string? d) (eq? d #f)))
        (check d => #f)
      ) ;let
    ) ;lambda
  ) ;%with-cwd
  (rmtree tmp #t)
) ;let

;; .git 文件内容不合法（无 gitdir: 前缀）→ #f
(let ((tmp (make-temp-dir "gf-gitdir-badfile-")))
  (path-write-text (path->string (path-join tmp ".git")) "garbage\n")
  (%with-cwd tmp
    (lambda ()
      (check (detect-git-dir) => #f)
    ) ;lambda
  ) ;%with-cwd
  (rmtree tmp #t)
) ;let

;; 性能：探测必须是毫秒级（不跑子进程）
(let ((tmp (make-temp-dir "gf-gitdir-perf-")))
  (let ((git-dir (path-join tmp ".git")))
    (%write-git-head! git-dir "ref: refs/heads/main\n")
    (%with-cwd tmp
      (lambda ()
        (let ((t0 (current-second)))
          (detect-git-dir)
          (detect-git-dir)
          (check-true (< (- (current-second) t0) 0.1))
        ) ;let
      ) ;lambda
    ) ;%with-cwd
  ) ;let
  (rmtree tmp #t)
) ;let

(check-report)
