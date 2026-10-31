(import (scheme base)
  (liii check)
  (liii string)
  (liii path)
  (liii os)
  (liii tempfile)
  (liii shutil)
  (liii pi code modes interactive common))

(check-set-mode! 'report-failed)

;; detect-git-branch
;; 纯文件读取探测当前分支名（零子进程）：detect-git-dir + 读 HEAD 文件。
;;
;; 语法
;; ----
;; (detect-git-branch) → string? | #f
;;
;; 返回值
;; -----
;; string?
;; 当前分支名（HEAD 为 "ref: refs/heads/<branch>"）。
;; #f
;; 非 git 仓库、detached HEAD（HEAD 为 40 位 hex）时返回 #f。
;;
;; 说明
;; ----
;; 与 branch_changed 事件路径的 git-branch-from-head-file 同一套解析逻辑，
;; 启动首帧 footer 与事件更新路径行为一致。

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

;; 普通分支：HEAD 为 "ref: refs/heads/<branch>"
(let ((tmp (make-temp-dir "gf-gitbranch-normal-")))
  (%write-git-head! (path-join tmp ".git") "ref: refs/heads/feature-x\n")
  (%with-cwd tmp
    (lambda ()
      (check (detect-git-branch) => "feature-x")
    ) ;lambda
  ) ;%with-cwd
  (rmtree tmp #t)
) ;let

;; 子目录：向上找到父目录 .git 后读 HEAD
(let ((tmp (make-temp-dir "gf-gitbranch-parent-")))
  (let* ((git-dir (path-join tmp ".git"))
         (sub (path-join (path-join tmp "x") "y"))
        ) ;
    (%write-git-head! git-dir "ref: refs/heads/dev\n")
    (path-mkdir sub #t)
    (%with-cwd (path->string sub)
      (lambda ()
        (check (detect-git-branch) => "dev")
      ) ;lambda
    ) ;%with-cwd
  ) ;let*
  (rmtree tmp #t)
) ;let

;; detached HEAD：40 位 hex → #f
(let ((tmp (make-temp-dir "gf-gitbranch-detached-")))
  (%write-git-head! (path-join tmp ".git")
    "0123456789abcdef0123456789abcdef01234567\n"
  ) ;%write-git-head!
  (%with-cwd tmp
    (lambda ()
      (check (detect-git-branch) => #f)
    ) ;lambda
  ) ;%with-cwd
  (rmtree tmp #t)
) ;let

;; 非 git 仓库 → #f
(let ((tmp (make-temp-dir "gf-gitbranch-none-")))
  (%with-cwd tmp
    (lambda ()
      (check (detect-git-branch) => #f)
    ) ;lambda
  ) ;%with-cwd
  (rmtree tmp #t)
) ;let

;; worktree：.git 文件指向的 gitdir 中读 HEAD
(let ((tmp (make-temp-dir "gf-gitbranch-worktree-")))
  (let* ((real-gitdir (path-join tmp "real-git"))
         (wt (path-join tmp "wt"))
        ) ;
    (path-mkdir real-gitdir #t)
    (path-mkdir wt #t)
    (path-write-text (path->string (path-join real-gitdir "HEAD"))
      "ref: refs/heads/wt-branch\n"
    ) ;path-write-text
    (path-write-text (path->string (path-join wt ".git"))
      (string-append "gitdir: " (path->string real-gitdir) "\n")
    ) ;path-write-text
    (%with-cwd (path->string wt)
      (lambda ()
        (check (detect-git-branch) => "wt-branch")
      ) ;lambda
    ) ;%with-cwd
  ) ;let*
  (rmtree tmp #t)
) ;let

(check-report)
