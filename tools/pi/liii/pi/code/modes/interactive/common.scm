;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code modes interactive common)
  (export %keyword-value detect-git-branch detect-git-dir make-branch-watcher
    stop-branch-watcher! git-branch-from-head-file
  ) ;export
  (import (scheme base) (liii string) (liii path) (liii fs-watch))
  (begin

    ;; 在 dir 下找 .git：目录 → 普通仓库；文件 → 读首行 "gitdir: <path>"
    ;; 解析 worktree/submodule 的 gitdir（相对路径相对 dir 解析）。
    ;; 返回 gitdir 字符串路径，无法识别时返回 #f
    (define (%git-dir-at dir)
      (let ((dot-git (path-join dir ".git")))
        (cond ((path-dir? dot-git) (path->string dot-git))
              ((path-file? dot-git)
               (let* ((text (path-read-text (path->string dot-git)))
                      (first-line
                        (if (string? text) (string-trim-both (car (string-split text "\n"))) "")
                      ) ;first-line
                     ) ;
                 (if (string-starts? first-line "gitdir:")
                   (let ((p (string-trim-both (substring first-line 7 (string-length first-line)))))
                     (if (string-null? p)
                       #f
                       (path->string (if (path-absolute? (path p)) (path p) (path-join dir p)))
                     ) ;if
                   ) ;let
                   #f
                 ) ;if
               ) ;let*
              ) ;
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    ;; 从 cwd 向上逐级查找 .git，到根仍未找到返回 #f
    (define (%find-git-dir-upward)
      (let loop
        ((dir (path-cwd)))
        (or (%git-dir-at dir)
          (let ((parent (path-parent dir)))
            (if (string=? (path->string parent) (path->string dir)) #f (loop parent))
          ) ;let
        ) ;or
      ) ;let
    ) ;define

    ;; 检测当前工作区所在的 git 分支名称，探测失败时返回 #f。
    ;; 纯文件读取（detect-git-dir + 读 HEAD），零子进程，毫秒级：
    ;; 启动首帧 footer 与 branch_changed 事件路径同一套解析逻辑
    (define (detect-git-branch)
      (catch #t
        (lambda ()
          (let ((git-dir (%find-git-dir-upward)))
            (and git-dir (git-branch-from-head-file git-dir))
          ) ;let
        ) ;lambda
        (lambda args #f)
      ) ;catch
    ) ;define

    ;; 检测当前工作区 .git 目录的绝对路径（含 worktree 等非 .git 子目录场景），
    ;; 探测失败（非 git 仓库 / .git 文件无法解析）时返回 #f。
    ;; 与 git rev-parse --absolute-git-dir 的差异：不支持 GIT_DIR 环境
    ;; 变量与 bare repo（返回 #f，纯装饰性降级）
    (define (detect-git-dir)
      (catch #t (lambda () (%find-git-dir-upward)) (lambda args #f))
    ) ;define

    ;; 直接读 <git-dir>/HEAD 文件解析当前分支（零子进程）：
    ;; "ref: refs/heads/<branch>" → branch；detached HEAD（40 位 hex）→ #f。
    ;; 用于 branch_changed 事件路径：CI 上子进程与后台 watcher 并发时
    ;; 存在退出竞态（tb_process_wait 无界等待），事件路径不跑子进程
    (define (git-branch-from-head-file git-dir)
      (catch #t
        (lambda ()
          (let* ((text (path-read-text (string-append git-dir "/HEAD")))
                 (lines (if (string? text) (string-split text "\n") '()))
                 (line (if (pair? lines) (string-trim-both (car lines)) ""))
                ) ;
            (if (string-starts? line "ref: refs/heads/")
              (let ((b (string-trim-both (substring line 16 (string-length line)))))
                (if (string-null? b) #f b)
              ) ;let
              #f
            ) ;if
          ) ;let*
        ) ;lambda
        (lambda args #f)
      ) ;catch
    ) ;define

    ;; 分支监听回调：在 (liii go) worker 的独立 s7 会话内执行！
    ;; 源码经 procedure-source 序列化后 worker 会话 eval 重建，因此：
    ;; 1. 自由符号只能是 (scheme base) 与 (liii go)/(liii fs-watch) 的导出绑定，
    ;;    不能引用本库辅助函数（如 string-contains?）或闭包捕获主会话对象；
    ;; 2. ctx 为可序列化值 (list event-chan git-dir)，channel 由 C 层共享。
    ;; 事件过滤：git checkout 走 HEAD.lock→HEAD 的 rename（moved 看 dest），
    ;; 原地改写 HEAD 是 modified（看 path）；macOS kqueue 后端只有目录级事件
    ;; （path 即监听目录本身），无法按文件过滤，直接放行。
    ;; 消息形态为 (branch_changed git-dir) 列表，与 event-chan 其余事件语法
    ;; 一致（主会话以 (case (car ev) ...) 分发）；携带 git-dir 使主会话
    ;; 能直接读 HEAD 文件而不必跑子进程；
    ;; chan-send! 超时/异常由 fs-watch-dispatch! 统一吞掉，不会拖垮监听泵
    (define %branch-watch-callback
      (lambda (ctx ev)
        (let ((ch (car ctx)) (git-dir (cadr ctx)))
          (let ((ends-head?
                  (lambda (s)
                    (and (string? s)
                      (>= (string-length s) 4)
                      (string=? "HEAD" (substring s (- (string-length s) 4) (string-length s)))
                    ) ;and
                  ) ;lambda
                ) ;ends-head?
               ) ;
            (let ((p (fs-event-path ev)) (d (fs-event-dest-path ev)))
              (when (or (string=? p git-dir) (ends-head? p) (ends-head? d))
                (chan-send! ch (list 'branch_changed git-dir) 5000)
              ) ;when
            ) ;let
          ) ;let
        ) ;let
      ) ;lambda
    ) ;define

    ;; 创建分支切换监听器：监听 git-dir 目录，HEAD 相关变更时 worker 回调
    ;; 经 channel 向 ch 发送 'branch_changed 信号，由主会话事件轮询消费。
    ;; git-dir 非法或启动失败时返回 #f（非 git 仓库等场景优雅降级）
    (define (make-branch-watcher ch git-dir)
      (catch #t
        (lambda ()
          (if (and (string? git-dir) (not (string-null? git-dir)))
            (let* ((obs (make-fs-observer))
                   (handler (make-fs-event-handler
                              :on-modified %branch-watch-callback
                              :on-created  %branch-watch-callback
                              :on-deleted  %branch-watch-callback
                              :on-moved    %branch-watch-callback
                            ) ;make-fs-event-handler
                   ) ;handler
                  ) ;
              (fs-observer-schedule! obs handler git-dir :context (list ch git-dir))
              (fs-observer-start! obs)
              obs
            ) ;let*
            #f
          ) ;if
        ) ;lambda
        (lambda args #f)
      ) ;catch
    ) ;define

    ;; 停止分支监听（幂等，容忍 #f 与异常）；C 层 watcher 撤销后
    ;; 后台泵收到 stopped 自行退出；返回 #t 表示实际执行了 stop
    (define (stop-branch-watcher! obs)
      (catch #t
        (lambda ()
          (if (and obs (fs-observer? obs)) (begin (fs-observer-stop! obs) #t) #f)
        ) ;lambda
        (lambda args #f)
      ) ;catch
    ) ;define

    ;; 从关键字参数列表中取出 key 对应的值，取不到时返回 default
    (define (%keyword-value key opts default)
      (let loop
        ((rest opts))
        (cond ((null? rest) default)
              ((and (pair? (cdr rest)) (eq? (car rest) key)) (cadr rest))
              ((null? (cdr rest)) default)
              (else (loop (cddr rest)))
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
