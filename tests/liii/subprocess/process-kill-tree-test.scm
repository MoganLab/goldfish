(import (liii check) (liii either) (liii os) (liii subprocess))

(check-set-mode! 'report-failed)

;; process-kill-tree
;; 级联终止指定进程及其所有派生子孙进程。
;;
;; 语法
;; ----
;; (process-kill-tree proc-or-pid)
;; (process-kill-tree proc-or-pid keyword value ...)
;;
;; 参数
;; ----
;; proc-or-pid : process 或 positive integer
;; 进程对象或正整数 PID。
;;
;; keyword value ... : 可选
;; :signal — 终止信号，支持 'sigkill（默认）或 'sigterm。
;; :timeout — 优雅退出缓冲时间（秒），默认 3 秒；若在 timeout 内未完全退出，自动升级为 SIGKILL 强杀。
;;
;; 返回值
;; ----
;; boolean : 成功终止返回 #t。
;;
;; 说明
;; ----
;; 级联终止目标进程及其创建的所有子孙进程，彻底杜绝孤儿进程与僵尸进程遗留。
;; POSIX 平台上基于独立进程组（Process Group）通过广播信号实现整组清理；
;; Windows 平台上基于 Job Object 机制级联关闭或 taskkill /F /T /PID 强杀。
;; 若传入已经退出的进程对象或不存在的有效 PID，优雅返回 #t。

;; 测试项4：已退出进程或非法 PID 的防御性健壮处理
(let ((proc (process-spawn '("python3" "-c" "import sys; sys.exit(0)"))))
  (check (process-wait proc 2000) => 0)
  (check (process-alive? proc) => #f)
  ;; 针对已经正常退出的进程对象调用，安全返回 #t
  (check (process-kill-tree proc) => #t)
  (process-close proc)
) ;let

;; 针对不存在的孤立有效 PID 调用，优雅处理返回 #t
(check (process-kill-tree 999999) => #t)

;; 参数类型与取值防御校验
(check-catch 'type-error (process-kill-tree "invalid"))
(check-catch 'type-error (process-kill-tree #f))
(check-catch 'type-error (process-kill-tree 'sym))
(check-catch 'type-error (process-kill-tree 3.14))

(check-catch 'value-error (process-kill-tree 0))
(check-catch 'value-error (process-kill-tree -1))
(check-catch 'value-error (process-kill-tree -100))

(check-catch 'type-error (process-kill-tree 12345 :signal 123))
(check-catch 'type-error (process-kill-tree 12345 :signal "sigkill"))
(check-catch 'value-error (process-kill-tree 12345 :signal 'sighup))
(check-catch 'value-error (process-kill-tree 12345 :signal 'invalid))

(check-catch 'type-error (process-kill-tree 12345 :timeout 'fast))
(check-catch 'type-error (process-kill-tree 12345 :timeout "3"))
(check-catch 'value-error (process-kill-tree 12345 :timeout -1))
(check-catch 'value-error (process-kill-tree 12345 :timeout -0.1))

;; POSIX 平台级联终止专项测试
(when (or (os-linux?) (os-macos?))
  ;; 测试项1：三层进程派生级联清理测试（进程对象）
  (let* ((proc (process-spawn '("sh" "-c" "sh -c 'sleep 30'")))
         (pid (process-pid proc))
        ) ;
    (check (process-alive? proc) => #t)
    (check (process-kill-tree proc) => #t)
    (let ((ret (process-wait proc 2000)))
      (check (integer? ret) => #t)
      (check (process-alive? proc) => #f)
    ) ;let
    ;; 验证子孙进程已完全清理
    (let ((res (run-either (string-append "pgrep -P " (number->string pid)))))
      (check
        (if (either-right? res)
          (zero? (string-length (to-right res)))
          (zero? (string-length (process-error-stdout (to-left res))))
        ) ;if
        =>
        #t
      ) ;check
    ) ;let
    (process-close proc)
  ) ;let*

  ;; 测试项1补充：三层进程派生级联清理测试（PID 直接调用）
  (let* ((proc (process-spawn '("sh" "-c" "sh -c 'sleep 30'")))
         (pid (process-pid proc))
        ) ;
    (check (process-alive? proc) => #t)
    (check (process-kill-tree pid) => #t)
    (let ((ret (process-wait proc 2000)))
      (check (integer? ret) => #t)
      (check (process-alive? proc) => #f)
    ) ;let
    (let ((res (run-either (string-append "pgrep -P " (number->string pid)))))
      (check
        (if (either-right? res)
          (zero? (string-length (to-right res)))
          (zero? (string-length (process-error-stdout (to-left res))))
        ) ;if
        =>
        #t
      ) ;check
    ) ;let
    (process-close proc)
  ) ;let*

  ;; 测试项2：优雅退出缓冲（Graceful SIGTERM）
  (let ((proc (process-spawn '("sh" "-c" "sleep 30"))))
    (check (process-alive? proc) => #t)
    (check (process-kill-tree proc :signal 'sigterm :timeout 1) => #t)
    (let ((ret (process-wait proc 2000)))
      (check (integer? ret) => #t)
      (check (process-alive? proc) => #f)
    ) ;let
    (process-close proc)
  ) ;let

  ;; 测试项2：忽略 SIGTERM 的子进程，缓冲超时到达后自动升级为 SIGKILL 强杀
  (let ((proc (process-spawn '("sh" "-c" "trap '' TERM; sleep 30"))))
    (check (process-alive? proc) => #t)
    (check (process-kill-tree proc :signal 'sigterm :timeout 0.3) => #t)
    (let ((ret (process-wait proc 2000)))
      (check (integer? ret) => #t)
      (check (process-alive? proc) => #f)
    ) ;let
    (process-close proc)
  ) ;let

  ;; 测试项3：run 超时默认级联清理测试
  (check (run "sh -c 'sh -c \"sleep 10\"'" :timeout 1) => -1)
) ;when

;; 跨平台通用测试：基于 Python 多级子进程
(let ((proc (process-spawn '("python3"
                             "-c"
                             "import subprocess, time; subprocess.Popen(['python3', '-c', 'import time; time.sleep(30)']); time.sleep(30)")
            ) ;process-spawn
      ) ;proc
     ) ;
  (check (process-alive? proc) => #t)
  (check (process-kill-tree proc) => #t)
  (let ((ret (process-wait proc 2000)))
    (check (integer? ret) => #t)
    (check (process-alive? proc) => #f)
  ) ;let
  (process-close proc)
) ;let

(check-report)
