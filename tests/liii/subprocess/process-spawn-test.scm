(import (liii check) (liii subprocess) (liii os))

(check-set-mode! 'report-failed)

;; process-spawn
;; 异步派生子进程，并返回一个子进程对象。
;;
;; 语法
;; ----
;; (process-spawn command)
;; (process-spawn command keyword value ...)
;;
;; 参数
;; ----
;; command : string 或 list
;; 可执行命令，字符串形式或者首元素为可执行程序的列表。
;;
;; keyword value ... : 可选
;; :cwd — 工作目录字符串。
;; :env — 环境变量键值对列表 '(("KEY" . "VAL") ...)。
;; :stdin — 'pipe、'null、'inherit 或文件路径字符串。
;; :stdout — 'pipe、'capture、'discard、'inherit 或文件路径字符串。
;; :stderr — 'pipe、'capture、'discard、'inherit、'stdout 或文件路径字符串。
;;
;; 返回值
;; ----
;; process
;; 新创建的子进程对象。

;; 异步派生并等待进程
(let ((proc (process-spawn '("python3" "-c" "import sys; sys.exit(0)"))))
  (check (process? proc) => #t)
  (check (process-wait proc 2000) => 0)
  (process-close proc)
) ;let

;; 带重定向启动
(let ((proc (process-spawn '("python3" "-c" "import sys; sys.exit(0)")
              :stdin  'null
              :stdout 'discard
              :stderr 'discard
            ) ;process-spawn
      ) ;proc
     ) ;
  (check (process? proc) => #t)
  (check (process-wait proc 2000) => 0)
  (process-close proc)
) ;let

;; 管道模式
(let ((proc (process-spawn '("python3" "-c" "print('hello', flush=True)") :stdout 'pipe)
      ) ;proc
     ) ;
  (check (process? proc) => #t)
  (check (process-read-line proc :stream 'stdout) => "hello")
  (process-close proc)
  (process-wait proc 2000)
) ;let

;; 非法参数错误检查
(check-catch 'value-error (process-spawn '("python3") :stdout 'invalid-sym))
(check-catch 'value-error (process-spawn '("python3") :stderr 'invalid-sym))
(check-catch 'value-error (process-spawn '("python3") :stdin 'invalid-sym))
(check-catch 'type-error (process-spawn 123))

(check-report)
