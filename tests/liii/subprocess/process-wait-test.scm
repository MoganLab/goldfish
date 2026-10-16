(import (liii check) (liii subprocess))

(check-set-mode! 'report-failed)

;; process-wait
;; 阻塞等待子进程退出，可指定超时时间。
;;
;; 语法
;; ----
;; (process-wait proc)
;; (process-wait proc timeout-ms)
;;
;; 参数
;; ----
;; proc : process
;; 进程对象。
;;
;; timeout-ms : integer（可选，默认为 -1 表示无限等待）
;; 超时毫秒数。
;;
;; 返回值
;; ----
;; integer 或 #f
;; 若进程退出则返回退出码整数；若在指定毫秒内未退出则返回 #f。

;; 正常退出并返回退出码
(let ((proc (process-spawn '("python3" "-c" "import sys; sys.exit(42)"))))
  (check (process-wait proc 2000) => 42)
  (process-close proc)
) ;let

;; 短超时返回 #f
(let ((proc (process-spawn '("python3" "-c" "import time; time.sleep(5)"))))
  (let ((ret (process-wait proc 100)))
    (check ret => #f)
  ) ;let
  (process-kill proc)
  (process-wait proc 2000)
  (process-close proc)
) ;let

;; 类型错误
(check-catch 'type-error (process-wait '()))
(check-catch 'type-error
  (let ((p (process-spawn '("python3" "-c" "pass"))))
    (process-wait p "not-an-integer")
  ) ;let
) ;check-catch

(check-report)
