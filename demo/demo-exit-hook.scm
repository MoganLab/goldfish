(import (liii base))

;; demo: exit-hook 的基本用法
;;
;; 展示如何在进程退出时通过 *exit-hook* 执行清理逻辑。

;; 注册一个 exit-hook 处理函数
(set! (hook-functions *exit-hook*)
  (cons (lambda (hook)
          (display "exit-hook triggered with code: ")
          (display (hook 'code))
          (newline)
        ) ;lambda
    (hook-functions *exit-hook*)
  ) ;cons
) ;set!

;; 也可以注册多个处理函数，它们会按顺序执行
(set! (hook-functions *exit-hook*)
  (cons (lambda (hook) (display "Second hook: process is exiting...") (newline))
    (hook-functions *exit-hook*)
  ) ;cons
) ;set!

(display "Exiting...")
(newline)

;; 正常退出时，exit-hook 会自动触发所有注册的处理函数
