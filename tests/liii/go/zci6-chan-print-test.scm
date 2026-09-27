(import (liii check)
        (liii go))
(check-set-mode! 'report-failed)
;; 故意失败的 check，迫使 check 报告打印 channel 对象（走 channel_to_string_glue）
;; 若打印崩溃 => 进程异常退出码；若打印正常 => exit -1（测试失败但非崩溃）
(check (make-chan 1) => #t)
(check-report)
