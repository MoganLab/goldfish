;; %run-values
;; 执行命令并以三个值分别返回标准输出、标准错误和退出码。
;;
;; 语法
;; ----
;; (%run-values command)
;; (%run-values command keyword value ...)
;;
;; 参数
;; ----
;; command : string 或 list
;; 待执行的命令。
;;
;; keyword value ... : 可选
;; 支持 :cwd、:env、:input、:timeout、:stdin、:stdout、:stderr 及对应流模式选项。
;;
;; 返回值
;; ----
;; 三个值：stdout、stderr 和 code。
;; 默认捕获标准输出和标准错误，code 为子进程退出码。

(import (liii check) (liii subprocess run))

(check-set-mode! 'report-failed)

(let-values (((out err code)
              (%run-values '("python3"
                             "-c"
                             "import sys; sys.stdout.write('ok')"))
             ) ;
            ) ;
  (check out => "ok")
  (check err => "")
  (check code => 0)
) ;let-values

(let-values (((out err code)
              (%run-values '("python3"
                             "-c"
                             "import sys; sys.stdout.write('out'); sys.stderr.write('err'); sys.exit(3)")
              ) ;%run-values
             ) ;
            ) ;
  (check out => "out")
  (check err => "err")
  (check code => 3)
) ;let-values

(check-report)
