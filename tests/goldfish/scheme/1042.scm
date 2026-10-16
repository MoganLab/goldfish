(import (liii check) (liii subprocess) (scheme base))

(check-set-mode! 'report-failed)

;; [1042] 验证直接以 gf <file> 执行脚本时管道标准输入与 current-input-port 行为

;; 测试项1：管道输入通过 gf <file> 脚本正常读取单行数据
(let ((proc (process-spawn '("bin/gf" "tests/resources/1042_stdin_line.scm")
              :stdin  'pipe
              :stdout 'pipe
            ) ;process-spawn
      ) ;proc
     ) ;
  (process-write-stdin proc "hello stdin\n")
  (process-close-stdin proc)
  (check (process-read-line proc :stream 'stdout) => "hello stdin")
  (process-close proc)
  (check (process-wait proc 3000) => 0)
) ;let

;; 测试项2：管道输入通过 gf <file> 脚本逐字符读取与 EOF 判定
(let ((proc (process-spawn '("bin/gf" "tests/resources/1042_stdin_char.scm")
              :stdin  'pipe
              :stdout 'pipe
            ) ;process-spawn
      ) ;proc
     ) ;
  (process-write-stdin proc "A")
  (process-close-stdin proc)
  (check (process-read-line proc :stream 'stdout) => "#\\A")
  (check (process-read-line proc :stream 'stdout) => "#<eof>")
  (process-close proc)
  (check (process-wait proc 3000) => 0)
) ;let

;; 测试项3：验证 current-input-port 在脚本顶层执行时严格为标准输入 (*stdin*)
(let ((proc (process-spawn '("bin/gf" "tests/resources/1042_stdin_port.scm") :stdout 'pipe)
      ) ;proc
     ) ;
  (check (process-read-line proc :stream 'stdout) => "*stdin*")
  (process-close proc)
  (check (process-wait proc 3000) => 0)
) ;let

;; 测试项4：验证 gf run 与直接 gf <file> 两种运行模式输入行为一致
(let ((proc-direct (process-spawn '("bin/gf" "tests/resources/1042_compare.scm")
                     :stdin  'pipe
                     :stdout 'pipe
                   ) ;process-spawn
      ) ;proc-direct
      (proc-run (process-spawn '("bin/gf"
                                 "run"
                                 "tests/resources/1042_compare.scm")
                  :stdin  'pipe
                  :stdout 'pipe
                ) ;process-spawn
      ) ;proc-run
     ) ;
  (process-write-stdin proc-direct "test data\n")
  (process-close-stdin proc-direct)
  (process-write-stdin proc-run "test data\n")
  (process-close-stdin proc-run)
  (let ((out-direct (process-read-line proc-direct :stream 'stdout))
        (out-run (process-read-line proc-run :stream 'stdout))
       ) ;
    (check out-direct => "test data")
    (check out-run => "test data")
  ) ;let
  (process-close proc-direct)
  (process-close proc-run)
  (check (process-wait proc-direct 3000) => 0)
  (check (process-wait proc-run 3000) => 0)
) ;let

(check-report)
