;; pi-provider-file-with-models
;; 生成重写 provider 文件 models 白名单的写入计划，保留其他字段。
;;
;; 语法
;; ----
;; (pi-provider-file-with-models config-dir p-id ids)
;;
;; 参数
;; ----
;; config-dir : string
;; 用户配置目录。
;; p-id : string
;; provider id。
;; ids : list
;; 新的模型 id 列表。
;;
;; 返回值
;; ----
;; pair
;; (相对路径 . JSON 文本)。

(import (liii check) (liii string) (liii pi code config user-providers))

(check-set-mode! 'report-failed)

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"
  ) ;if
) ;define

(let ((write (pi-provider-file-with-models (string-append FIXTURE "/full")
               "kimi-coding"
               '("k3.1-pro" "k3")
             ) ;pi-provider-file-with-models
      ) ;write
     ) ;
  (check (car write) => "models/kimi-coding.json")
  (check (string-contains (cdr write) "\"k3.1-pro\"") => #t)
  (check (string-contains (cdr write) "\"k3\"") => #t)
) ;let

(check-report)
