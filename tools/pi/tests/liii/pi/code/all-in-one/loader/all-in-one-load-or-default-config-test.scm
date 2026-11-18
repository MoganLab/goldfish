;; all-in-one-load-or-default-config
;; 统一读取入口：读到有效用户配置就用它，否则回退默认配置记录。
;;
;; 语法
;; ----
;; (all-in-one-load-or-default-config . opt-config-dir)
;;
;; 参数
;; ----
;; opt-config-dir : (or string path #f) (可选)
;; 可选配置目录，未提供或为 #f 时使用默认的 ~/.gf/agent/all-in-one.json。
;;
;; 返回值
;; ----
;; <all-in-one-cfg>
;; 始终返回可用的配置记录，永不返回 #f。
;;
;; 说明
;; ----
;; 与 all-in-one-load-config 的分工：后者表达“是否成功读到用户配置”，
;; 未读到或校验失败时返回 #f；本函数是请求、压缩摘要与配置 TUI 的
;; 统一回退入口，负责把 #f 折叠成默认记录，从而消除各调用方重复构造
;; (make-all-in-one-cfg #t 3600000 60000 8000 ...) 的写法。
;; 回退只在“没读到”或“读到的配置没有任何链”时发生，且不写任何文件。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii pi code all-in-one loader)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "aio-load-or-default-test-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(define (write-cfg content)
  (when (file-exists? TMP-DIR)
    (rmtree TMP-DIR #t)
  ) ;when
  (path-mkdir (path TMP-DIR) :parents #t :exist-ok #t)
  (let ((cfg-file (path-join TMP-DIR "all-in-one.json")))
    (call-with-output-file (path->string cfg-file) (lambda (p) (display content p)))
  ) ;let
) ;define

(define (clean-dir!)
  (when (file-exists? TMP-DIR)
    (rmtree TMP-DIR #t)
  ) ;when
) ;define

;; 1. 没有配置文件：返回默认记录，且不创建文件
(clean-dir!)
(let ((cfg (all-in-one-load-or-default-config TMP-DIR)))
  (check-true (all-in-one-cfg? cfg))
  (check (all-in-one-cfg-continuous? cfg) => #t)
  (check (all-in-one-cfg-max-wait-ms cfg) => 3600000)
  (check (all-in-one-cfg-retry-interval-ms cfg) => 60000)
  (check (all-in-one-cfg-probe-timeout-ms cfg) => 8000)
  (check (length (all-in-one-cfg-chains cfg)) => 2)
  (check-false (file-exists? (path->string (path-join TMP-DIR "all-in-one.json")))
  ) ;check-false
) ;let

;; 2. 有有效用户配置：使用用户配置，而不是默认值
(write-cfg "{\"provider\":\"all-in-one\",\"continuous\":false,\"retryIntervalMs\":5000,\"probeTimeoutMs\":3000,\"chains\":[{\"id\":\"user-chain\",\"nodes\":[{\"provider\":\"p1\",\"model\":\"m1\"}]}]}"
) ;write-cfg
(let* ((cfg (all-in-one-load-or-default-config TMP-DIR))
       (chains (all-in-one-cfg-chains cfg))
      ) ;
  (check (all-in-one-cfg-continuous? cfg) => #f)
  (check (all-in-one-cfg-retry-interval-ms cfg) => 5000)
  (check (all-in-one-cfg-probe-timeout-ms cfg) => 3000)
  (check (length chains) => 1)
  (check (chain-def-id (car chains)) => "user-chain")
) ;let*

;; 3. 配置非法（根 provider 不是 all-in-one）时回退默认记录
(write-cfg "{\"provider\":\"openai\"}")
(let ((cfg (all-in-one-load-or-default-config TMP-DIR)))
  (check-true (all-in-one-cfg? cfg))
  (check (length (all-in-one-cfg-chains cfg)) => 2)
  (check (chain-def-id (car (all-in-one-cfg-chains cfg))) => "coding-fast")
) ;let

;; 4. JSON 语法错误时同样回退默认记录，不抛异常
(write-cfg "{ broken json")
(let ((cfg (all-in-one-load-or-default-config TMP-DIR)))
  (check-true (all-in-one-cfg? cfg))
  (check (all-in-one-cfg-max-wait-ms cfg) => 3600000)
) ;let

;; 5. chains 数组非法时回退默认记录
(write-cfg "{\"provider\":\"all-in-one\",\"chains\":\"invalid\"}")
(let ((cfg (all-in-one-load-or-default-config TMP-DIR)))
  (check-true (all-in-one-cfg? cfg))
  (check (length (all-in-one-cfg-chains cfg)) => 2)
) ;let

;; 6. 配置存在但链为空：按“无有效链”回退默认记录
(write-cfg "{\"provider\":\"all-in-one\",\"chains\":[]}")
(let ((cfg (all-in-one-load-or-default-config TMP-DIR)))
  (check-true (all-in-one-cfg? cfg))
  (check (length (all-in-one-cfg-chains cfg)) => 2)
) ;let

;; 7. 回退过程绝不覆盖已有用户文件
(write-cfg "{\"provider\":\"openai\"}")
(let ((before (path-read-text (path->string (path-join TMP-DIR "all-in-one.json")))))
  (all-in-one-load-or-default-config TMP-DIR)
  (check (path-read-text (path->string (path-join TMP-DIR "all-in-one.json")))
    =>
    before
  ) ;check
) ;let

(clean-dir!)

(check-report)
