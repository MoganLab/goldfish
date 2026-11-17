;; pi-config-aio-state
;; 构建 all-in-one 子标签页的状态视图，读取 all-in-one.json 或回退内置默认链。
;;
;; 语法
;; ----
;; (pi-config-aio-state st)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置视图模型。
;;
;; 返回值
;; ------
;; list
;; 包含 source、continuous、maxWaitMs、retryIntervalMs、probeTimeoutMs 与 chains 的 alist。

(import (scheme base)
  (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii pi code modes config state)
  (liii pi code modes config all-in-one)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path-join (path-temp-dir)
    (string-append "pi-cfg-aio-state-" (number->string (getpid)))
  ) ;path-join
) ;define

(when (file-exists? (path->string TMP-DIR))
  (rmtree (path->string TMP-DIR))
) ;when
(path-mkdir TMP-DIR :parents #t :exist-ok #t)

;; 1. 无 all-in-one.json 时回退内置默认链
(let* ((st (pi-config-state (path->string TMP-DIR) :env '()))
       (aio (pi-config-aio-state st))
       (chains (pi-config-aio-chains aio))
      ) ;
  (check (cdr (assoc "source" aio)) => "(builtin default)")
  (check (cdr (assoc "continuous" aio)) => #t)
  (check (length chains) => 2)
  (check (cdr (assoc "id" (car chains))) => "coding-fast")
) ;let*

;; 2. 有 all-in-one.json 时从文件读取，并匹配默认模型与 provider 启用状态
(path-write-text (path-join TMP-DIR "settings.json")
  "{\"defaultProvider\":\"all-in-one\",\"defaultModel\":\"custom-chain\"}"
) ;path-write-text
(path-mkdir (path-join TMP-DIR "models") :parents #t :exist-ok #t)
(path-write-text (path-join TMP-DIR "models" "providers.json")
  "[\"google\",\"all-in-one\"]"
) ;path-write-text
(path-write-text (path-join TMP-DIR "all-in-one.json")
  "{\"provider\":\"all-in-one\",\"continuous\":false,\"maxWait\":120,\"retryInterval\":30,\"probeTimeout\":5,\"chains\":[{\"id\":\"custom-chain\",\"name\":\"自定义链\",\"nodes\":[{\"provider\":\"google\",\"model\":\"gemini-3.8-flash\",\"thinkingLevel\":\"high\"},{\"provider\":\"deepseek\",\"model\":\"deepseek-v4-pro\",\"thinkingLevel\":\"off\"}]}]}"
) ;path-write-text

(let* ((st (pi-config-state (path->string TMP-DIR) :env '(("GEMINI_API_KEY"
                                                           . "k"))))
       (aio (pi-config-aio-state st))
       (chains (pi-config-aio-chains aio))
       (c1 (car chains))
       (nodes (pi-config-aio-chain-nodes c1))
      ) ;
  (check (cdr (assoc "source" aio)) => "all-in-one.json")
  (check (cdr (assoc "continuous" aio)) => #f)
  (check (cdr (assoc "maxWaitMs" aio)) => 120000)
  (check (cdr (assoc "retryIntervalMs" aio)) => 30000)
  (check (cdr (assoc "probeTimeoutMs" aio)) => 5000)
  (check (length chains) => 1)
  (check (cdr (assoc "default" c1)) => #t)
  (check (length nodes) => 2)
  (check (cdr (assoc "enabled" (car nodes))) => #t)
  (check (cdr (assoc "keySource" (car nodes))) => "$GEMINI_API_KEY")
  (check (cdr (assoc "enabled" (cadr nodes))) => #f)
) ;let*

(rmtree (path->string TMP-DIR))

(check-report)
