;; pi-resolve-compaction-window
;; 动态解析本次请求前自动压缩判定的上下文容量。
;;
;; 语法
;; ----
;; (pi-resolve-compaction-window cfg)
;;
;; 参数
;; ----
;; cfg : alist
;; 运行配置 alist，含 provider, model-id, config-dir, model-meta, models-store。
;;
;; 返回值
;; ----
;; number | #f
;; 上下文窗口容量（token 数）。

(import (scheme base)
  (liii check)
  (liii path)
  (liii shutil)
  (liii date-time)
  (liii pi ai config record)
  (liii pi code all-in-one chain)
  (liii pi code all-in-one types)
  (liii pi code core compaction flow)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string (path-join (path-temp-dir) "pi-resolve-compaction-window-test"))
) ;define
(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(path-mkdir (path TMP-DIR) :parents #t :exist-ok #t)

;; 1. 常规供应商模型：直接使用其 metadata 的 context-window
(let* ((meta (pi-model-metadata "gemini-3.8-flash" "Gemini" "google"
               "https://..." 1048576 65536
             ) ;pi-model-metadata
       ) ;meta
       (cfg (list (cons 'provider "google")
              (cons 'model-id "gemini-3.8-flash")
              (cons 'model-meta meta)
              (cons 'config-dir TMP-DIR)
            ) ;list
       ) ;cfg
      ) ;
  (check (pi-resolve-compaction-window cfg) => 1048576)
) ;let*

;; 2. all-in-one 默认链（coding-fast: google(1M) -> minimax(1M) -> deepseek(1M)）
;; 无粘性节点时：返回整条链的保守窗口 1000000
(pi-chain-clear-sticky-node! "coding-fast")
(let ((cfg (list (cons 'provider "all-in-one")
             (cons 'model-id "coding-fast")
             (cons 'config-dir TMP-DIR)
           ) ;list
      ) ;cfg
     ) ;
  (check (pi-resolve-compaction-window cfg) => 1000000)
) ;let

;; 3. all-in-one 有有效粘性节点（非冷却期）：返回粘性节点实际窗口
(let* ((node0 (make-chain-node "google" "gemini-3.8-flash" "medium"))
       (cfg (list (cons 'provider "all-in-one")
              (cons 'model-id "coding-fast")
              (cons 'config-dir TMP-DIR)
            ) ;list
       ) ;cfg
      ) ;
  (pi-chain-record-sticky-node! "coding-fast" 0)
  (check (pi-resolve-compaction-window cfg) => 1048576)
) ;let*

;; 4. all-in-one 粘性节点处于冷却期：不能使用过期/故障节点信息，自动回退到整条链的保守窗口
(let* ((node0 (make-chain-node "google" "gemini-3.8-flash" "medium"))
       (cfg (list (cons 'provider "all-in-one")
              (cons 'model-id "coding-fast")
              (cons 'config-dir TMP-DIR)
            ) ;list
       ) ;cfg
      ) ;
  (pi-chain-record-sticky-node! "coding-fast" 0)
  (pi-chain-record-node-failure! node0 (current-monotonic-millis))
  (check (pi-resolve-compaction-window cfg) => 1000000)
) ;let*

;; 5. all-in-one 自定义链含不对称小窗口节点（如 128K 与 1M）：
;; 兜底保守窗口必须是各节点容量的最小值（128K），不能简单写死 1M；
;; 当大窗口节点粘性生效时使用 1M；大窗口节点冷却时回退至 128K。
(path-write-text (path-join (path TMP-DIR) (path "all-in-one.json"))
  "{\n  \"provider\": \"all-in-one\",\n  \"chains\": [\n    {\n      \"id\": \"asym-chain\",\n      \"name\": \"非对称链\",\n      \"nodes\": [\n        {\"provider\": \"zai-coding-cn\", \"model\": \"glm-4.6v\", \"thinkingLevel\": \"off\"},\n        {\"provider\": \"google\", \"model\": \"gemini-3.8-flash\", \"thinkingLevel\": \"medium\"}\n      ]\n    }\n  ]\n}"
) ;path-write-text

(pi-chain-clear-sticky-node! "asym-chain")
(pi-chain-reset-cooldown-state!)
(let ((cfg (list (cons 'provider "all-in-one")
             (cons 'model-id "asym-chain")
             (cons 'config-dir TMP-DIR)
           ) ;list
      ) ;cfg
      (node1 (make-chain-node "google" "gemini-3.8-flash" "medium"))
     ) ;
  ;; 无粘性节点时：保守窗口为 min(128000, 1048576) = 128000
  (check (pi-resolve-compaction-window cfg) => 128000)
  ;; 粘性节点为下标 1（google 1M）：使用实际节点窗口 1048576
  (pi-chain-record-sticky-node! "asym-chain" 1)
  (check (pi-resolve-compaction-window cfg) => 1048576)
  ;; 粘性节点 1 进入冷却期：回退至保守窗口 128000
  (pi-chain-record-node-failure! node1 (current-monotonic-millis))
  (check (pi-resolve-compaction-window cfg) => 128000)
) ;let

;; 清理粘性与故障状态
(pi-chain-clear-sticky-node! "asym-chain")
(pi-chain-clear-sticky-node! "coding-fast")
(pi-chain-reset-cooldown-state!)
(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)
