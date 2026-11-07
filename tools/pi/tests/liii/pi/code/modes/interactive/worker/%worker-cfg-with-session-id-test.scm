;; %worker-cfg-with-session-id
;; 将会话标识附加到 Worker 配置，供请求层用作 prompt_cache_key。
;;
;; 语法
;; ----
;; (%worker-cfg-with-session-id cfg session-file)
;;
;; 参数
;; ----
;; cfg : list?
;; 配置列表。
;; session-file : (or string? #f)
;; 会话文件路径，#f（如 --no-session）表示无会话。
;;
;; 返回值
;; ----
;; list?
;; session-file 为字符串时返回附加 (session-id . 路径) 的新配置（已有
;; session-id 键则替换，不重复）；否则原样返回 cfg。

(import (scheme base)
  (liii check)
  (liii list)
  (liii pi code modes interactive worker)
) ;import

(check-set-mode! 'report-failed)

;; 1. 有会话文件：附加 session-id，其余配置项保留
(let* ((cfg '((provider . "openai") (model-id . "gpt-5.2")))
       (out (%worker-cfg-with-session-id cfg "/tmp/sess-1.jsonl"))
      ) ;
  (check (cdr (assoc 'session-id out)) => "/tmp/sess-1.jsonl")
  (check (cdr (assoc 'provider out)) => "openai")
  (check (cdr (assoc 'model-id out)) => "gpt-5.2")
) ;let*

;; 2. 无会话文件（#f）：原样返回
(let ((cfg '((provider . "openai"))))
  (check (%worker-cfg-with-session-id cfg #f) => cfg)
) ;let

;; 3. 已有 session-id 键：替换而非追加重复
(let* ((cfg '((session-id . "/old.jsonl") (provider . "openai")))
       (out (%worker-cfg-with-session-id cfg "/new.jsonl"))
      ) ;
  (check
    (count (lambda (kv) (eq? (car kv) 'session-id)) out)
    =>
    1
  ) ;check
  (check (cdr (assoc 'session-id out)) => "/new.jsonl")
) ;let*

;; 4. 空配置 + 会话文件：仅含 session-id
(check (%worker-cfg-with-session-id '() "/tmp/s.jsonl")
  =>
  '((session-id . "/tmp/s.jsonl"))
) ;check

(check-report)
