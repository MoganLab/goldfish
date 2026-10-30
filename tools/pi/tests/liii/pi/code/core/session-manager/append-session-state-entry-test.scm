(import (liii check)
  (liii path)
  (liii json)
  (liii pi code core session-manager)
  (liii pi code core compaction session)
) ;import

(check-set-mode! 'report-failed)

;; append-session-state-entry!
;; 将模型 / 思考等级等状态条目写入会话 JSONL 文件。
;;
;; 语法
;; ----
;; (append-session-state-entry! session-file type fields)
;;
;; 参数
;; ----
;; session-file : string | path | #f
;; 目标会话文件；为 #f 时不执行写入并返回 #f。
;; type : string
;; 条目类型，如 "model_change" / "thinking_level_change"；不做白名单校验。
;; fields : list
;; 业务字段 alist（字符串键），平铺在条目顶层。
;;
;; 返回值
;; ----
;; 新条目 id（string）；未写入时返回 #f。

(define (make-state-session-test-file name)
  (let* ((tmp-dir (path->string (path-join (path-temp-dir) name)))
         (session-file (path->string (path-join tmp-dir "session.jsonl")))
        ) ;
    (if (file-exists? tmp-dir) (path-rmdir tmp-dir))
    (path-mkdir tmp-dir :parents #t :exist-ok #t)
    (save-session-header! session-file "sess-state" "2026-10-30T08:00:00.000Z" tmp-dir)
    session-file
  ) ;let*
) ;define

(define (cleanup-state-session-test-file session-file)
  (let ((tmp-dir (path->string (path-parent session-file))))
    (when (file-exists? session-file) (delete-file session-file))
    (path-rmdir tmp-dir)
  ) ;let
) ;define

;; ------------------------------------------------------------
;; 一、条目字段：type/id/parentId/timestamp 齐全，业务字段平铺
;; ------------------------------------------------------------

(let* ((session-file (make-state-session-test-file "test-pi-session-state-fields"))
       (model-id "a\"b\\c")
       (mid (append-session-state-entry! session-file
              "model_change"
              (list (cons "provider" "anthropic") (cons "model-id" model-id))
            ) ;append-session-state-entry!
      ) ;mid
     ) ;
  (check-true (string? mid))
  (let* ((entries (pi-read-session-entries session-file))
         (entry (cadr entries))
        ) ;
    ;; header 之外只有一条状态条目，且被 string->json 完整读回
    (check (length entries) => 2)
    (check (cdr (assoc "type" entry)) => "model_change")
    (check (cdr (assoc "id" entry)) => mid)
    ;; parentId 指向文件中上一条（header）
    (check (cdr (assoc "parentId" entry)) => "sess-state")
    (check-true (string? (cdr (assoc "timestamp" entry))))
    ;; 业务字段平铺在顶层，不嵌套
    (check (cdr (assoc "provider" entry)) => "anthropic")
    (check (cdr (assoc "model-id" entry)) => model-id)
    (check-false (assoc "fields" entry))
  ) ;let*
  (cleanup-state-session-test-file session-file)
) ;let*

;; ------------------------------------------------------------
;; 二、连续写入两类状态条目：读回顺序与写入顺序一致，链不断
;; ------------------------------------------------------------

(let* ((session-file (make-state-session-test-file "test-pi-session-state-order"))
       (model-id (append-session-state-entry! session-file
                   "model_change"
                   (list (cons "provider" "openai") (cons "model-id" "gpt-5"))
                 ) ;append-session-state-entry!
       ) ;model-id
       (thinking-id (append-session-state-entry! session-file
                      "thinking_level_change"
                      (list (cons "level" "high"))
                    ) ;append-session-state-entry!
       ) ;thinking-id
      ) ;
  (check-true (string? thinking-id))
  (let* ((entries (pi-read-session-entries session-file))
         (first (cadr entries))
         (second (caddr entries))
        ) ;
    (check (length entries) => 3)
    (check (cdr (assoc "type" first)) => "model_change")
    (check (cdr (assoc "id" first)) => model-id)
    (check (cdr (assoc "parentId" first)) => "sess-state")
    (check (cdr (assoc "type" second)) => "thinking_level_change")
    (check (cdr (assoc "id" second)) => thinking-id)
    ;; 第二条链接到第一条，保持线性链
    (check (cdr (assoc "parentId" second)) => model-id)
    (check (cdr (assoc "level" second)) => "high")
  ) ;let*
  (cleanup-state-session-test-file session-file)
) ;let*

;; ------------------------------------------------------------
;; 三、空业务字段与未知 type：不做白名单校验，照常写入
;; ------------------------------------------------------------

(let* ((session-file (make-state-session-test-file "test-pi-session-state-custom"))
       (mid (append-session-state-entry! session-file "custom_state" '()))
      ) ;
  (check-true (string? mid))
  (let* ((entries (pi-read-session-entries session-file))
         (entry (cadr entries))
        ) ;
    (check (cdr (assoc "type" entry)) => "custom_state")
    (check (cdr (assoc "id" entry)) => mid)
    ;; 空 fields 时只有 type/id/parentId/timestamp 四个键
    (check (length entry) => 4)
  ) ;let*
  (cleanup-state-session-test-file session-file)
) ;let*

;; ------------------------------------------------------------
;; 四、--no-session（sfile 为 #f）：静默返回 #f
;; ------------------------------------------------------------

(check-false (append-session-state-entry! #f
               "model_change"
               (list (cons "provider" "p") (cons "model-id" "m"))
             ) ;append-session-state-entry!
) ;check-false

(check-report)
