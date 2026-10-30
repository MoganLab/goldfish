(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii shutil)
  (liii string)
  (liii list)
  (liii json)
  (srfi srfi-13)
  (liii pi ai message)
  (liii pi code core session-manager)
  (liii pi code core session-projection)
) ;import

(check-set-mode! 'report-failed)

;; context_edit 投影：删除与替换
;; 条目形如 {type:"context_edit", targetId, replacement:{content: <字符串|块数组>} | null}：
;; - replacement 为 JSON null（符号 'null）或字段缺失 → 删除目标，目标不进入上下文
;; - replacement 非 null → 只替换目标消息的 content，role 与其他元数据保留
;; - replacement 形态不合法（裸值、缺 content、content 非字符串或块数组）→ 不应用，计入 ignored-edits
;; - 同一目标多次编辑时，路径上最后一条胜出
;; - 编辑作用在裁剪之后的条目上，先于消息构造
;; - 目标不在裁剪结果内时忽略该编辑，计入诊断 ignored-edits

;; 建一个 message 条目：role 为 user / assistant，content 原样放入

(define (make-message id parent-id role content)
  (list (cons "type" "message")
    (cons "id" id)
    (cons "parentId" parent-id)
    (cons "message" (list (cons "role" role) (cons "content" content)))
  ) ;list
) ;define

(define (make-user id parent-id text)
  (make-message id parent-id "user" text)
) ;define

;; 建一个 assistant 条目：content 为块数组，带 stopReason

(define (make-assistant id parent-id text stop-reason)
  (list (cons "type" "message")
    (cons "id" id)
    (cons "parentId" parent-id)
    (cons "message"
      (list (cons "role" "assistant")
        (cons "content" (list (list (cons "type" "text") (cons "text" text))))
        (cons "stopReason" stop-reason)
      ) ;list
    ) ;cons
  ) ;list
) ;define

;; 建一个 context_edit 条目：replacement 可传 'null 表示删除，
;; 其余值作为 replacement.content（对齐上游 ContextEditEntry 的 {content: ...} 形态）
;; 其余值作为 replacement.content（对齐上游 ContextEditEntry 的 {content: ...} 形态）

(define (make-edit id parent-id target-id content)
  (list (cons "type" "context_edit")
    (cons "id" id)
    (cons "parentId" parent-id)
    (cons "targetId" target-id)
    (cons "replacement"
      (if (eq? content 'null) 'null (list (cons "content" content)))
    ) ;cons
  ) ;list
) ;define

;; 建一个 replacement 形态自由填写的 context_edit：用于覆盖不合法形态

(define (make-raw-edit id parent-id target-id replacement)
  (list (cons "type" "context_edit")
    (cons "id" id)
    (cons "parentId" parent-id)
    (cons "targetId" target-id)
    (cons "replacement" replacement)
  ) ;list
) ;define

;; 字段缺失的 context_edit（不带 replacement）：同样表示删除

(define (make-edit-no-replacement id parent-id target-id)
  (list (cons "type" "context_edit")
    (cons "id" id)
    (cons "parentId" parent-id)
    (cons "targetId" target-id)
  ) ;list
) ;define

;; 建一个 compaction 条目

(define (make-compaction id parent-id summary kept-id)
  (list (cons "type" "compaction")
    (cons "id" id)
    (cons "parentId" parent-id)
    (cons "summary" summary)
    (cons "firstKeptEntryId" kept-id)
  ) ;list
) ;define

(define (msg-contents msgs)
  (map pi-user-message-content (filter pi-user-message? msgs))
) ;define

(define (diag-ref diag key fallback)
  (cdr (or (assoc key diag) (cons #f fallback)))
) ;define

;; 1. 带替换内容的编辑：目标消息 content 被替换，role 与条目顺序不变
(let*-values (((entries)
               (list (make-user "u1" 'null "one")
                 (make-user "u2" "u1" "old")
                 (make-user "u3" "u2" "three")
                 (make-edit "e1" "u3" "u2" "new")
               ) ;list
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check (length msgs) => 3)
  (check (msg-contents msgs) => '("one" "new" "three"))
  ;; role 保持不变：三条仍是 user 消息
  (check (map pi-message-role msgs) => '("user" "user" "user"))
  (check (diag-ref diag 'ignored-edits '()) => '())
  (check state => #f)
) ;let*-values

;; 2. replacement 为 'null：目标被删除，其余条目保持原顺序
(let*-values (((entries)
               (list (make-user "u1" 'null "one")
                 (make-user "u2" "u1" "drop-null")
                 (make-edit "e1" "u2" "u2" 'null)
                 (make-user "u3" "e1" "three")
               ) ;list
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check (msg-contents msgs) => '("one" "three"))
  (check (diag-ref diag 'ignored-edits '()) => '())
) ;let*-values

;; 3. replacement 字段缺失：同样删除目标，不报错
(let*-values (((entries)
               (list (make-user "u1" 'null "one")
                 (make-user "u2" "u1" "drop-missing")
                 (make-edit-no-replacement "e1" "u2" "u2")
                 (make-user "u3" "e1" "three")
               ) ;list
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check (msg-contents msgs) => '("one" "three"))
  (check (diag-ref diag 'ignored-edits '()) => '())
  (check state => #f)
) ;let*-values

;; 4. 目标已被压缩裁剪掉：忽略该编辑，目标 id 计入 ignored-edits
(let*-values (((entries)
               (list (make-user "m1" 'null "early")
                 (make-user "m2" "m1" "kept")
                 (make-compaction "c1" "m2" "sum" "m2")
                 (make-user "m3" "c1" "late")
                 ;; m1 已被压缩裁剪，不在裁剪结果内
                 (make-edit "e1" "m3" "m1" "never-applied")
               ) ;list
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  ;; 首条为摘要，其后为保留的 m2 与 m3：编辑既未新增也未替换任何消息
  (check (length msgs) => 3)
  (check (diag-ref diag 'ignored-edits '()) => '("m1"))
  (check (msg-contents (cdr msgs)) => '("kept" "late"))
  (check state => #f)
) ;let*-values

;; 5. 同一目标多次编辑：路径上最后一条胜出
(let*-values (((entries)
               (list (make-user "u1" 'null "one")
                 (make-user "u2" "u1" "original")
                 (make-edit "e1" "u2" "u2" "first-edit")
                 (make-edit "e2" "e1" "u2" "second-edit")
                 (make-user "u3" "e2" "three")
               ) ;list
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check (msg-contents msgs) => '("one" "second-edit" "three"))
  (check (length msgs) => 3)
) ;let*-values

;; 6. assistant 的字符串替换规范化为单个 text 块，role 与 stopReason 保留
(let*-values (((entries)
               (list (make-assistant "a1" 'null "before" "end_turn")
                 (make-edit "e1" "a1" "a1" "after")
               ) ;list
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check (length msgs) => 1)
  (check (pi-message-role (car msgs)) => "assistant")
  (check (pi-assistant-message-stop-reason (car msgs)) => "end_turn")
  (let ((content (pi-assistant-message-content (car msgs))))
    (check (length content) => 1)
    (check (pi-block-type (car content)) => "text")
    (check (cdr (assoc "text" (car content))) => "after")
  ) ;let
) ;let*-values

;; 7. context_edit 属已知类型：不出现在 skipped-unknown-types 中
(let*-values (((entries)
               (list (make-user "u1" 'null "one") (make-edit "e1" "u1" "u1" "revised"))
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check (msg-contents msgs) => '("revised"))
  (check (diag-ref diag 'skipped-unknown-types '()) => '())
) ;let*-values

;; 8. 应用编辑不影响其他条目顺序：删除中间条目后前后顺序保持
(let*-values (((entries)
               (list (make-user "u1" 'null "one")
                 (make-user "u2" "u1" "two")
                 (make-user "u3" "u2" "three")
                 (make-user "u4" "u3" "four")
                 (make-edit "e1" "u4" "u2" 'null)
                 (make-edit "e2" "e1" "u3" "three-revised")
               ) ;list
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check (msg-contents msgs) => '("one" "three-revised" "four"))
  (check (diag-ref diag 'ignored-edits '()) => '())
) ;let*-values

;; 9. 目标存在但不是 message 条目（compaction）：无法应用，计入 ignored-edits；
;;    即使替换值为 null 也不得删除 compaction，摘要照常进入上下文
(let*-values (((entries)
               (list (make-user "m1" 'null "one")
                 (make-compaction "c1" "m1" "sum" "m1")
                 (make-user "m2" "c1" "two")
                 (make-edit "e1" "m2" "c1" "ignored")
                 (make-edit "e2" "e1" "c1" 'null)
               ) ;list
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check (diag-ref diag 'ignored-edits '()) => '("c1"))
  ;; compaction 仍贡献头部摘要，未被删除
  (check-true (string-contains? (pi-user-message-content (car msgs)) "sum"))
  (check (msg-contents (cdr msgs)) => '("one" "two"))
) ;let*-values

;; 10. replacement 形态不合法：裸字符串、对象缺 content、content 非字符串/块数组
;;     都不应用，目标条目保持原样并计入 ignored-edits
(let*-values (((entries)
               (list (make-user "u1" 'null "one")
                 (make-user "u2" "u1" "two")
                 (make-assistant "a1" "u2" "before" "end_turn")
                 ;; 裸字符串：上游 ContextEditEntry 要求 {content: ...}
                 (make-raw-edit "e1" "a1" "u1" "bare")
                 ;; 对象缺 content 键
                 (make-raw-edit "e2" "e1" "u2" (list (cons "text" "no-content")))
                 ;; content 不是块数组：元素没有 type
                 (make-raw-edit "e3" "e2" "a1" (list (cons "content" (list 1 2))))
               ) ;list
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check (msg-contents (filter pi-user-message? msgs)) => '("one" "two"))
  (check (pi-message-role (list-ref msgs 2)) => "assistant")
  (check (diag-ref diag 'ignored-edits '()) => '("u1" "u2" "a1"))
) ;let*-values

;; 11. replacement 形态合法：{content: [块数组]} 原样替换 assistant 的 content
(let*-values (((entries)
               (list (make-assistant "a1" 'null "before" "end_turn")
                 (make-edit "e1"
                   "a1"
                   "a1"
                   (list (list (cons "type" "text") (cons "text" "first"))
                     (list (cons "type" "text") (cons "text" "second"))
                   ) ;list
                 ) ;make-edit
               ) ;list
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check (length msgs) => 1)
  (let ((content (pi-assistant-message-content (car msgs))))
    (check (length content) => 2)
    (check (cdr (assoc "text" (car content))) => "first")
    (check (cdr (assoc "text" (cadr content))) => "second")
  ) ;let
  (check (diag-ref diag 'ignored-edits '()) => '())
) ;let*-values

;; 12. JSONL 真实落盘往返：message + context_edit 行解析后投影
(let* ((tmp-dir (path->string (path-join (path-temp-dir) "test-pi-context-edit-1208")))
       (session-file (path->string (path-join tmp-dir "roundtrip.jsonl")))
      ) ;
  (when (file-exists? tmp-dir)
    (rmtree tmp-dir)
  ) ;when
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (save-session-header! session-file "sess-1208" "2026-10-29T09:00:00.000Z"
    tmp-dir
  ) ;save-session-header!
  (save-session-message! session-file
    "{\"role\":\"user\",\"content\":\"first\"}" "m1" #f
    "2026-10-29T09:00:01.000Z"
  ) ;save-session-message!
  (save-session-message! session-file
    "{\"role\":\"user\",\"content\":\"second\"}" "m2" "m1"
    "2026-10-29T09:00:02.000Z"
  ) ;save-session-message!
  (save-session-message! session-file
    "{\"role\":\"user\",\"content\":\"third\"}" "m3" "m2"
    "2026-10-29T09:00:03.000Z"
  ) ;save-session-message!
  ;; 替换 m1，删除 m2：replacement 落盘为 {content:...}，JSON null 解析为符号 'null
  (append-session-entry! session-file
    "{\"type\":\"context_edit\",\"id\":\"e1\",\"parentId\":\"m3\",\"timestamp\":\"2026-10-29T09:00:04.000Z\",\"targetId\":\"m1\",\"replacement\":{\"content\":\"revised\"}}"
  ) ;append-session-entry!
  (append-session-entry! session-file
    "{\"type\":\"context_edit\",\"id\":\"e2\",\"parentId\":\"e1\",\"timestamp\":\"2026-10-29T09:00:05.000Z\",\"targetId\":\"m2\",\"replacement\":null}"
  ) ;append-session-entry!
  (let*-values (((lines) (path-read-lines session-file))
                ((entries)
                 (filter (lambda (e) e)
                   (map
                     (lambda (line)
                       (catch #t (lambda () (string->json (string-trim line))) (lambda _ #f))
                     ) ;lambda
                     lines
                   ) ;map
                 ) ;filter
                ) ;
                ((msgs state diag) (pi-build-session-projection entries))
               ) ;
    (check (msg-contents msgs) => '("revised" "third"))
    (check (diag-ref diag 'ignored-edits '()) => '())
    (check (diag-ref diag 'skipped-unknown-types '()) => '())
    (check state => #f)
  ) ;let*-values
  (rmtree tmp-dir)
) ;let*

(check-report)
