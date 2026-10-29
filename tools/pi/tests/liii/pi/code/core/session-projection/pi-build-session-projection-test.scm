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

;; pi-build-session-projection
;; 投影入口：组装 1201-1203 的三级能力，返回消息列表、状态与诊断。
;;
;; 语法
;; ----
;; (pi-build-session-projection entries)
;; (pi-build-session-projection entries leaf-id)
;;
;; 参数
;; ----
;; entries : list，已解析的会话条目（pi-read-session-entries 的结果）
;; leaf-id : string 或 #f，可选叶子条目 id；#f 时取文件中最后一条带 id 的条目
;;
;; 返回值
;; ----
;; (values messages state diagnostics)
;; messages : list，LLM 上下文消息；有压缩时首条为摘要用户消息，其后为保留条目的消息
;; state : #f（路径上无状态条目）或 alist：
;;   thinkingLevel / thinkingLevelEntryId / model / modelEntryId（见 1209）
;; diagnostics : alist，键为 path-reason / trim-reason / skipped-unknown-types，
;;   值为 #f（无异常）或诊断信息（skipped-unknown-types 为条目类型列表）

;; 建一个 user message 条目：parentId 为字符串或 'null

(define (make-entry id parent-id)
  (list (cons "type" "message")
    (cons "id" id)
    (cons "parentId" parent-id)
    (cons "message" (list (cons "role" "user") (cons "content" id)))
  ) ;list
) ;define

;; 建一个 compaction 条目 alist

(define (make-compaction id parent-id summary kept-id)
  (list (cons "type" "compaction")
    (cons "id" id)
    (cons "parentId" parent-id)
    (cons "summary" summary)
    (cons "firstKeptEntryId" kept-id)
  ) ;list
) ;define

(define (entry-type e)
  (cdr (assoc "type" e))
) ;define

(define (msg-contents msgs)
  (map pi-user-message-content (filter pi-user-message? msgs))
) ;define

;; 1. 无压缩线性会话：全部条目映射为消息，无诊断异常
(let*-values (((entries)
               (list (make-entry "a" 'null) (make-entry "b" "a") (make-entry "c" "b"))
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check (map pi-message-role msgs) => '("user" "user" "user"))
  (check (msg-contents msgs) => '("a" "b" "c"))
  (check state => #f)
  (check (cdr (or (assoc 'path-reason diag) '(#f . #f))) => #f)
  (check (cdr (or (assoc 'trim-reason diag) '(#f . #f))) => #f)
) ;let*-values

;; 2. 有压缩会话：摘要用户消息位于首条，其后为保留条目
(let*-values (((entries)
               (list (make-entry "m1" 'null)
                 (make-entry "m2" "m1")
                 (make-compaction "c1" "m2" "summary of early turns" "m2")
                 (make-entry "m3" "c1")
                 (make-entry "m4" "m3")
               ) ;list
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check (map pi-message-role msgs) => '("user" "user" "user" "user"))
  ;; 首条为摘要用户消息：既有前缀 + 摘要 + 后缀
  (check-true (string-prefix? "The conversation history" (pi-user-message-content (car msgs))))
  (check-true (string-contains? (pi-user-message-content (car msgs)) "summary of early turns"))
  (check-true (string-suffix? "</summary>" (pi-user-message-content (car msgs))))
  ;; 其后为保留条目（m2 起保留 + 压缩点之后）
  (check (msg-contents (cdr msgs)) => '("m2" "m3" "m4"))
  (check state => #f)
  (check (cdr (or (assoc 'trim-reason diag) '(#f . #f))) => #f)
) ;let*-values

;; 3. 分叉会话：只投影叶子所在分支
(let*-values (((entries)
               (append (list (make-entry "root" 'null))
                 (list (make-entry "b1" "root") (make-entry "b1-leaf" "b1"))
                 (list (make-entry "b2" "root") (make-entry "b2-leaf" "b2"))
               ) ;append
              ) ;
              ((msgs-all state-all diag-all) (pi-build-session-projection entries))
              ((msgs-b1 state-b1 diag-b1) (pi-build-session-projection entries "b1-leaf"))
             ) ;
  ;; 缺省叶子取文件最后一条带 id 条目（b2-leaf）
  (check (msg-contents msgs-all) => '("root" "b2" "b2-leaf"))
  ;; 显式指定 b1 分支叶子：另一分支不出现
  (check (msg-contents msgs-b1) => '("root" "b1" "b1-leaf"))
) ;let*-values

;; 4. 未知 type 计入诊断：不进入消息，也不再无声跳过
(let*-values (((entries)
               (list (make-entry "a" 'null)
                 (list (cons "type" "mystery") (cons "id" "x") (cons "parentId" "a"))
                 (make-entry "b" "x")
               ) ;list
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  (check (msg-contents msgs) => '("a" "b"))
  (check (cdr (or (assoc 'skipped-unknown-types diag) '(#f . '()))) => '("mystery"))
  (check state => #f)
) ;let*-values

;; 5. 路径断链：parentId 指向不存在条目，原因计入诊断
(let*-values (((entries)
               (list (make-entry "a" 'null) (make-entry "b" "ghost"))
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  ;; 断链时返回已收集路径（b 及其可达前缀）
  (check (msg-contents msgs) => '("b"))
  (check-true
    (string-contains?
      (cdr (or (assoc 'path-reason diag) '(#f . "")))
      "ghost"
    ) ;string-contains?
  ) ;check-true
) ;let*-values

;; 6. 裁剪降级：firstKeptEntryId 指向路径外，原因计入诊断
(let*-values (((entries)
               (list (make-entry "m1" 'null)
                 (make-entry "m2" "m1")
                 (make-compaction "c1" "m2" "s" "ghost")
                 (make-entry "m3" "c1")
               ) ;list
              ) ;
              ((msgs state diag) (pi-build-session-projection entries))
             ) ;
  ;; 降级为压缩点之后：首条摘要 + m3
  (check (length msgs) => 2)
  (check (msg-contents (cdr msgs)) => '("m3"))
  (check-true
    (string-contains?
      (cdr (or (assoc 'trim-reason diag) '(#f . "")))
      "ghost"
    ) ;string-contains?
  ) ;check-true
) ;let*-values

;; 7. JSONL 真实落盘往返：真实写入与手写 compaction 行解析后投影
(let* ((tmp-dir (path->string (path-join (path-temp-dir) "test-pi-build-projection-1204")))
       (session-file (path->string (path-join tmp-dir "roundtrip.jsonl")))
      ) ;
  (when (file-exists? tmp-dir)
    (rmtree tmp-dir)
  ) ;when
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (save-session-header! session-file "sess-1204" "2026-10-29T08:00:00.000Z"
    tmp-dir
  ) ;save-session-header!
  (save-session-message! session-file
    "{\"role\":\"user\",\"content\":\"first\"}" "m1" #f
    "2026-10-29T08:00:01.000Z"
  ) ;save-session-message!
  (save-session-message! session-file
    "{\"role\":\"user\",\"content\":\"second\"}" "m2" "m1"
    "2026-10-29T08:00:02.000Z"
  ) ;save-session-message!
  (append-session-entry! session-file
    "{\"type\":\"compaction\",\"id\":\"c1\",\"parentId\":\"m2\",\"summary\":\"real summary\",\"firstKeptEntryId\":\"m2\",\"tokensBefore\":100}"
  ) ;append-session-entry!
  (save-session-message! session-file
    "{\"role\":\"user\",\"content\":\"third\"}" "m3" "c1"
    "2026-10-29T08:00:04.000Z"
  ) ;save-session-message!
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
    ;; session 头与 compaction 都不直接映射消息：首条摘要 + 保留的 m2、m3
    (check (length msgs) => 3)
    (check-true (string-contains? (pi-user-message-content (car msgs)) "real summary"))
    (check (msg-contents (cdr msgs)) => '("second" "third"))
    (check state => #f)
    (check (cdr (or (assoc 'path-reason diag) '(#f . #f))) => #f)
    (check (cdr (or (assoc 'trim-reason diag) '(#f . #f))) => #f)
  ) ;let*-values
  (rmtree tmp-dir)
) ;let*

(check-report)
