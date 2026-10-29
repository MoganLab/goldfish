(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii shutil)
  (liii string)
  (liii list)
  (liii json)
  (liii pi code core session-manager)
  (liii pi code core session-projection)
) ;import

(check-set-mode! 'report-failed)

;; pi-context-entries
;; 压缩感知的路径裁剪：在路径条目上找最后一条 compaction，
;; 返回该压缩条目与 firstKeptEntryId 起的保留条目。
;;
;; 语法
;; ----
;; (pi-context-entries path)
;;
;; 参数
;; ----
;; path : list
;; 根到叶顺序的路径条目列表（pi-session-path 的第一个返回值）。
;;
;; 返回值
;; ----
;; (values context-entries reason)
;; context-entries : list，无压缩时为路径全部条目；
;;   有压缩时为压缩条目 + firstKeptEntryId 到压缩点之前的条目 + 压缩点之后的全部路径条目；
;; reason : #f 或降级原因字符串（firstKeptEntryId 不在压缩点之前的路径上、缺失或非法）。
;;
;; 示例
;; ----
;; (pi-context-entries (list m1 m2 C m3))
;; => 条目 (C m3) 与降级原因（C 的 firstKeptEntryId 指向路径外时）

;; 建一个 message 条目 alist：parentId 为字符串或 'null

(define (make-entry id parent-id)
  (list (cons "type" "message")
    (cons "id" id)
    (cons "parentId" parent-id)
    (cons "message" (list (cons "role" "user") (cons "content" id)))
  ) ;list
) ;define

;; 建一个 compaction 条目 alist；kept-id 为字符串或 'null（缺失场景传 #f 时省略字段）

(define (make-compaction id parent-id summary kept-id)
  (if kept-id
    (list (cons "type" "compaction")
      (cons "id" id)
      (cons "parentId" parent-id)
      (cons "summary" summary)
      (cons "firstKeptEntryId" kept-id)
    ) ;list
    (list (cons "type" "compaction")
      (cons "id" id)
      (cons "parentId" parent-id)
      (cons "summary" summary)
    ) ;list
  ) ;if
) ;define

(define (entry-id e)
  (cdr (assoc "id" e))
) ;define

;; 路径条目列表的类型列表，便于断言裁剪结果的形状

(define (entry-types entries)
  (map (lambda (e) (cdr (assoc "type" e))) entries)
) ;define

;; 1. 无压缩：线性路径原样返回，无降级原因
(let*-values (((path) (list (make-entry "a" 'null) (make-entry "b" "a") (make-entry "c" "b")))
              ((result reason) (pi-context-entries path))
             ) ;
  (check (map entry-id result) => '("a" "b" "c"))
  (check (equal? result path) => #t)
  (check reason => #f)
) ;let*-values

;; 2. 单次压缩：压缩条目居首，其后为 firstKeptEntryId 起的条目与压缩点之后的全部条目
(let*-values (((path)
               (list (make-entry "m1" 'null)
                 (make-entry "m2" "m1")
                 (make-entry "m3" "m2")
                 (make-entry "m4" "m3")
                 (make-compaction "c1" "m4" "summary of early turns" "m3")
                 (make-entry "m5" "c1")
                 (make-entry "m6" "m5")
               ) ;list
              ) ;
              ((result reason) (pi-context-entries path))
             ) ;
  (check (map entry-id result) => '("c1" "m3" "m4" "m5" "m6"))
  (check (entry-types result)
    =>
    '("compaction" "message" "message" "message" "message")
  ) ;check
  ;; 摘要作为文本随压缩条目返回，不构造消息
  (check (cdr (assoc "summary" (car result))) => "summary of early turns")
  (check (cdr (assoc "firstKeptEntryId" (car result))) => "m3")
  (check reason => #f)
) ;let*-values

;; 3. 多次压缩：只看最后一条，其摘要已包含此前摘要
(let*-values (((path)
               (list (make-entry "m1" 'null)
                 (make-entry "m2" "m1")
                 (make-compaction "c1" "m2" "first summary" "m1")
                 (make-entry "m3" "c1")
                 (make-entry "m4" "m3")
                 (make-compaction "c2" "m4" "second summary" "m3")
                 (make-entry "m5" "c2")
               ) ;list
              ) ;
              ((result reason) (pi-context-entries path))
             ) ;
  ;; 只看 c2：从 m3 起保留，c1 及更早条目不出现
  (check (map entry-id result) => '("c2" "m3" "m4" "m5"))
  (check (cdr (assoc "summary" (car result))) => "second summary")
  (check reason => #f)
) ;let*-values

;; 4. 压缩点之后新增消息仍被保留（含非 message 条目也按路径保留）
(let*-values (((path)
               (list (make-entry "m1" 'null)
                 (make-compaction "c1" "m1" "s" "m1")
                 (make-entry "m2" "c1")
                 (make-entry "m3" "m2")
               ) ;list
              ) ;
              ((result reason) (pi-context-entries path))
             ) ;
  (check (map entry-id result) => '("c1" "m1" "m2" "m3"))
  (check reason => #f)
) ;let*-values

;; 5. firstKeptEntryId 指向路径外：降级为压缩点之后，原因作为第二返回值
(let*-values (((path)
               (list (make-entry "m1" 'null)
                 (make-entry "m2" "m1")
                 (make-compaction "c1" "m2" "s" "ghost")
                 (make-entry "m3" "c1")
               ) ;list
              ) ;
              ((result reason) (pi-context-entries path))
             ) ;
  (check (map entry-id result) => '("c1" "m3"))
  (check-true (and (string? reason) (string-contains? reason "ghost")))
) ;let*-values

;; 6. firstKeptEntryId 指向压缩点之后的条目（异常数据）：同样降级并给出原因
(let*-values (((path)
               (list (make-entry "m1" 'null)
                 (make-entry "m2" "m1")
                 (make-compaction "c1" "m2" "s" "m3")
                 (make-entry "m3" "c1")
               ) ;list
              ) ;
              ((result reason) (pi-context-entries path))
             ) ;
  (check (map entry-id result) => '("c1" "m3"))
  (check-true (and (string? reason) (not (string-null? reason))))
) ;let*-values

;; 7. firstKeptEntryId 字段缺失：不抛异常，降级为压缩点之后并给出原因
(let*-values (((path)
               (list (make-entry "m1" 'null)
                 (make-entry "m2" "m1")
                 (make-compaction "c1" "m2" "s" #f)
                 (make-entry "m3" "c1")
               ) ;list
              ) ;
              ((result reason) (pi-context-entries path))
             ) ;
  (check (map entry-id result) => '("c1" "m3"))
  (check-true (and (string? reason) (not (string-null? reason))))
) ;let*-values

;; 8. firstKeptEntryId 为 JSON null（解析为符号 'null）：与缺失同样处理
(let*-values (((compaction)
               (list (cons "type" "compaction")
                 (cons "id" "c1")
                 (cons "parentId" "m2")
                 (cons "summary" "s")
                 (cons "firstKeptEntryId" 'null)
               ) ;list
              ) ;
              ((path) (list (make-entry "m1" 'null) (make-entry "m2" "m1") compaction))
              ((result reason) (pi-context-entries path))
             ) ;
  (check (map entry-id result) => '("c1"))
  (check-true (and (string? reason) (not (string-null? reason))))
) ;let*-values

;; 9. firstKeptEntryId 为非字符串类型（数字）：降级并给出原因，不抛异常
(let*-values (((compaction)
               (list (cons "type" "compaction")
                 (cons "id" "c1")
                 (cons "parentId" "m2")
                 (cons "summary" "s")
                 (cons "firstKeptEntryId" 42)
               ) ;list
              ) ;
              ((path) (list (make-entry "m1" 'null) (make-entry "m2" "m1") compaction))
              ((result reason) (pi-context-entries path))
             ) ;
  (check (map entry-id result) => '("c1"))
  (check-true (and (string? reason) (not (string-null? reason))))
) ;let*-values

;; 10. 保留区间内含更早的压缩条目：按位置保留（旧摘要语义由投影入口决定）
(let*-values (((path)
               (list (make-entry "m1" 'null)
                 (make-compaction "c1" "m1" "first summary" "m1")
                 (make-entry "m2" "c1")
                 (make-compaction "c2" "m2" "second summary" "m1")
                 (make-entry "m3" "c2")
               ) ;list
              ) ;
              ((result reason) (pi-context-entries path))
             ) ;
  ;; c2 的 firstKeptEntryId 为 m1：c1 在保留区间内，按路径顺序保留
  (check (map entry-id result) => '("c2" "m1" "c1" "m2" "m3"))
  (check reason => #f)
) ;let*-values

;; 11. 与 pi-session-path 组合：fork 会话先回溯路径再裁剪，另一分支不混入
(let*-values (((fork-entries)
               (append (list (make-entry "root" 'null))
                 (list (make-entry "b1" "root") (make-entry "b1-leaf" "b1"))
                 (list (make-entry "b2" "root") (make-entry "b2-leaf" "b2"))
               ) ;append
              ) ;
              ((path path-reason) (pi-session-path fork-entries "b1-leaf"))
              ((result reason) (pi-context-entries path))
             ) ;
  (check (map entry-id result) => '("root" "b1" "b1-leaf"))
  (check reason => #f)
  (check path-reason => #f)
) ;let*-values

;; 12. fork 且压缩：裁剪只作用于叶子所在分支的路径
(let*-values (((fork-entries)
               (append (list (make-entry "root" 'null))
                 (list (make-entry "b1" "root")
                   (make-compaction "c1" "b1" "b1 summary" "b1")
                   (make-entry "b1-leaf" "c1")
                 ) ;list
                 (list (make-entry "b2" "root") (make-entry "b2-leaf" "b2"))
               ) ;append
              ) ;
              ((path _) (pi-session-path fork-entries "b1-leaf"))
              ((result reason) (pi-context-entries path))
             ) ;
  (check (map entry-id result) => '("c1" "b1" "b1-leaf"))
  (check (cdr (assoc "summary" (car result))) => "b1 summary")
  (check reason => #f)
) ;let*-values

;; 13. 空路径：空结果，无原因
(let*-values (((result reason) (pi-context-entries '())))
  (check result => '())
  (check reason => #f)
) ;let*-values

;; 14. JSONL 真实落盘往返：真实写入的消息行与手写 compaction 行解析后裁剪
(let* ((tmp-dir (path->string (path-join (path-temp-dir) "test-pi-context-entries-1203"))
       ) ;tmp-dir
       (session-file (path->string (path-join tmp-dir "roundtrip.jsonl")))
      ) ;
  (when (file-exists? tmp-dir)
    (rmtree tmp-dir)
  ) ;when
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (save-session-header! session-file "sess-1203" "2026-10-29T08:00:00.000Z"
    tmp-dir
  ) ;save-session-header!
  (save-session-message! session-file
    "{\"role\":\"user\",\"content\":\"first\"}" "m1" #f
    "2026-10-29T08:00:01.000Z"
  ) ;save-session-message!
  (save-session-message! session-file
    "{\"role\":\"assistant\",\"content\":\"second\"}" "m2" "m1"
    "2026-10-29T08:00:02.000Z"
  ) ;save-session-message!
  (save-session-message! session-file
    "{\"role\":\"user\",\"content\":\"third\"}" "m3" "m2"
    "2026-10-29T08:00:03.000Z"
  ) ;save-session-message!
  (append-session-entry! session-file
    "{\"type\":\"compaction\",\"id\":\"c1\",\"parentId\":\"m3\",\"summary\":\"real summary\",\"firstKeptEntryId\":\"m3\",\"tokensBefore\":100}"
  ) ;append-session-entry!
  (save-session-message! session-file
    "{\"role\":\"user\",\"content\":\"fourth\"}" "m4" "c1"
    "2026-10-29T08:00:05.000Z"
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
                ((path _) (pi-session-path entries))
                ((result reason) (pi-context-entries path))
               ) ;
    (check (map entry-id result) => '("c1" "m3" "m4"))
    (check (cdr (assoc "summary" (car result))) => "real summary")
    (check reason => #f)
  ) ;let*-values
  (rmtree tmp-dir)
) ;let*

(check-report)
