;; pi-build-session-context-entries
;; 投影的条目层入口：返回当前有效分支的条目、状态与诊断。
;;
;; 语法
;; ----
;; (pi-build-session-context-entries entries)
;; (pi-build-session-context-entries entries leaf-id)
;;
;; 参数
;; ----
;; entries : list，已解析的会话条目（pi-read-session-entries 的结果）
;; leaf-id : string 或 #f，可选叶子条目 id；#f 时取文件中最后一条带 id 的条目
;;
;; 返回值
;; ----
;; (values entries state diagnostics)
;; entries : list，当前有效分支的条目（根到叶顺序，压缩点保留 + 压缩点之后）
;; state : #f（路径上无状态条目）或 alist（thinkingLevel / model 等会话设置）
;; diagnostics : alist，键为 path-reason / trim-reason / skipped-unknown-types /
;;   ignored-edits，值为 #f（无异常）或诊断信息

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii pi code core session-projection)
) ;import

(check-set-mode! 'report-failed)

;; 用户消息条目
(define (make-entry id parent-id)
  (list (cons "type" "message")
    (cons "id" id)
    (cons "parentId" parent-id)
    (cons "message" (list (cons "role" "user") (cons "content" id)))
  ) ;list
) ;define

;; 压缩条目
(define (make-compaction id parent-id summary kept-id)
  (list (cons "type" "compaction")
    (cons "id" id)
    (cons "parentId" parent-id)
    (cons "summary" summary)
    (cons "firstKeptEntryId" kept-id)
  ) ;list
) ;define

(define (entry-ids entries)
  (map (lambda (e) (cdr (assoc "id" e))) entries)
) ;define

;; 1. 无压缩线性会话：条目全部保留，顺序为根到叶
(let*-values (((entries _state _diag)
               (pi-build-session-context-entries
                 (list (make-entry "a" 'null) (make-entry "b" "a") (make-entry "c" "b"))
               ) ;pi-build-session-context-entries
              ) ;
             ) ;
  (check (entry-ids entries) => '("a" "b" "c"))
) ;let*-values

;; 2. 有压缩会话：压缩条目在首位，其后为保留条目与压缩点之后的条目
(let*-values (((entries _state _diag)
               (pi-build-session-context-entries
                 (list (make-entry "m1" 'null)
                   (make-entry "m2" "m1")
                   (make-compaction "c1" "m2" "摘要" "m2")
                   (make-entry "m3" "c1")
                 ) ;list
               ) ;pi-build-session-context-entries
              ) ;
             ) ;
  (check (entry-ids entries) => '("c1" "m2" "m3"))
) ;let*-values

;; 3. 分叉会话：默认叶子取文件最后一条带 id 的条目
(let*-values (((entries-all _s1 _d1)
               (pi-build-session-context-entries
                 (append (list (make-entry "root" 'null))
                   (list (make-entry "b1" "root") (make-entry "b1-leaf" "b1"))
                   (list (make-entry "b2" "root") (make-entry "b2-leaf" "b2"))
                 ) ;append
               ) ;pi-build-session-context-entries
              ) ;
              ((entries-b1 _s2 _d2)
               (pi-build-session-context-entries
                 (append (list (make-entry "root" 'null))
                   (list (make-entry "b1" "root") (make-entry "b1-leaf" "b1"))
                   (list (make-entry "b2" "root") (make-entry "b2-leaf" "b2"))
                 ) ;append
                 "b1-leaf"
               ) ;pi-build-session-context-entries
              ) ;
             ) ;
  (check (entry-ids entries-all) => '("root" "b2" "b2-leaf"))
  (check (entry-ids entries-b1) => '("root" "b1" "b1-leaf"))
) ;let*-values

;; 4. 诊断信息与消息投影入口一致：断链与裁剪降级都记录原因
(let*-values (((_entries _state diag)
               (pi-build-session-context-entries
                 (list (make-entry "a" 'null) (make-entry "b" "ghost"))
               ) ;pi-build-session-context-entries
              ) ;
             ) ;
  (check-true (string-contains? (cdr (or (assoc 'path-reason diag) '(#f . ""))) "ghost"))
) ;let*-values

(check-report)
