;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 会话路径回溯：从叶子条目沿 parentId 回溯到根，得到根到叶顺序的路径条目。
;; 这是 fork 分支切换与后续投影（1203/1204）的基础。
(define-library (liii pi code core session-projection)
  (import (scheme base) (liii string) (liii list) (liii json))
  (export pi-session-path pi-context-entries)
  (begin

    ;; 字段访问兼容字符串键与符号键；JSON null 解析为符号 'null，与 #f 区分
    (define (%ref obj key)
      (let* ((str-key (if (symbol? key) (symbol->string key) key))
             (sym-key (if (string? key) (string->symbol key) key))
             (pair (or (assoc str-key obj) (assoc sym-key obj)))
            ) ;
        (if pair (cdr pair) #f)
      ) ;let*
    ) ;define

    ;; parentId 有效终止：缺失、JSON null 或空字符串都视为根
    (define (%root-parent? parent-id)
      (or (not parent-id)
        (json-null? parent-id)
        (and (string? parent-id) (string-null? parent-id))
      ) ;or
    ) ;define

    (define (%entry-id entry)
      (let ((id (%ref entry "id")))
        (and (string? id) (not (string-null? id)) id)
      ) ;let
    ) ;define

    ;; 单遍建立 id -> 条目映射；重复 id 以后写条目为准（同文件追加语义）
    (define (%build-index entries)
      (let loop
        ((rest entries) (index '()))
        (if (null? rest)
          (reverse index)
          (let* ((entry (car rest))
                 (id (%entry-id entry))
                 (index (if id (%assoc-set index id entry) index))
                ) ;
            (loop (cdr rest) index)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 前插构建索引，返回保持文件顺序的索引（重复 id 时先查到的是最新条目）
    (define (%assoc-set alist key val)
      (cons (cons key val) alist)
    ) ;define

    ;; 文件中最后一条带 id 的条目（追加式写入的实际叶子布局）
    (define (%last-id-entry entries)
      (let loop
        ((rest (reverse entries)))
        (cond ((null? rest) #f)
              ((%entry-id (car rest)) (car rest))
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%index-lookup index id)
      (let ((pair (assoc id index)))
        (and pair (cdr pair))
      ) ;let
    ) ;define

    ;; 从叶子沿 parentId 回溯；visited 集合防环；
    ;; path 以叶到根顺序收集，终止时在头部接上当前（最顶层）条目即得根到叶顺序；
    ;; 断链与成环均返回已收集路径与中断原因
    (define (%walk-leaf-to-root leaf index)
      (let loop
        ((current leaf) (path '()) (visited '()))
        (let* ((id (%entry-id current))
               (visited (if id (cons id visited) visited))
               (parent-id (%ref current "parentId"))
               (found
                 (and (not (%root-parent? parent-id))
                   (cond ((not (string? parent-id)) (list 'type 'nonstring))
                         ((assoc parent-id index) => cdr)
                         (else #f)
                   ) ;cond
                 ) ;and
               ) ;found
              ) ;
          (cond ((%root-parent? parent-id) (values (cons current path) #f))
                ((not (string? parent-id))
                 (values (cons current path)
                   (string-append "invalid parentId: " (%parent->string parent-id))
                 ) ;values
                ) ;
                ((not found)
                 (values (cons current path) (string-append "parentId not found: " parent-id))
                ) ;
                ((member parent-id visited)
                 (values (cons current path) (string-append "parentId cycle at: " parent-id))
                ) ;
                (else (loop found (cons current path) visited))
          ) ;cond
        ) ;let*
      ) ;let
    ) ;define

    ;; parentId 的诊断字符串：非字符串时降级描述，不抛异常
    (define (%parent->string v)
      (cond ((string? v) v)
            ((symbol? v) (symbol->string v))
            ((number? v) (number->string v))
            (else "unknown")
      ) ;cond
    ) ;define

    (define (pi-session-path entries . opt-leaf-id)
      (let* ((index (%build-index entries))
             (leaf-id (and (pair? opt-leaf-id) (car opt-leaf-id)))
            ) ;
        (cond
          ;; 显式指定叶子但索引中不存在：空路径，由调用方决定退化策略
          ((and leaf-id (not (assoc leaf-id index))) (values '() "leaf id not found"))
          (else
            (let ((leaf (if leaf-id (%index-lookup index leaf-id) (%last-id-entry entries))))
              (if (not leaf) (values '() #f) (%walk-leaf-to-root leaf index))
            ) ;let
          ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    ;; firstKeptEntryId 的诊断字符串：非字符串时降级描述，不抛异常
    (define (%kept-id->string v)
      (cond ((string? v) v)
            ((symbol? v) (symbol->string v))
            ((number? v) (number->string v))
            (else "unknown")
      ) ;cond
    ) ;define

    ;; 路径上最后一条 compaction 的索引；无压缩时返回 #f
    (define (%last-compaction-index path)
      (let ((rev-idx
              (list-index (lambda (e) (equal? (%ref e "type") "compaction")) (reverse path))
            ) ;rev-idx
           ) ;
        (and rev-idx (- (length path) 1 rev-idx))
      ) ;let
    ) ;define

    ;; 在压缩点之前的路径上找 firstKeptEntryId 的索引；找不到返回 #f
    (define (%kept-start-index path cidx kept-id)
      (and (string? kept-id)
        (list-index
          (lambda (e)
            (and (equal? (%ref e "id") kept-id)
              ;; 指向压缩点之后的同 id 条目属异常数据，不算命中
              (not (equal? (%ref e "type") "compaction"))
            ) ;and
          ) ;lambda
          (take path cidx)
        ) ;list-index
      ) ;and
    ) ;define

    ;; pi-context-entries：压缩感知的路径裁剪（对齐上游 buildContextEntries）
    ;; 输入为根到叶顺序的路径条目（pi-session-path 的第一个返回值）：
    ;; - 无压缩：返回路径全部条目与空摘要，不裁剪
    ;; - 有压缩：只看最后一条 compaction（其摘要已包含此前的摘要），
    ;;   返回压缩条目 + firstKeptEntryId 到压缩点之前的保留条目 + 压缩点之后的全部路径条目
    ;; - 摘要不构造成消息，只随压缩条目作为文本返回；头部用户消息由投影入口统一插入
    ;; - firstKeptEntryId 缺失、为 JSON null、非字符串或不在压缩点之前的路径上时，
    ;;   显式降级为压缩点之后的条目，并把原因作为第二返回值（不再静默回退）
    (define (pi-context-entries path)
      (let ((cidx (%last-compaction-index path)))
        (if (not cidx)
          (values path #f)
          (let* ((compaction (list-ref path cidx))
                 (kept-id (%ref compaction "firstKeptEntryId"))
                 (kept-idx (%kept-start-index path cidx kept-id))
                 (kept-part
                   (cond (kept-idx (drop (take path cidx) kept-idx))
                         ;; 降级：只保留压缩点之后的条目，近期消息不丢
                         (else '())
                   ) ;cond
                 ) ;kept-part
                 (after-part (drop path (+ cidx 1)))
                 (reason (and (not kept-idx)
                           (string-append "firstKeptEntryId not on path: "
                             (%kept-id->string kept-id)
                             "; kept entries after compaction only"
                           ) ;string-append
                         ) ;and
                 ) ;reason
                ) ;
            (values (cons compaction (append kept-part after-part)) reason)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
