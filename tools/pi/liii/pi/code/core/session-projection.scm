;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 会话路径回溯：从叶子条目沿 parentId 回溯到根，得到根到叶顺序的路径条目。
;; 这是 fork 分支切换与后续投影（1203/1204）的基础。
;; 投影在 1204 的基础上补齐 context_edit（上游 /undo 的存储形式）：
;; 被撤销的目标条目从上下文中删除，被替换的目标只替换 content 而保留 role 与元数据。
;; 编辑的收集与应用在子库 (liii pi code core session-projection context-edit)，
;; 本库只负责在裁剪结果与消息构造之间调用它。
;; 1209 起从完整路径导出会话设置（thinkingLevel 与 model），对应上游
;; buildSessionContext 的第二步；扫描对象是 pi-session-path 的完整路径，
;; 不受 pi-context-entries 裁剪影响（压缩只折叠对话内容，不重置会话设置）。
;; 状态提取在子库 (liii pi code core session-projection state)。
(define-library (liii pi code core session-projection)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii json)
    (liii pi ai message)
    (liii pi code core session-manager)
    (liii pi code core session-projection context-edit)
    (liii pi code core session-projection state)
  ) ;import
  (export pi-session-path pi-context-entries pi-build-session-projection)
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

    ;; 头部摘要用户消息的文本（对齐上游 createCompactionSummaryMessage）：
    ;; 既有前缀 + 摘要 + 后缀，与压缩层 pi-compaction-summary-message 同构
    (define %projection-summary-prefix
      "The conversation history before this point was compacted into the following summary:\n\n<summary>\n"
    ) ;define

    (define %projection-summary-suffix "\n</summary>")

    ;; 条目类型是否在投影入口的已知集合内：
    ;; message 参与上下文；compaction 贡献头部摘要；session 为文件头；
    ;; context_edit 是编辑指令，自身不产生消息（见子库 pi-apply-context-edits）；
    ;; model_change / thinking_level_change 只提供会话设置，同样不产生消息
    (define (%known-entry-type? entry)
      (let ((t (%ref entry "type")))
        (and (string? t)
          (or (equal? t "message")
            (equal? t "compaction")
            (equal? t "session")
            (equal? t "context_edit")
            (equal? t "model_change")
            (equal? t "thinking_level_change")
          ) ;or
        ) ;and
      ) ;let
    ) ;define

    ;; 首个 compaction 条目的 summary；无则 #f
    (define (%first-compaction-summary context-entries)
      (let loop
        ((rest context-entries))
        (cond ((null? rest) #f)
              ((equal? (%ref (car rest) "type") "compaction") (%ref (car rest) "summary"))
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    ;; 摘要文本的字符串化：非字符串降级描述，不抛异常
    (define (%summary->string summary)
      (cond ((string? summary) summary)
            ((not summary) "(no summary)")
            ((eq? summary 'null) "(no summary)")
            ((symbol? summary) (symbol->string summary))
            ((number? summary) (number->string summary))
            (else "(no summary)")
      ) ;cond
    ) ;define

    ;; 未知类型条目计入诊断，不再无声跳过
    (define (%unknown-types context-entries)
      (let loop
        ((rest context-entries) (acc '()))
        (if (null? rest)
          (reverse acc)
          (let ((t (%ref (car rest) "type")))
            (loop (cdr rest) (if (%known-entry-type? (car rest)) acc (cons t acc)))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; pi-build-session-projection：投影入口（对齐上游 buildSessionProjection）
    ;; 纯组合 1201-1204 的能力并补齐 context_edit 与状态导出：
    ;; 1) pi-session-path：从叶子沿 parentId 回溯路径（1202）
    ;; 2) 状态提取：从完整路径取最后的 thinkingLevel 与 model（1209）
    ;; 3) pi-context-entries：压缩感知的路径裁剪（1203）
    ;; 4) context_edit 应用：在裁剪结果上删除 / 替换目标（1208），先于消息构造
    ;; 5) pi-entry-to-context-messages：逐条目到消息的映射（1201）
    ;; 返回三部分（values messages state diagnostics）：
    ;; - messages：LLM 上下文消息列表；裁剪返回摘要时，头部插入一条用户消息，
    ;;   文本为既有摘要前缀 + 摘要 + 后缀；其后为保留条目的消息（含压缩点之后）
    ;; - state：#f（路径上无状态条目）或 alist：
    ;;   (thinkingLevel . 字符串|#f)
    ;;   (thinkingLevelEntryId . 条目 id|#f)
    ;;   (model . ((provider . 字符串) (modelId . 字符串))|#f)
    ;;   (modelEntryId . 条目 id|#f)
    ;;   状态扫描作用于完整路径，压缩不会裁掉压缩点之前的设置
    ;; - diagnostics：alist，汇总路径断链（path-reason）与裁剪降级（trim-reason）
    ;;   两类原因，未知 type 的条目计入 skipped-unknown-types（context_edit、
    ;;   model_change、thinking_level_change 均属已知类型），
    ;;   无法应用的编辑目标 id 计入 ignored-edits（无则 '()）
    ;; 纯函数：不读文件、不写日志、不改写会话文件，所有 I/O 由调用方完成
    (define (pi-build-session-projection entries . opt-leaf-id)
      (let* ((leaf-id (and (pair? opt-leaf-id) (car opt-leaf-id)))
             (call-path (lambda ()
                          (if leaf-id (pi-session-path entries leaf-id) (pi-session-path entries))
                        ) ;lambda
             ) ;call-path
            ) ;
        (let*-values (((path path-reason) (call-path)))
          (let*-values (((context-entries trim-reason) (pi-context-entries path)))
            (let*-values (((edited-entries ignored-edits) (pi-apply-context-edits context-entries)))
              (let* ((msgs (apply append (map pi-entry-to-context-messages edited-entries)))
                     ;; 未知类型在编辑前的条目上统计：应用编辑后会移除 context_edit 条目
                     (summary (%first-compaction-summary edited-entries))
                     (unknown-types (%unknown-types context-entries))
                     ;; 状态必须基于完整 path 而非裁剪后的 context-entries
                     (state (pi-collect-projection-state path))
                     (msgs
                       (if summary
                         (cons (pi-user-message (string-append %projection-summary-prefix
                                                  (%summary->string summary)
                                                  %projection-summary-suffix
                                                ) ;string-append
                               ) ;pi-user-message
                           msgs
                         ) ;cons
                         msgs
                       ) ;if
                     ) ;msgs
                     (diagnostics (list (cons 'path-reason path-reason)
                                    (cons 'trim-reason trim-reason)
                                    (cons 'skipped-unknown-types unknown-types)
                                    (cons 'ignored-edits ignored-edits)
                                  ) ;list
                     ) ;diagnostics
                    ) ;
                (values msgs state diagnostics)
              ) ;let*
            ) ;let*-values
          ) ;let*-values
        ) ;let*-values
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
