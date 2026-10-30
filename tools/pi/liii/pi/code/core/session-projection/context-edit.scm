;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;;; context_edit 应用：在压缩裁剪后的条目上删除或替换目标消息。
;; 从投影入口拆出，避免投影主库超过单文件行数限制；纯函数，不读文件、不写日志。
(define-library (liii pi code core session-projection context-edit)
  (import (scheme base) (liii string) (liii json))
  (export pi-apply-context-edits)
  (begin
    ;; 字段访问兼容字符串键与符号键；与投影主库的 %ref 同构（子库不共享内部定义）
    (define (%ref obj key)
      (let* ((str-key (if (symbol? key) (symbol->string key) key))
             (sym-key (if (string? key) (string->symbol key) key))
             (pair (or (assoc str-key obj) (assoc sym-key obj)))
            ) ;
        (if pair (cdr pair) #f)
      ) ;let*
    ) ;define

    ;; 条目 id：仅接受非空字符串，其余按缺失处理
    (define (%entry-id entry)
      (let ((id (%ref entry "id")))
        (and (string? id) (not (string-null? id)) id)
      ) ;let
    ) ;define

    ;; context_edit 条目（上游 /undo 的存储形式）承载一次撤销或替换：
    ;; {type:"context_edit", targetId, replacement}
    ;; - replacement 缺失或为 JSON null（(liii json) 解析为符号 'null）表示删除：
    ;;   该目标条目整体不进入上下文
    ;; - replacement 非 null 表示只替换目标消息的 content，role 与其他元数据保留
    ;; - 多个编辑指向同一目标时，路径上最后一条胜出（后出现的覆盖先出现的）
    ;; - 编辑作用在裁剪之后的条目上，且先于消息构造，三套消息构造无需各自处理替换
    ;; - context_edit 自身不产生消息，但属于已知类型，不计入 skipped-unknown-types

    ;; 删除语义：字段缺失（%ref 返回 #f）与 JSON null 均是删除；
    ;; JSON false 因与缺失同形，同样按删除处理
    (define (%delete-edit? replacement)
      (or (not replacement) (json-null? replacement))
    ) ;define

    ;; 可用的替换值：字符串或数组（vector / list）；
    ;; 其余类型无法规范化，视为不可应用（保留原条目并计入诊断）
    (define (%applicable-replacement? replacement)
      (or (string? replacement) (vector? replacement) (list? replacement))
    ) ;define

    ;; 收集裁剪结果中的 context_edit，返回 (targetId . replacement) 的前插 alist：
    ;; 后出现的编辑位于表头，因此 assoc 命中的就是"路径上最后一条"（后者胜出）；
    ;; 缺少有效 targetId 的编辑无从应用，直接跳过
    (define (%collect-context-edits context-entries)
      (let loop
        ((rest context-entries) (edits '()))
        (cond ((null? rest) edits)
              ((equal? (%ref (car rest) "type") "context_edit")
               (let ((target-id (%ref (car rest) "targetId")))
                 (loop (cdr rest)
                   (if (and (string? target-id) (not (string-null? target-id)))
                     (cons (cons target-id (%ref (car rest) "replacement")) edits)
                     edits
                   ) ;if
                 ) ;loop
               ) ;let
              ) ;
              (else (loop (cdr rest) edits))
        ) ;cond
      ) ;let
    ) ;define

    ;; 按 id 在裁剪结果中定位条目；找不到返回 #f
    (define (%entry-by-id context-entries id)
      (let loop
        ((rest context-entries))
        (cond ((null? rest) #f)
              ((equal? (%entry-id (car rest)) id) (car rest))
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    ;; alist 的键值替换：兼容字符串键与符号键，保留原键形态；
    ;; 键不存在时以字符串键前插，返回新 alist（不改写原结构）
    (define (%alist-set alist key val)
      (let* ((str-key (if (symbol? key) (symbol->string key) key))
             (sym-key (if (string? key) (string->symbol key) key))
             (pair (or (assoc str-key alist) (assoc sym-key alist)))
            ) ;
        (if pair
          (map
            (lambda (p) (if (eq? p pair) (cons (car p) val) p))
            alist
          ) ;map
          (cons (cons str-key val) alist)
        ) ;if
      ) ;let*
    ) ;define

    ;; 替换内容规范化：user 可直接用字符串；assistant / toolResult 的 content 要求块数组，
    ;; 字符串规范化为单个 text 块；数组（vector 或 list）统一转为 list 后原样使用
    (define (%replacement->content role replacement)
      (let ((content (if (vector? replacement) (vector->list replacement) replacement)))
        (if (and (string? content) (not (equal? role "user")))
          (list (list (cons "type" "text") (cons "text" content)))
          content
        ) ;if
      ) ;let
    ) ;define

    ;; 只替换 message 的 content，其余字段（role / stopReason / usage / timestamp）保持不变；
    ;; 返回新条目，不就地修改
    (define (%replace-entry-content entry replacement)
      (let* ((msg (%ref entry "message"))
             (role (and (pair? msg) (%ref msg "role")))
             (content (%replacement->content role replacement))
             (msg (if (pair? msg)
                    (%alist-set msg "content" content)
                    (list (cons "content" content))
                  ) ;if
             ) ;msg
            ) ;
        (%alist-set entry "message" msg)
      ) ;let*
    ) ;define

    ;; 在裁剪结果上应用编辑：删除的目标条目移出上下文，替换的目标只换 content；
    ;; 条目顺序由原裁剪结果决定，编辑自身不产生条目；
    ;; 无法应用的编辑（目标不在裁剪结果内、目标不是 message、replacement 类型不合法）
    ;; 保持条目原样，由 %ignored-edit-ids 统一计入诊断
    (define (%apply-context-edits context-entries edits)
      (let loop
        ((rest context-entries) (acc '()))
        (if (null? rest)
          (reverse acc)
          (let* ((entry (car rest))
                 (type (%ref entry "type"))
                 (edit
                   (let ((id (%entry-id entry)))
                     (and id (assoc id edits))
                   ) ;let
                 ) ;edit
                 (replacement (and edit (cdr edit)))
                ) ;
            (cond
              ;; 编辑条目自身不进入上下文
              ((equal? type "context_edit") (loop (cdr rest) acc))
              ;; 删除：目标整体移出上下文（只对 message 生效，compaction 等非消息条目不动）
              ((and edit (equal? type "message") (%delete-edit? replacement))
               (loop (cdr rest) acc)
              ) ;
              ;; 替换：仅 message 条目且 replacement 可规范化
              ((and edit (equal? type "message") (%applicable-replacement? replacement))
               (loop (cdr rest) (cons (%replace-entry-content entry replacement) acc))
              ) ;
              (else (loop (cdr rest) (cons entry acc)))
            ) ;cond
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 无法应用的编辑目标 id（去重，保持路径顺序）：
    ;; 目标不在裁剪结果内、目标不是 message、replacement 类型不可规范化
    (define (%ignored-edit-ids context-entries edits)
      (let loop
        ((rest (reverse edits)) (acc '()))
        (if (null? rest)
          (reverse acc)
          (let* ((edit (car rest))
                 (id (car edit))
                 (replacement (cdr edit))
                 (entry (%entry-by-id context-entries id))
                 (ignored
                   (or (not entry)
                     (not (equal? (%ref entry "type") "message"))
                     (and (not (%delete-edit? replacement))
                       (not (%applicable-replacement? replacement))
                     ) ;and
                   ) ;or
                 ) ;ignored
                ) ;
            (loop (cdr rest) (if (and ignored (not (member id acc))) (cons id acc) acc))
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; pi-apply-context-edits：应用裁剪结果上的全部 context_edit。
    ;; 返回 (values 应用后的条目 ignored 目标 id 列表)：
    ;; - 第一值为新条目列表，顺序与原裁剪结果一致，context_edit 自身不保留
    ;; - 第二值为无法应用的目标 id（去重，路径顺序），无则为 '()
    (define (pi-apply-context-edits context-entries)
      (let* ((edits (%collect-context-edits context-entries))
             (ignored (%ignored-edit-ids context-entries edits))
            ) ;
        (values (%apply-context-edits context-entries edits) ignored)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
