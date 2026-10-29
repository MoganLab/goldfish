;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 会话路径回溯：从叶子条目沿 parentId 回溯到根，得到根到叶顺序的路径条目。
;; 这是 fork 分支切换与后续投影（1203/1204）的基础。
(define-library (liii pi code core session-projection)
  (import (scheme base) (liii string) (liii json))
  (export pi-session-path)
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

  ) ;begin
) ;define-library
