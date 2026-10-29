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

;; pi-session-path
;; 从叶子条目沿 parentId 回溯到根，返回根到叶顺序的条目列表。
;;
;; 语法
;; ----
;; (pi-session-path entries)
;; (pi-session-path entries leaf-id)
;;
;; 参数
;; ----
;; entries : list
;; 已解析的 JSONL 条目列表（alist）。
;; leaf-id : string | #f
;; 可选叶子条目 id；缺省取文件中最后一条带 id 的条目。
;;
;; 返回值
;; ----
;; (values path reason)
;; path : list，根到叶顺序的条目；
;; reason : #f 或描述回溯中断原因的字符串（断链、成环、叶子不存在）。
;;
;; 示例
;; ----
;; (pi-session-path (list (make-entry \"a\" 'null) (make-entry \"b\" \"a\")) \"b\")
;; => 条目 (a b) 与 #f

;; 建一个 message 条目 alist：parentId 为字符串或 'null
(define (make-entry id parent-id)
  (list (cons "type" "message")
    (cons "id" id)
    (cons "parentId" parent-id)
    (cons "message" (list (cons "role" "user") (cons "content" id)))
  ) ;list
) ;define

(define (entry-id e) (cdr (assoc "id" e)))

;; 1. 线性会话：路径等于全部带 id 的条目，顺序与文件一致
(let*-values (((entries)
                (list (make-entry "a" 'null)
                  (make-entry "b" "a")
                  (make-entry "c" "b")
                ) ;list
              ) ;entries
              ((result reason) (pi-session-path entries)))
  (check (map entry-id result) => '("a" "b" "c"))
  (check (equal? result entries) => #t)
  (check reason => #f)
) ;let*-values

;; 2. 单次分叉：只返回叶子所在分支，另一分支不出现
(let*-values
  (
    ((fork-entries)
     (append
       (list (make-entry "root" 'null))
       (list (make-entry "b1" "root") (make-entry "b1-leaf" "b1"))
       (list (make-entry "b2" "root") (make-entry "b2-leaf" "b2"))
     ) ;append
    ) ;fork-entries
    ((result reason) (pi-session-path fork-entries "b1-leaf"))
  ) ;
  ;; 指定叶子：只走 b1 分支，b2 分支不混入
  (check (map entry-id result) => '("root" "b1" "b1-leaf"))
  (check reason => #f)
) ;let*-values
(let*-values
  (
    ((fork-entries)
     (append
       (list (make-entry "root" 'null))
       (list (make-entry "b1" "root") (make-entry "b1-leaf" "b1"))
       (list (make-entry "b2" "root") (make-entry "b2-leaf" "b2"))
     ) ;append
    ) ;fork-entries
    ((result reason) (pi-session-path fork-entries))
  ) ;
  ;; 缺省叶子：取文件中最后一条带 id 的条目（追加式布局），即 b2 分支
  (check (map entry-id result) => '("root" "b2" "b2-leaf"))
  (check reason => #f)
) ;let*-values

;; 3. 根条目 parentId 为 null（符号）时正确终止
(let*-values
  (((entries) (list (make-entry "a" 'null) (make-entry "b" "a")))
   ((result reason) (pi-session-path entries "b")))
  (check (map entry-id result) => '("a" "b"))
  (check reason => #f))

;; 4. 根条目 parentId 缺失时同样终止
(let*-values
  (
    ((entries)
     (list (list (cons "type" "message") (cons "id" "a"))
       (make-entry "b" "a")
     ) ;list
    ) ;entries
    ((result reason) (pi-session-path entries "b"))
  ) ;
  (check (map entry-id result) => '("a" "b"))
  (check reason => #f)
) ;let*-values

;; 5. parentId 指向不存在的条目：返回已收集路径，原因作为第二返回值
(let*-values
  (
    ((entries) (list (make-entry "a" 'null) (make-entry "ghost" "missing")))
    ((path reason) (pi-session-path entries "ghost"))
  ) ;let*-values 内部按序绑定
  (check (map entry-id path) => '("ghost"))
  (check-true (and (string? reason) (not (string-null? reason))))
) ;let*-values

;; 6. parentId 成环：以已访问 id 集合截断，原因作为第二返回值
(let*-values
  (
    ((entries)
     (list (make-entry "x" "y") (make-entry "y" "x") (make-entry "leaf" "x"))
     ) ;
    ((path reason) (pi-session-path entries "leaf"))
  ) ;let*-values
  ;; 从 leaf 回溯：leaf -> x -> y，y 的 parentId 指回已访问的 x，截断；
  ;; 返回已收集路径（最上层为截断处的 y）
  (check (map entry-id path) => '("y" "x" "leaf"))
  (check-true (and (string? reason) (not (string-null? reason))))
) ;let*-values

;; 7. 叶子 id 不存在：返回空列表，不抛异常
(let*-values
  (((path reason) (pi-session-path (list (make-entry "a" 'null)) "no-such")))
  (check path => '())
  (check-true (and (string? reason) (not (string-null? reason))))
) ;let*-values

;; 8. 空条目列表：空路径
(let*-values (((path reason) (pi-session-path '())))
  (check path => '())
  (check reason => #f))

;; 9. 缺省叶子跳过无 id 的条目（header 行），取最后一条带 id 条目
(let*-values
  (
    ((entries)
     (list (list (cons "type" "session") (cons "id" "session-1"))
       (make-entry "a" 'null)
       (make-entry "b" "a")
     ) ;list
    ) ;entries
    ((result reason) (pi-session-path entries))
  ) ;
  (check (map entry-id result) => '("a" "b"))
  (check reason => #f)
) ;let*-values

;; 9b. 缺省叶子跳过无 id 的行，但也取最后一条带 id 的 header 后条目
(let*-values
  (
    ((entries)
     (list (make-entry "a" 'null)
       (list (cons "type" "session_info") (cons "name" "demo"))
       (make-entry "b" "a")
     ) ;list
    ) ;entries
    ((result reason) (pi-session-path entries))
  ) ;
  (check (map entry-id result) => '("a" "b"))
  (check reason => #f)
) ;let*-values

;; 10. JSONL 真实落盘往返：经 save-session-header!/save-session-message! 写入，
;;     string->json 解析出的条目回溯结果与文件布局一致
(let* ((tmp-dir (path->string (path-join (path-temp-dir) "test-pi-session-path-1202")))
       (session-file (path->string (path-join tmp-dir "roundtrip.jsonl"))))
  (when (file-exists? tmp-dir) (rmtree tmp-dir))
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (save-session-header! session-file "sess-1202" "2026-10-22T08:00:00.000Z" tmp-dir)
  (save-session-message! session-file
    "{\"role\":\"user\",\"content\":\"first\"}" "m1" #f "2026-10-22T08:00:01.000Z")
  (save-session-message! session-file
    "{\"role\":\"assistant\",\"content\":\"second\"}" "m2" "m1"
    "2026-10-22T08:00:02.000Z")
  (save-session-message! session-file
    "{\"role\":\"user\",\"content\":\"third\"}" "m3" "m2"
    "2026-10-22T08:00:03.000Z")
  (let*-values
    (
      ((lines) (path-read-lines session-file))
      ((entries)
       (filter
         (lambda (e) e)
         (map (lambda (line)
                (catch #t
                  (lambda () (string->json (string-trim line)))
                  (lambda _ #f)))
           lines))
      ) ;entries
      ((result reason) (pi-session-path entries))
      ((prefix prefix-reason) (pi-session-path entries "m2"))
    ) ;
    (check (map entry-id result) => '("m1" "m2" "m3"))
    (check reason => #f)
    ;; 指定中间叶子：截取前缀
    (check (map entry-id prefix) => '("m1" "m2"))
    (check prefix-reason => #f)
  ) ;let*-values
  (rmtree tmp-dir)
) ;let*

(check-report)
