(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive session-selector)
) ;import

(check-set-mode! 'report-failed)

(define (info path id cwd name first modified count . all-text)
  (list (cons 'path path) (cons 'id id) (cons 'cwd cwd) (cons 'name name)
    (cons 'first-message first) (cons 'modified modified)
    (cons 'message-count count)
    (cons 'all-messages-text (if (pair? all-text) (car all-text) first))
  ) ;list
) ;define

(define OLD
  (info "/sessions/old.jsonl" "old-id" "/work/current" #f "old question"
    "2026-10-20T08:00:02.000Z" 2 "old question old answer")
) ;define
(define NEW
  (info "/sessions/new.jsonl" "new-id" "/work/current" "Named current session"
    "new question" "2026-10-21T08:00:01.000Z" 1)
) ;define
(define OTHER
  (info "/sessions/other.jsonl" "other-id" "/work/other" #f "other project"
    "2026-10-22T08:00:00.000Z" 0)
) ;define

(define selected '())
(define cancel-count 0)

(define (make-selector)
  (make-session-selector (list NEW OLD) (list OTHER NEW OLD)
    (lambda (path) (set! selected (cons path selected)))
    (lambda () (set! cancel-count (+ cancel-count 1)))
  ) ;make-session-selector
) ;define

(define (plain selector)
  (string-join (map strip-ansi (component-render selector 120)) "\n")
) ;define

;; 1. Current Folder 为默认 scope，显示名称/首条消息和数量，不混入其他项目。
(let ((selector (make-selector)))
  (check-true (session-selector? selector))
  (check (session-selector-scope selector) => 'current)
  (check (session-selector-sort-mode selector) => 'recent)
  (check (session-selector-name-filter selector) => 'all)
  (check (map (lambda (s) (cdr (assoc 'path s)))
           (session-selector-visible-sessions selector))
    => '("/sessions/new.jsonl" "/sessions/old.jsonl"))
  (let ((text (plain selector)))
    (check-true (string-contains? text "Resume Session (Current Folder)"))
    (check-true (string-contains? text "Sort: Recent"))
    (check-true (string-contains? text "Named current session"))
    (check-true (string-contains? text "old question"))
    (check (session-selector-format-age "2026-10-20T08:00:02.000Z" 1793077200) => "6d")
    (check-false (string-contains? text "other project"))
  ) ;let
) ;let

;; 2. Tab 切换 All 并显示 cwd；搜索覆盖全部用户/助手文本。
(let ((selector (make-selector)))
  (component-handle-input selector 'tab)
  (check (session-selector-scope selector) => 'all)
  (check-true (string-contains? (plain selector) "/work/other"))
  (component-handle-input selector "old answer")
  (check (map (lambda (s) (cdr (assoc 'path s)))
           (session-selector-visible-sessions selector))
    => '("/sessions/old.jsonl"))
) ;let

;; 3. Ctrl+N 只看命名会话，Ctrl+P 显示完整文件路径，Ctrl+S 切换相关度排序。
(let ((selector (make-selector)))
  (component-handle-input selector 'ctrl-n)
  (check (session-selector-name-filter selector) => 'named)
  (check (map (lambda (s) (cdr (assoc 'path s)))
           (session-selector-visible-sessions selector))
    => '("/sessions/new.jsonl"))
  (component-handle-input selector 'ctrl-p)
  (check-true (session-selector-show-path? selector))
  (check-true (string-contains? (plain selector) "/sessions/new.jsonl"))
  (component-handle-input selector 'ctrl-s)
  (check (session-selector-sort-mode selector) => 'relevance)
  (check-true (string-contains? (plain selector) "Sort: Fuzzy"))
) ;let

;; 4. 上下/Page 键不会越界，Enter 返回选中的确切路径。
(let ((selector (make-selector)))
  (component-handle-input selector 'down)
  (component-handle-input selector 'down)
  (component-handle-input selector 'page-down)
  (component-handle-input selector 'enter)
  (check selected => '("/sessions/old.jsonl"))
  (component-handle-input selector 'up)
  (component-handle-input selector 'up)
  (component-handle-input selector 'page-up)
  (component-handle-input selector 'enter)
  (check selected => '("/sessions/new.jsonl" "/sessions/old.jsonl"))
) ;let

;; 5. 搜索无匹配时显示空状态；Escape 与 Ctrl+C 只取消，不产生选择。
(let ((selector (make-selector)))
  (component-handle-input selector "no-such-session")
  (check (session-selector-visible-sessions selector) => '())
  (check-true (string-contains? (plain selector) "No sessions found"))
  (component-handle-input selector 'escape)
  (component-handle-input selector 'ctrl-c)
  (check cancel-count => 2)
  (check selected => '("/sessions/new.jsonl" "/sessions/old.jsonl"))
) ;let

(check-report)
