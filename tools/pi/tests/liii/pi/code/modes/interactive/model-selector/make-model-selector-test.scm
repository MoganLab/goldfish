(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive model-selector)
) ;import

(check-set-mode! 'report-failed)

(define %models
  (list (list (cons 'provider "google") (cons 'id "gemini-2.5-pro")
         (cons 'name "Gemini 2.5 Pro") (cons 'reasoning #t))
    (list (cons 'provider "google") (cons 'id "gemini-3-flash-preview")
      (cons 'name "Gemini 3 Flash Preview") (cons 'reasoning #t))
    (list (cons 'provider "zai") (cons 'id "glm-5")
      (cons 'name "GLM 5") (cons 'reasoning #t))
  ) ;list
) ;define

(define selected '())
(define saved '())
(define cancel-count 0)

;; 辅助函数：无参数用默认 6 参；字符串参数为初始搜索词；
;; 其余情况（≥6 个参数）直接透传给 make-model-selector
(define (make-selector . opt-args)
  (cond
   ((null? opt-args)
    (apply make-model-selector
      (list (list (cons 'provider "google") (cons 'id "gemini-2.5-pro"))
        #f
        %models
        (lambda (model) (set! selected (cons model selected)))
        (lambda () (set! cancel-count (+ cancel-count 1)))
        (lambda (model) (set! saved (cons model saved)))
      ) ;list
    ) ;apply
   ) ;null?
   ((string? (car opt-args))
    (apply make-model-selector
      (append
        (list (list (cons 'provider "google") (cons 'id "gemini-2.5-pro"))
          #f
          %models
          (lambda (model) (set! selected (cons model selected)))
          (lambda () (set! cancel-count (+ cancel-count 1)))
          (lambda (model) (set! saved (cons model saved)))
        ) ;list
        opt-args
      ) ;append
    ) ;apply
   ) ;string?
   (else (apply make-model-selector opt-args))
  ) ;cond
) ;define

;; 1. 布局与排序（对齐上游 ModelSelectorComponent）：
;;    当前模型置顶并标 ✓，行格式 id [provider]，描述列显示模型名；
;;    header 提示只显示已配置 provider 的模型。
(let* ((selector (make-selector))
       (lines (map strip-ansi (component-render selector 100))))
  (check-true (model-selector? selector))
  (check (length lines) => 14)
  (check (list-ref lines 2) => "Model")
  (check (list-ref lines 4) => "Only showing models from configured providers.")
  (check (list-ref lines 6) => "> ")
  (check-true (string-contains? (list-ref lines 8) "✓ gemini-2.5-pro"))
  (check-true (string-contains? (list-ref lines 8) "[google]"))
  (check-true (string-contains? (list-ref lines 8) "Gemini 2.5 Pro"))
  (check-true (string-contains? (list-ref lines 9) "gemini-3-flash-preview"))
  (check-true (string-contains? (list-ref lines 10) "glm-5"))
  (check-true (string-contains? (list-ref lines 10) "[zai]"))
  (check (list-ref lines 12) => "  enter to select · ctrl+s to set as default · esc to cancel")
  (check (select-list-selected-index (model-selector-list selector)) => 0)
  (check (model-selector-cursor-position selector) => '(6 2))
) ;let*

;; 2. fuzzy 搜索即时过滤（provider/id 复合、裸 id、name 均可命中，
;;    按匹配分数排序后最佳命中置顶），Enter 选择当前项。
(let ((selector (make-selector)))
  (component-handle-input selector "glm")
  (let ((items (select-list-items (model-selector-list selector))))
    (check-true (>= (length items) 1))
    (check (cdr (assoc 'id (cdr (assoc 'value (car items))))) => "glm-5")
  ) ;let
  (check (model-selector-cursor-position selector) => '(6 5))
  (component-handle-input selector 'enter)
  (check (length selected) => 1)
  (check (cdr (assoc 'id (car selected))) => "glm-5")
  (check (cdr (assoc 'provider (car selected))) => "zai")
) ;let

(let ((selector (make-selector)))
  ;; provider 前缀复合搜索：仅命中 google 两个 gemini 模型
  (component-handle-input selector "google/gemini")
  (check (length (select-list-items (model-selector-list selector))) => 2)
) ;let

(let ((selector (make-selector)))
  ;; name 搜索：最佳命中为 glm-5
  (component-handle-input selector "GLM")
  (let ((items (select-list-items (model-selector-list selector))))
    (check-true (>= (length items) 1))
    (check (cdr (assoc 'id (cdr (assoc 'value (car items))))) => "glm-5")
  ) ;let
) ;let

;; 3. Ctrl+S 保存选中项为默认模型；Escape 与 Ctrl+C 均取消。
(let ((selector (make-selector)))
  (component-handle-input selector 'down)
  (component-handle-input selector 'ctrl-s)
  (check (length saved) => 1)
  (check (cdr (assoc 'id (car saved))) => "gemini-3-flash-preview")
  (component-handle-input selector 'escape)
  (component-handle-input selector 'ctrl-c)
  (check cancel-count => 2)
) ;let

;; 4. 无匹配时显示提示。
(let ((selector (make-selector)))
  (component-handle-input selector "zzz")
  (check-true
    (string-contains?
      (string-join (map strip-ansi (component-render selector 100)) "\n")
      "No matching commands"))
) ;let

;; 5. 默认模型在描述列标记 · default（对齐上游 defaultBadge：当前第一、默认第二）。
(let* ((selector (make-selector
                   (list (cons 'provider "google") (cons 'id "gemini-2.5-pro"))
                   (list (cons 'provider "zai") (cons 'id "glm-5"))
                   %models
                   (lambda (model) #f)
                   (lambda () #f)
                   (lambda (model) #f)))
       (lines (map strip-ansi (component-render selector 100))))
  (check-true (string-contains? (list-ref lines 9) "glm-5"))
  (check-true (string-contains? (list-ref lines 9) "GLM 5 · default"))
) ;let*

;; 6. 初始搜索词预填（/model <query> 无精确匹配时的回退入口）。
(let ((selector (make-selector "flash")))
  (check (editor-text (model-selector-search selector)) => "flash")
  (check (length (select-list-items (model-selector-list selector))) => 1)
) ;let

(check-report)
