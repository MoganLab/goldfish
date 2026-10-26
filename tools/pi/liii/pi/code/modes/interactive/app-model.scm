;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; /model 命令：选择器交互、精确引用切换、thinking 联动与持久化
;; （对齐上游 handleModelCommand + session.setModel）
(define-library (liii pi code modes interactive app-model)
  (export pi-app-model-command? pi-app-submit-model!)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii list)
    (liii tui)
    (liii pi code modes interactive state)
    (liii pi code modes interactive render)
    (liii pi code modes interactive views)
    (liii pi code modes interactive footer)
    (liii pi code modes interactive thinking)
    (liii pi code modes interactive app-thinking)
    (liii pi code modes interactive model-selector)
  ) ;import
  (begin

    (define (pi-app-model-command? text)
      (or (string=? text "/model") (string-starts? text "/model "))
    ) ;define

    (define (%model-ref model key)
      (cdr (or (assoc key model) (cons #f #f)))
    ) ;define

    ;; 精确引用匹配（对齐上游 findExactModelReferenceMatch）：
    ;; provider/id 复合引用或裸 id，大小写不敏感
    (define (%find-exact-model query models)
      (let ((q (string-downcase (string-trim-both query))))
        (find
          (lambda (model)
            (or (string=? (string-downcase (%model-ref model 'id)) q)
              (string=? (string-append (string-downcase (%model-ref model 'provider))
                          "/"
                          (string-downcase (%model-ref model 'id))
                        ) ;string-append
                q
              ) ;string=?
            ) ;or
          ) ;lambda
          (or models '())
        ) ;find
      ) ;let
    ) ;define

    ;; 应用模型切换（对齐上游 setModel）：
    ;; - 状态与 footer 的 provider/model/supports 联动
    ;; - thinking 回退到默认级别并按模型能力收敛（不支持 reasoning 时为 off）
    ;; - persist? 时触发 on-save-model 持久化 settings 默认模型
    (define (%apply-model! app model persist?)
      (let* ((provider (%model-ref model 'provider))
             (model-id (%model-ref model 'id))
             (supports (and (%model-ref model 'reasoning) #t))
             (new-tl (if supports (pi-app-default-thinking-level app) "off"))
             (footer (%pi-app-footer app))
            ) ;
        (%pi-app-provider-set! app provider)
        (%pi-app-model-set! app model-id)
        (%pi-app-supports-thinking?-set! app supports)
        (%pi-app-available-thinking-levels-set! app
          (if supports pi-thinking-levels '("off"))
        ) ;%pi-app-available-thinking-levels-set!
        (%pi-app-thinking-level-set! app new-tl)
        (pi-footer-set-model! footer provider model-id supports)
        (pi-footer-set-thinking-level! footer new-tl)
        (editor-set-border-color! (pi-app-editor app) (pi-thinking-border-color new-tl))
        (when persist?
          (let ((save (%pi-app-on-save-model app)))
            (when (procedure? save)
              (save provider model-id)
            ) ;when
          ) ;let
        ) ;when
        (%pi-app-active-selector-set! app #f)
        (editor-set-focused! (pi-app-editor app) #t)
        (container-add! (%pi-app-chat app)
          (make-assistant-card (if persist?
                                 (string-append "Default model: " provider "/" model-id)
                                 (string-append "Model: " model-id)
                               ) ;if
          ) ;make-assistant-card
        ) ;container-add!
        (pi-app-render! app)
      ) ;let*
    ) ;define

    (define (%show-model-selector! app initial-search)
      (editor-set-focused! (pi-app-editor app) #f)
      (%pi-app-active-selector-set! app
        (make-model-selector (list (cons 'provider (pi-app-provider app)) (cons 'id (pi-app-model app)))
          (%pi-app-default-model app)
          (pi-app-models app)
          (lambda (model) (%apply-model! app model #f))
          (lambda () (pi-app-close-selector! app))
          (lambda (model) (%apply-model! app model #t))
          initial-search
        ) ;make-model-selector
      ) ;%pi-app-active-selector-set!
      (pi-app-render! app)
    ) ;define

    ;; /model：无参数开选择器；精确引用直接切换；否则回退选择器并预填搜索词
    (define (pi-app-submit-model! app text)
      (editor-set-text! (pi-app-editor app) "")
      (let* ((query (string-trim-both (substring text 6)))
             (model (and (not (string-null? query)) (%find-exact-model query (pi-app-models app)))
             ) ;model
            ) ;
        (if model (%apply-model! app model #f) (%show-model-selector! app query))
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
