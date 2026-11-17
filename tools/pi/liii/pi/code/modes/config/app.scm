;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; `gf pi config`：ranger 风格三列配置浏览 TUI（含 providers 与 all-in-one 子标签页）。
(define-library (liii pi code modes config app)
  (import (scheme base)
    (liii base)
    (liii list)
    (liii string)
    (liii time)
    (liii tui)
    (liii pi code config user-fs)
    (liii pi code modes config state)
    (liii pi code modes config layout)
    (liii pi code modes config overlay)
    (liii pi code modes config files)
    (liii pi code modes config json-write)
    (liii pi code modes config all-in-one)
    (liii pi code modes config app-aio)
    (liii pi code modes config aio-modal)
    (liii pi code modes config aio-modal-render)
  ) ;import
  (export pi-config-run pi-config-key-handler)
  (begin

    (define %columns 3)

    (define (%ref alist key)
      (let ((pair (assoc key alist)))
        (if pair (cdr pair) 0)
      ) ;let
    ) ;define

    (define (%set-cursor cursor provider-index model-index)
      (let ((tab (assoc 'tab cursor))
            (chain (assoc 'chain cursor))
            (node (assoc 'node cursor))
           ) ;
        (append (list (cons 'provider provider-index) (cons 'model model-index))
          (if tab (list tab) '())
          (if chain (list chain) '())
          (if node (list node) '())
        ) ;append
      ) ;let
    ) ;define

    (define (%set-tab-cursor cursor tab)
      (list (cons 'provider (%ref cursor 'provider))
        (cons 'model (%ref cursor 'model))
        (cons 'tab tab)
        (cons 'chain (%ref cursor 'chain))
        (cons 'node (%ref cursor 'node))
      ) ;list
    ) ;define

    (define (%aio-tab? cursor)
      (let ((tab (%ref cursor 'tab)))
        (or (eq? tab 'all-in-one) (equal? tab 1))
      ) ;let
    ) ;define

    (define (%clamp index count)
      (cond ((<= count 0) 0)
            ((< index 0) 0)
            ((>= index count) (- count 1))
            (else index)
      ) ;cond
    ) ;define

    (define (%provider-at st index)
      (let ((providers (pi-config-state-providers st)))
        (and (pair? providers) (list-ref providers (%clamp index (length providers))))
      ) ;let
    ) ;define

    ;; 当前光标指向的模型（按 cursor 的 model 下标）
    (define (%model-at-index st cursor)
      (let* ((p (%provider-at st (%ref cursor 'provider)))
             (models (if p (pi-config-provider-models p) '()))
             (i (%ref cursor 'model))
            ) ;
        (and (> (length models) i) (list-ref models i))
      ) ;let*
    ) ;define

    (define (%model-count st provider-index)
      (let ((p (%provider-at st provider-index)))
        (if p (length (pi-config-provider-models p)) 0)
      ) ;let
    ) ;define

    ;; 返回 (新光标 新激活列 是否退出)；键可为 symbol（特殊键）或字符串（普通字符）
    (define (pi-config-key-handler st cursor active key)
      (let* ((p-index (%ref cursor 'provider))
             (m-index (%ref cursor 'model))
             (p-count (max 1 (length (pi-config-state-providers st))))
             (m-count (max 1 (%model-count st p-index)))
             (c (cond ((char? key) (string key))
                      ((symbol? key) (symbol->string key))
                      (else key)
                ) ;cond
             ) ;c
            ) ;
        (cond
          ;; 退出：仅 q 与 escape
          ((or (eq? key 'escape) (equal? c "q")) (list cursor active #t))
          ;; 子标签页切换：1 = providers，2 = all-in-one，shift-tab / [ / ] = 循环切换
          ((equal? c "1") (list (%set-tab-cursor cursor 0) 0 #f))
          ((equal? c "2") (list (%set-tab-cursor cursor 1) 0 #f))
          ((or (eq? key 'shift-tab) (equal? c "[") (equal? c "]"))
           (list (%set-tab-cursor cursor (if (%aio-tab? cursor) 0 1)) 0 #f)
          ) ;
          ((%aio-tab? cursor) (pi-config-aio-key-handler st cursor active key))
          ;; 纵向移动
          ((or (eq? key 'up) (equal? c "k"))
           (list
             (if (= active 0)
               (%set-cursor cursor (%clamp (- p-index 1) p-count) m-index)
               (%set-cursor cursor p-index (%clamp (- m-index 1) m-count))
             ) ;if
             active
             #f
           ) ;list
          ) ;
          ((or (eq? key 'down) (equal? c "j"))
           (list
             (if (= active 0)
               (%set-cursor cursor (%clamp (+ p-index 1) p-count) m-index)
               (%set-cursor cursor p-index (%clamp (+ m-index 1) m-count))
             ) ;if
             active
             #f
           ) ;list
          ) ;
          ;; 右侧进入：最多到第三列
          ((or (eq? key 'right) (equal? c "l"))
           (list (%set-cursor cursor p-index (%clamp m-index m-count))
             (min (+ active 1) (- %columns 1))
             #f
           ) ;list
          ) ;
          ((or (eq? key 'left) (equal? c "h")) (list cursor (max (- active 1) 0) #f))
          ((eq? key 'tab) (list cursor (modulo (+ active 1) %columns) #f))
          ((equal? c "g")
           (list (if (= active 0) (%set-cursor cursor 0 m-index) (%set-cursor cursor p-index 0))
             active
             #f
           ) ;list
          ) ;
          ((equal? c "G")
           (list (if (= active 0)
                   (%set-cursor cursor (- p-count 1) m-index)
                   (%set-cursor cursor p-index (- m-count 1))
                 ) ;if
             active
             #f
           ) ;list
          ) ;
          (else (list cursor active #f))
        ) ;cond
      ) ;let*
    ) ;define

    ;; provider 变化时把模型光标夹回有效范围
    (define (%normalize-cursor st cursor)
      (let* ((p
               (%clamp (%ref cursor 'provider) (max 1 (length (pi-config-state-providers st))))
             ) ;p
             (m (%clamp (%ref cursor 'model) (max 1 (%model-count st p))))
            ) ;
        (%set-cursor cursor p m)
      ) ;let*
    ) ;define

    ;; 改配置后按 provider id / 模型 id 重新定位光标（默认项会置顶，位置会变）
    (define* (pi-config-relocate-cursor st cursor (p-id #f) (m-id #f))
      (let* ((providers (pi-config-state-providers st))
             (p
               (or
                 (list-index (lambda (x) (string=? (pi-config-provider-id x) (or p-id "")))
                   providers
                 ) ;list-index
                 0
               ) ;or
             ) ;p
             (models (if (pair? providers) (pi-config-provider-models (list-ref providers p)) '())
             ) ;models
             (m
               (or
                 (and m-id
                   (list-index (lambda (x) (string=? (pi-config-model-id x) m-id)) models)
                 ) ;and
                 0
               ) ;or
             ) ;m
            ) ;
        (%normalize-cursor st (%set-cursor cursor p m))
      ) ;let*
    ) ;define*

    (define (%initial-cursor st)
      (let ((idx (list-index pi-config-provider-default? (pi-config-state-providers st))))
        (%normalize-cursor st (pi-config-cursor (or idx 0) 0))
      ) ;let
    ) ;define

    (define (%apply-plan st plan)
      (let ((msg (pi-user-dict-ref plan "message" "")))
        (if (not (pi-user-dict-ref plan "ok" #f))
          msg
          (let ((res (pi-config-apply-writes st (pi-user-dict-ref plan "writes" '()))))
            (if (pi-user-dict-ref res "ok" #f)
              msg
              (pi-user-dict-ref res "message" "save failed")
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; nested = #t：由外层 TUI（如 `gf pi` 的 /config）调用，此时不进/退备用屏与
    ;; raw 模式（外层已开启），退出时只清屏并把控制权交回外层。
    (define* (pi-config-run config-dir (terminal #f) (env #f) (nested #f))
      (let* ((term (or terminal (make-standard-terminal)))
             (screen (make-main-screen term :auto-hide-cursor #t))
             (st (pi-config-state config-dir :env env))
             (cursor (%initial-cursor st))
             (active 0)
             (overlay #f)
             (overlay-offset 0)
             (aio-modal #f)
             (message "")
             (exit-mode 'quit)
             (done? #f)
             (last-size #f)
            ) ;
        (define (reload!)
          (let* ((provider (%provider-at st (%ref cursor 'provider)))
                 (p (if provider (pi-config-provider-id provider) #f))
                 (model (%model-at-index st cursor))
                 (m (if model (pi-config-model-id model) #f))
                ) ;
            (set! st (pi-config-state config-dir :env env))
            (set! cursor (pi-config-relocate-cursor st cursor p m))
          ) ;let*
        ) ;define
        (define (render!)
          (let* ((s (terminal-size term)) (w (car s)) (h (cadr s)))
            (unless (equal? s last-size)
              (set! last-size s)
              (screen-clear! screen)
            ) ;unless
            (cond (aio-modal (screen-render! screen (pi-config-aio-modal-render aio-modal w h)))
                  (overlay (screen-render! screen
                             (pi-config-render-overlay overlay w h overlay-offset message)
                           ) ;screen-render!
                  ) ;overlay
                  (else (screen-render! screen (pi-config-render st cursor active w h message)))
            ) ;cond
          ) ;let*
        ) ;define
        (define (handle-key! key)
          (let* ((c (cond ((char? key) (string key))
                          ((symbol? key) (symbol->string key))
                          (else key)
                    ) ;cond
                 ) ;c
                 (aio-act (and (%aio-tab? cursor) (pi-config-aio-action st cursor active key)))
                ) ;
            (cond
              (aio-modal
                (let ((r (pi-config-aio-modal-handle st aio-modal key)))
                  (set! aio-modal (car r))
                  (when (caddr r)
                    (set! message (cadr r))
                    (reload!)
                  ) ;when
                ) ;let
              ) ;aio-modal
              (overlay
                (let ((res (pi-config-overlay-step overlay overlay-offset (cadr (terminal-size term)) key)
                      ) ;res
                     ) ;
                  (set! overlay (car res))
                  (set! overlay-offset (cadr res))
                ) ;let
              ) ;overlay
              (aio-act
                (let ((r (pi-config-aio-exec-action st cursor aio-act)))
                  (set! aio-modal (car r))
                  (set! message (cadr r))
                  (when (caddr r)
                    (reload!)
                  ) ;when
                ) ;let
              ) ;aio-act
              ((equal? c "v")
               (set! overlay
                 (if (%aio-tab? cursor)
                   (pi-config-aio-file-view st)
                   (pi-config-file-view st cursor (if (= active 0) 0 1))
                 ) ;if
               ) ;set!
               (set! overlay-offset 0)
              ) ;
              ((and (eq? key 'enter) (not (%aio-tab? cursor)))
               (if (= active 0)
                 (let ((r (pi-config-key-handler st cursor active 'right)))
                   (set! cursor (%normalize-cursor st (car r)))
                   (set! active (cadr r))
                 ) ;let
                 (begin
                   (set! message (%apply-plan st (pi-config-plan-set-default st cursor 1)))
                   (reload!)
                 ) ;begin
               ) ;if
              ) ;
              ((or (equal? c " ") (eq? key 'space))
               (if (%aio-tab? cursor)
                 (set! message "switch to providers tab (1) to toggle")
                 (begin
                   (set! message
                     (%apply-plan st (pi-config-plan-toggle st cursor (if (= active 0) 0 1)))
                   ) ;set!
                   (reload!)
                 ) ;begin
               ) ;if
              ) ;
              (else
                (when (or (equal? c "q") (eq? key 'escape))
                  (set! exit-mode (if (equal? c "q") 'chat 'quit))
                ) ;when
                (let ((r (pi-config-key-handler st cursor active key)))
                  (set! cursor (%normalize-cursor st (car r)))
                  (set! active (cadr r))
                  (when (caddr r)
                    (set! done? #t)
                  ) ;when
                ) ;let
              ) ;else
            ) ;cond
          ) ;let*
        ) ;define
        (dynamic-wind (lambda ()
                        (unless nested
                          (terminal-enter-raw-mode! term)
                          (terminal-hide-cursor! term)
                          (terminal-enter-alt-screen! term)
                        ) ;unless
                        (terminal-hide-cursor! term)
                        (render!)
                      ) ;lambda
          (lambda ()
            (let loop
              ()
              (unless done?
                (let ((key (terminal-poll-key term)))
                  (if key
                    (begin
                      (set! message "")
                      (handle-key! key)
                      (unless done?
                        (render!)
                        (loop)
                      ) ;unless
                    ) ;begin
                    (begin
                      (unless (equal? (terminal-size term) last-size)
                        (render!)
                      ) ;unless
                      (sleep 0.02)
                      (loop)
                    ) ;begin
                  ) ;if
                ) ;let
              ) ;unless
            ) ;let
          ) ;lambda
          (lambda ()
            (if nested
              (catch #t (lambda () (screen-clear! screen)) (lambda args #f))
              (begin
                (catch #t (lambda () (terminal-exit-alt-screen! term)) (lambda args #f))
                (catch #t (lambda () (terminal-exit-raw-mode! term)) (lambda args #f))
              ) ;begin
            ) ;if
            (terminal-show-cursor! term)
            (terminal-flush term)
          ) ;lambda
        ) ;dynamic-wind
        exit-mode
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
