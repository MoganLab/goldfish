;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; `gf pi config`：ranger 风格三列配置浏览 TUI。
;;   ← → 切换激活列，↑ ↓ 在列内移动，Tab 循环切列，g/G 首尾，q/Esc/Ctrl+C 退出。
;;   space：启停（写 models/providers.json 或 models/<provider>.json）
;;   v：只读查看光标条目对应的配置文件（覆盖层）
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
  ) ;import
  (export pi-config-run pi-config-key-handler)
  (begin

    (define %columns 3)

    ;; ---------- 光标与导航（纯逻辑，便于单测） ----------

    (define (%ref alist key)
      (let ((pair (assoc key alist)))
        (if pair (cdr pair) 0)
      ) ;let
    ) ;define

    (define (%set-cursor cursor provider-index model-index)
      (list (cons 'provider provider-index) (cons 'model model-index))
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
        (and (pair? providers)
          (list-ref providers (%clamp index (- (length providers) 1)))
        ) ;and
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
          ;; 退出
          ((or (eq? key 'escape) (eq? key 'ctrl-c) (equal? c "q"))
           (list cursor active #t)
          ) ;
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

    ;; ---------- 初始光标 ----------

    (define (%initial-cursor st)
      (let* ((providers (pi-config-state-providers st))
             (index (list-index (lambda (p) (pi-config-provider-default? p)) providers))
            ) ;
        (%normalize-cursor st (%set-cursor (pi-config-cursor (or index 0) 0) 0 0))
      ) ;let*
    ) ;define

    ;; ---------- 覆盖层 ----------

    (define (%overlay-scroll-lines content-lines height)
      (max 1 (- (min (+ content-lines 2) (- height 4)) 2))
    ) ;define

    ;; 覆盖层的文本行（有文件看原文，虚拟条目看缺省说明；这里只需行数用于滚动）
    (define (%overlay-lines view)
      (if (pi-user-dict-ref view "exists" #f)
        (string-split (or (pi-user-dict-ref view "content" #f) "") "\n")
        '("not found" "" "")
      ) ;if
    ) ;define

    ;; ---------- Enter ----------

    ;; provider 列：等同右方向键（进入模型列）
    ;; 模型列/详情列：把选中模型设为默认模型（写 settings.json）
    (define (%handle-enter! st cursor active message-box)
      (if (= active 0)
        (pi-config-key-handler st cursor active 'right)
        (let* ((plan (pi-config-plan-set-default st cursor 1))
               (msg (pi-user-dict-ref plan "message" ""))
              ) ;
          (if (not (pi-user-dict-ref plan "ok" #f))
            (set! message-box msg)
            (let ((res (pi-config-apply-writes st (pi-user-dict-ref plan "writes" '()))))
              (set! message-box
                (if (pi-user-dict-ref res "ok" #f)
                  msg
                  (pi-user-dict-ref res "message" "save failed")
                ) ;if
              ) ;set!
            ) ;let
          ) ;if
          (list cursor active #f)
        ) ;let*
      ) ;if
    ) ;define

    ;; ---------- 启停（空格） ----------

    (define (%apply-toggle st cursor active message-box)
      (let* ((column (if (= active 0) 0 1))
             (plan (pi-config-plan-toggle st cursor column))
             (msg (pi-user-dict-ref plan "message" ""))
            ) ;
        (if (not (pi-user-dict-ref plan "ok" #f))
          (set! message-box msg)
          (let ((res (pi-config-apply-writes st (pi-user-dict-ref plan "writes" '()))))
            (if (pi-user-dict-ref res "ok" #f)
              (set! message-box msg)
              (set! message-box (pi-user-dict-ref res "message" "save failed"))
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    ;; ---------- 主循环 ----------

    (define* (pi-config-run config-dir (terminal #f) (env #f))
      (let* ((term (or terminal (make-standard-terminal)))
             (screen (make-main-screen term :auto-hide-cursor #t))
             (st (pi-config-state config-dir :env env))
             (cursor (%initial-cursor st))
             (active 0)
             (overlay #f)
             (overlay-offset 0)
             (message "")
             (done? #f)
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
        (define (size)
          (terminal-size term)
        ) ;define
        (define (render!)
          (let* ((s (size)) (w (car s)) (h (cadr s)))
            (if overlay
              (screen-render! screen
                (pi-config-render-overlay overlay w h overlay-offset message)
              ) ;screen-render!
              (screen-render! screen (pi-config-render st cursor active w h message))
            ) ;if
          ) ;let*
        ) ;define
        ;; 覆盖层的按键：滚动与关闭
        (define (overlay-key! key)
          (let* ((c (cond ((char? key) (string key))
                          ((symbol? key) (symbol->string key))
                          (else key)
                    ) ;cond
                 ) ;c
                 (h (cadr (size)))
                 (lines (%overlay-lines overlay))
                 (rows (%overlay-scroll-lines (length lines) h))
                ) ;
            (cond ((or (eq? key 'escape) (equal? c "q")) (set! overlay #f))
                  ((or (eq? key 'down) (equal? c "j")) (set! overlay-offset (+ overlay-offset 1)))
                  ((or (eq? key 'up) (equal? c "k"))
                   (set! overlay-offset (max 0 (- overlay-offset 1)))
                  ) ;
                  ((equal? c "g") (set! overlay-offset 0))
                  ((equal? c "G") (set! overlay-offset (max 0 (- (length lines) rows))))
                  (else #f)
            ) ;cond
          ) ;let*
        ) ;define
        (define (handle-key! key)
          (let* ((c (cond ((char? key) (string key))
                          ((symbol? key) (symbol->string key))
                          (else key)
                    ) ;cond
                 ) ;c
                ) ;
            (cond (overlay (overlay-key! key))
                  ((equal? c "v")
                   (set! overlay (pi-config-file-view st cursor (if (= active 0) 0 1)))
                   (set! overlay-offset 0)
                  ) ;
                  ((eq? key 'enter)
                   (let ((r (%handle-enter! st cursor active message)))
                     (set! cursor (%normalize-cursor st (car r)))
                     (set! active (cadr r))
                     (reload!)
                   ) ;let
                  ) ;
                  ((or (equal? c " ") (eq? key 'space))
                   (%apply-toggle st cursor active message)
                   (reload!)
                  ) ;
                  (else
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
                        (terminal-enter-raw-mode! term)
                        (terminal-hide-cursor! term)
                        (terminal-enter-alt-screen! term)
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
                      (sleep 0.02)
                      (loop)
                    ) ;begin
                  ) ;if
                ) ;let
              ) ;unless
            ) ;let
          ) ;lambda
          (lambda ()
            (catch #t (lambda () (terminal-exit-alt-screen! term)) (lambda args #f))
            (terminal-show-cursor! term)
            (catch #t (lambda () (terminal-exit-raw-mode! term)) (lambda args #f))
            (terminal-flush term)
          ) ;lambda
        ) ;dynamic-wind
        0
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
