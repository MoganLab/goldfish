;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; `gf pi config`：ranger 风格三列只读配置浏览 TUI。
;;   ← → 切换激活列，↑ ↓ 在列内移动，Tab 循环切列，g/G 首尾，q/Esc/Ctrl+C 退出。
;; 只读：运行期不写任何配置文件。
(define-library (liii pi code modes config app)
  (import (scheme base)
    (liii base)
    (liii list)
    (liii string)
    (liii time)
    (liii tui)
    (liii pi code modes config state)
    (liii pi code modes config layout)
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
             ;; 普通键在真实终端是字符串、在虚拟终端可能是符号，统一成字符串比较
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
          ;; 右侧进入：无右列时停在第二列
          ((or (eq? key 'right) (eq? key 'enter) (equal? c "l"))
           (list (%set-cursor cursor p-index (%clamp m-index m-count))
             (min (+ active 1) 1)
             #f
           ) ;list
          ) ;
          ((or (eq? key 'left) (equal? c "h")) (list cursor (max (- active 1) 0) #f))
          ((eq? key 'tab)
           ;; 只在有右列时允许进入第三列
           (list cursor (modulo (+ active 1) 2) #f)
          ) ;
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

    ;; ---------- 初始光标 ----------

    (define (%initial-cursor st)
      (let* ((providers (pi-config-state-providers st))
             (index (list-index (lambda (p) (pi-config-provider-default? p)) providers))
            ) ;
        (%set-cursor (%normalize-cursor st (pi-config-cursor (or index 0) 0)) 0 0)
      ) ;let*
    ) ;define

    ;; ---------- 主循环 ----------

    (define* (pi-config-run config-dir (terminal #f) (env #f))
      (let* ((st (pi-config-state config-dir :env (if env env #f)))
             (term (or terminal (make-standard-terminal)))
             (screen (make-main-screen term :auto-hide-cursor #t))
             (cursor (%initial-cursor st))
             (active 0)
             (done? #f)
             (render!
               (lambda ()
                 (let ((size (terminal-size term)))
                   (screen-render! screen
                     (pi-config-render st cursor active (car size) (cadr size))
                   ) ;screen-render!
                 ) ;let
               ) ;lambda
             ) ;render!
            ) ;
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
                    (let ((r (pi-config-key-handler st cursor active key)))
                      (set! cursor (%normalize-cursor st (car r)))
                      (set! active (cadr r))
                      (when (caddr r)
                        (set! done? #t)
                      ) ;when
                      (unless done?
                        (render!)
                        (loop)
                      ) ;unless
                    ) ;let
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
