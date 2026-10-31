;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; `gf pi config` 的 ranger 风格三列纯文本渲染（无副作用，便于单测）。
;;   左列：provider 白名单        中列：该 provider 的模型
;;   右列：选中模型的详情预览      底两行：状态栏 + 键位提示
(define-library (liii pi code modes config layout)
  (import (scheme base)
    (liii base)
    (liii list)
    (liii string)
    (liii unicode)
    (liii tui)
    (liii pi code modes config state)
  ) ;import
  (export pi-config-column-widths pi-config-cursor pi-config-render)
  (begin

    (define %accent "\x1b;[35m")
    (define %muted "\x1b;[38;2;120;130;140m")
    (define %reset "\x1b;[0m")
    ;; 选中行：紫色底 + 近白前景（ranger 风格整行高亮）。
    ;; 焦点列用浓紫，非焦点列用浅紫，保持"选中但未聚焦"的层次。
    (define %selected-style "\x1b;[48;2;74;44;122m\x1b;[38;2;244;240;255m")
    (define %selected-style-idle "\x1b;[48;2;56;46;78m\x1b;[38;2;205;198;220m")
    (define %dim "\x1b;[2m")
    (define %on "\x1b;[32m")

    ;; ---------- 光标 ----------

    (define (pi-config-cursor provider-index model-index)
      (list (cons 'provider provider-index) (cons 'model model-index))
    ) ;define

    (define (%cursor-ref cursor key)
      (let ((pair (assoc key cursor)))
        (if pair (cdr pair) 0)
      ) ;let
    ) ;define

    ;; ---------- 列宽 ----------

    ;; 返回 (左 中 右)；右列不足 12 列时置 0（隐藏），左列按剩余宽度收缩
    (define (pi-config-column-widths width)
      (let* ((w (max width 1)) (raw-right (- w 56)))
        (if (>= raw-right 12)
          (list 22 34 raw-right)
          (let ((left (min 22 (max 10 (quotient w 2)))))
            (list left (max 8 (- w left)) 0)
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    ;; ---------- 文本工具 ----------

    ;; 把行列表补齐/裁剪到恰好 rows 行
    (define (%pad lines rows)
      (let ((n (length lines)))
        (cond ((= n rows) lines)
              ((> n rows) (take lines rows))
              (else
                (append lines (map (lambda (i) "") (iota (- rows n))))
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (%fit text width)
      (let* ((plain (strip-ansi text)) (len (display-width plain)))
        (cond ((= width 0) "")
              ((<= len width) (string-append text (utf8-make-string (- width len) #\space)))
              ((<= (string-length plain) width) plain)
              (else (truncate-to-width plain width))
        ) ;cond
      ) ;let*
    ) ;define

    ;; ---------- 单列表格 ----------

    ;; 返回 (顶边 内容行... 底边)，内容行恰好 rows 行（不足补空、超出截断），
    ;; 每行已按 width 补齐
    (define (%box title lines width active? rows)
      (let* ((color (if active? %accent %muted))
             (sel-style (if active? %selected-style %selected-style-idle))
             (inner (- width 4))
             (title-text (string-append " " title " "))
             (span (+ inner 2))
             (head (string-append color "┌" (%fit title-text span) "┐" %reset))
             (foot (string-append color "└" (utf8-make-string span #\─) "┘" %reset))
             (body
               (map
                 (lambda (line)
                   (let* ((sel? (%selected? line))
                          (cell (string-append " " (%fit (%strip-marker line) inner) " "))
                         ) ;
                     (if sel?
                       (string-append color "│" %reset sel-style cell %reset color "│" %reset)
                       (string-append color "│" %reset cell color "│" %reset)
                     ) ;if
                   ) ;let*
                 ) ;lambda
                 (%pad lines rows)
               ) ;map
             ) ;body
            ) ;
        (cons head (append body (list foot)))
      ) ;let*
    ) ;define

    ;; 以选中索引为中心的滚动窗口
    (define (%window items count selected)
      (if (<= (length items) count)
        items
        (let* ((half (quotient count 2))
               (start
                 (max 0 (min (- (length items) count) (- selected half)))
               ) ;start
              ) ;
          (list-tail items start)
        ) ;let*
      ) ;if
    ) ;define

    ;; ---------- 三列数据 ----------

    (define (%provider-lines providers selected count)
      (map
        (lambda (p)
          (let* ((id (pi-config-provider-id p))
                 (mark (if (pi-config-provider-default? p) " *" ""))
                 (text (string-append id mark))
                ) ;
            (%highlight text selected)
          ) ;let*
        ) ;lambda
        (%window providers count selected)
      ) ;map
    ) ;define

    ;; 需要知道 window 起点才能判断哪一行是选中行
    (define (%window-start total count selected)
      (if (<= total count)
        0
        (max 0 (min (- total count) (- selected (quotient count 2))))
      ) ;if
    ) ;define

    ;; 列表行：可用 ● 绿色，不可用 × 暗显；选中行交给 %box 整行反显（ranger 风格）。
    ;; 选中行本身不带任何内嵌 SGR —— 否则中间的 %reset 会提前关掉反显，
    ;; 高亮就只剩左边一小截。
    (define %selected-marker "\x00;")

    (define (%entry-line text enabled? default? selected?)
      (let* ((tail (if default? " *" ""))
             (plain (string-append (if enabled? "●" "×") " " text tail))
            ) ;
        (if selected?
          (string-append %selected-marker plain)
          (let* ((mark (if enabled? (string-append %on "●" %reset) (string-append %dim "×" %reset))
                 ) ;mark
                 (body (string-append (if enabled? "" %dim) text tail (if enabled? "" %reset)))
                ) ;
            (string-append mark " " body)
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (%selected? line)
      (string-starts? line %selected-marker)
    ) ;define

    (define (%strip-marker line)
      (if (%selected? line)
        (substring line (string-length %selected-marker) (string-length line))
        line
      ) ;if
    ) ;define

    (define (%list-lines items count selected render)
      (let* ((total (length items))
             (start (%window-start total count selected))
             (visible (list-tail items start))
             (trimmed (if (> (length visible) count) (take visible count) visible))
            ) ;
        (let loop
          ((rest trimmed) (index start) (acc '()))
          (if (null? rest)
            (reverse acc)
            (loop (cdr rest) (+ index 1) (cons (render (car rest) (= index selected)) acc))
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    ;; 第三列详情：标签左对齐到固定宽度（暗色），值统一从同一列开始
    (define %detail-label-width 10)

    (define (%detail-lines rows)
      (let ((label-width
              (apply max
                %detail-label-width
                (map (lambda (row) (display-width (car row))) rows)
              ) ;apply
            ) ;label-width
           ) ;
        (map
          (lambda (row)
            (let* ((label (car row))
                   (pad (- label-width (display-width label)))
                   (label-cell (string-append %dim label %reset (utf8-make-string (+ pad 2) #\space))
                   ) ;label-cell
                  ) ;
              (string-append label-cell (cadr row))
            ) ;let*
          ) ;lambda
          rows
        ) ;map
      ) ;let
    ) ;define

    ;; ---------- 主渲染 ----------

    (define (pi-config-render st cursor active width height)
      (let* ((widths (pi-config-column-widths width))
             (w1 (car widths))
             (w2 (cadr widths))
             (w3 (caddr widths))
             (providers (pi-config-state-providers st))
             (provider-count (max 1 (length providers)))
             (p-index (min (max 0 (%cursor-ref cursor 'provider)) (- provider-count 1)))
             (provider (list-ref providers p-index))
             (models (pi-config-provider-models provider))
             (model-count (max 1 (length models)))
             (m-index (min (max 0 (%cursor-ref cursor 'model)) (- model-count 1)))
             (model (and (pair? models) (list-ref models m-index)))
             ;; 上下各留一行边框 + 状态栏 + 提示行
             (content-rows (max 1 (- height 4)))
            ) ;
        (let* ((col1
                 (%box (string-append "providers "
                         (number->string (pi-config-state-enabled-provider-count st))
                         "/"
                         (number->string (length providers))
                         " on"
                       ) ;string-append
                   (%list-lines providers
                     content-rows
                     p-index
                     (lambda (p selected?)
                       (%entry-line (pi-config-provider-id p)
                         (pi-config-provider-enabled? p)
                         (pi-config-provider-default? p)
                         selected?
                       ) ;%entry-line
                     ) ;lambda
                   ) ;%list-lines
                   w1
                   (= active 0)
                   content-rows
                 ) ;%box
               ) ;col1
               (col2
                 (%box (string-append "models · "
                         (pi-config-provider-id provider)
                         (if (pi-config-provider-enabled? provider) "" " ×")
                       ) ;string-append
                   (%list-lines models
                     content-rows
                     m-index
                     (lambda (m selected?)
                       (%entry-line (pi-config-model-id m)
                         (and (pi-config-provider-enabled? provider) (pi-config-model-enabled? m))
                         (pi-config-model-default? m)
                         selected?
                       ) ;%entry-line
                     ) ;lambda
                   ) ;%list-lines
                   w2
                   (= active 1)
                   content-rows
                 ) ;%box
               ) ;col2
               (col3
                 (if (or (= w3 0) (not model))
                   '()
                   (%box (string-append "preview · " (pi-config-model-id model))
                     (%detail-lines (pi-config-model-detail-rows model provider))
                     w3
                     (= active 2)
                     content-rows
                   ) ;%box
                 ) ;if
               ) ;col3
               (body
                 (if (null? col3)
                   (map (lambda (a b) (string-append a b)) col1 col2)
                   (map (lambda (a b c) (string-append a b c)) col1 col2 col3)
                 ) ;if
               ) ;body
               (status
                 (%fit (string-append " "
                         (pi-config-state-dir st)
                         " · "
                         (number->string (pi-config-state-enabled-provider-count st))
                         "/"
                         (number->string (pi-config-state-provider-count st))
                         " providers · "
                         (number->string (pi-config-state-enabled-model-count st))
                         "/"
                         (number->string (pi-config-state-model-count st))
                         " models · default "
                         (or (pi-config-state-default-provider st) "—")
                         "/"
                         (or (pi-config-state-default-model st) "—")
                       ) ;string-append
                   width
                 ) ;%fit
               ) ;status
               (hint (%fit " ↑↓/jk move · →/Enter enter · ←/h back · Tab column · g/G top/bottom · q quit"
                       width
                     ) ;%fit
               ) ;hint
              ) ;
          (append body (list status hint))
        ) ;let*
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
