;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui layout)
  (import (scheme base)
    (liii base)
    (liii error)
    (liii tui box)
    (liii tui main-screen)
    (liii tui terminal)
  ) ;import
  (export <tui-rect> make-tui-rect tui-rect? rect-top rect-left rect-width
    rect-height <tui-layout> make-tui-layout tui-layout? layout-cols layout-rows
    layout-messages-rect layout-editor-rect layout-status-rect layout-narrow?
    layout-compact? compute-editor-height compute-tui-layout
    detect-terminal-dimensions handle-screen-resize!
  ) ;export
  (begin

    (define-record-type <tui-rect>
      (make-tui-rect top left width height)
      tui-rect?
      (top rect-top)
      (left rect-left)
      (width rect-width)
      (height rect-height)
    ) ;define-record-type

    (define <tui-rect> '<tui-rect>)

    (define-record-type <tui-layout>
      (make-tui-layout cols rows messages-rect editor-rect status-rect narrow?
        compact?
      ) ;make-tui-layout
      tui-layout?
      (cols layout-cols)
      (rows layout-rows)
      (messages-rect layout-messages-rect)
      (editor-rect layout-editor-rect)
      (status-rect layout-status-rect)
      (narrow? layout-narrow?)
      (compact? layout-compact?)
    ) ;define-record-type

    (define <tui-layout> '<tui-layout>)

    ;; 计算输入框高度
    ;; rows: 终端总行数
    ;; input-line-count: 当前输入框行数（默认至少 1 行）
    ;; 规则：
    ;; 1. 基础高度 = input-line-count + 2（包含上下边框）
    ;; 2. 最小高度 3 行（1 行内容 + 2 行边框）
    ;; 3. 最大高度不超过 rows * 40%（即 (floor (* rows 0.4))），但若 rows 很小至少保证 3 行（当 rows >= 3 时）
    (define (compute-editor-height rows input-line-count)
      (unless (and (exact-integer? rows) (> rows 0))
        (type-error "compute-editor-height: expected positive integer rows")
      ) ;unless
      (unless (and (exact-integer? input-line-count) (>= input-line-count 0))
        (type-error "compute-editor-height: expected non-negative integer input-line-count"
        ) ;type-error
      ) ;unless
      (let* ((lines (max 1 input-line-count))
             (desired (+ lines 2))
             (max-allowed
               (max 3 (inexact->exact (floor (* rows 0.4))))
             ) ;max-allowed
            ) ;
        (if (<= rows 3) rows (min rows (max 3 (min desired max-allowed))))
      ) ;let*
    ) ;define

    ;; 计算整体屏幕三段式拓扑
    ;; cols: 终端列宽
    ;; rows: 终端行高
    ;; editor-lines: 输入框当前行数
    (define (compute-tui-layout cols rows . maybe-editor-lines)
      (unless (and (exact-integer? cols) (> cols 0))
        (type-error "compute-tui-layout: expected positive integer cols")
      ) ;unless
      (unless (and (exact-integer? rows) (> rows 0))
        (type-error "compute-tui-layout: expected positive integer rows")
      ) ;unless
      (let* ((lines (if (pair? maybe-editor-lines) (car maybe-editor-lines) 1))
             (narrow? (< cols 60))
             (compact? (or narrow? (< rows 15)))
             ;; 状态栏高度：极小屏 (< 10 行) 为 0，compact 屏 1 行，常规屏 1 行
             (status-height (cond ((< rows 10) 0) (else 1)))
             (remaining-rows (max 0 (- rows status-height)))
             (editor-height
               (if (<= remaining-rows 0)
                 0
                 (let ((ed-h (compute-editor-height rows lines)))
                   (min remaining-rows ed-h)
                 ) ;let
               ) ;if
             ) ;editor-height
             (messages-height (max 0 (- remaining-rows editor-height)))
             ;; 计算各个区域的 Rect
             ;; 顶部：messages-rect (0, 0, cols, messages-height)
             (msg-rect (make-tui-rect 0 0 cols messages-height))
             ;; 中部：editor-rect (messages-height, 0, cols, editor-height)
             (ed-rect (make-tui-rect messages-height 0 cols editor-height))
             ;; 底部：status-rect (+ messages-height editor-height, 0, cols, status-height)
             (st-rect (make-tui-rect (+ messages-height editor-height) 0 cols status-height))
            ) ;
        (make-tui-layout cols rows msg-rect ed-rect st-rect narrow? compact?)
      ) ;let*
    ) ;define

    ;; 动态感知终端尺寸（支持虚拟终端、标准终端与终端尺寸检测兜底）
    (define (detect-terminal-dimensions term)
      (cond
       ((virtual-terminal? term)
        (let ((sz (terminal-size term)))
          (values (car sz) (cadr sz))
        ) ;let
       ) ;
       ((terminal? term)
        (let ((detected (terminal-detect-size)))
          (values (car detected) (cadr detected))
        ) ;let
       ) ;
       (else (type-error "detect-terminal-dimensions: expected terminal"))
      ) ;cond
    ) ;define

    ;; 响应式尺寸重置与屏幕适配
    ;; 当传入的 new-cols / new-rows 与旧尺寸不一致时，触发 screen-resize! 重建
    (define (handle-screen-resize! screen old-cols old-rows new-cols new-rows)
      (unless (main-screen? screen)
        (type-error "handle-screen-resize!: expected <main-screen>")
      ) ;unless
      (unless (and (exact-integer? new-cols) (> new-cols 0))
        (type-error "handle-screen-resize!: expected positive integer new-cols")
      ) ;unless
      (unless (and (exact-integer? new-rows) (> new-rows 0))
        (type-error "handle-screen-resize!: expected positive integer new-rows")
      ) ;unless
      (if (and (= old-cols new-cols) (= old-rows new-rows))
        #f
        (begin
          (screen-resize! screen new-cols new-rows)
          #t
        ) ;begin
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
