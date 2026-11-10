;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License.
;;

;; 鼠标事件接入：
;; 1. 选区状态机：对 press, drag, release 进行分流，管理全屏视口选区，
;;    并在 release 时自动复制选区（copy-on-select，对齐上游 copyOnSelect）
;; 2. 卡片点击派发：把屏幕坐标的鼠标事件换算为 doc 逻辑行后派发给 doc 组件树，
;;    由各卡片自带的 mouse-region 回调完成点击切换展开（对齐上游 pi
;;    tool-execution.ts 的 createResultRegion：click + left 且已完成时翻转）。

(define-library (liii pi code modes interactive app-mouse)
  (export pi-app-dispatch-mouse! pi-app-handle-selection-mouse!
    pi-app-copy-selection! pi-app-handle-scroll-hint-mouse!
  ) ;export
  (import (scheme base)
    (liii tui)
    (liii tui clipboard)
    (liii tui mouse)
    (liii tui scroll-view)
    (liii pi code modes interactive state)
    (liii pi code modes interactive selection)
    (liii pi code modes interactive render)
  ) ;import
  (begin

    ;; 把当前选区的屏幕文本复制到剪贴板（OSC 52 + 原生剪贴板）并闪烁 Copied!。
    ;; 返回 #t 表示发生了复制，需要重绘。
    (define (pi-app-copy-selection! app)
      (and (pi-app-selection-active? app)
        (let-values (((frame _) (pi-app-frame-lines app)))
          (let ((text (selection-extract-text frame (pi-app-selection app))))
            (and (string? text)
              (> (string-length text) 0)
              (begin
                (clipboard-copy! (pi-app-term app) text)
                (pi-app-flash! app "Copied!")
                #t
              ) ;begin
            ) ;and
          ) ;let
        ) ;let-values
      ) ;and
    ) ;define

    ;; 处理选区相关的鼠标事件流：
    ;; - press: 记录起始锚点
    ;; - drag: 产生位移时激活选区并更新焦点坐标
    ;; - release: 结束拖拽并固化选区
    ;; - click: 若存在旧选区则清除
    ;; 返回 #t 表示选区状态改变，需重绘
    (define (pi-app-handle-selection-mouse! app ev)
      (case (mouse-event-type ev)
            ((press)
             (if (eq? (mouse-event-button ev) 'left)
               (let* ((x (mouse-event-x ev))
                      (y (mouse-event-y ev))
                      (was-active? (pi-app-selection-active? app))
                     ) ;
                 (pi-app-selection-set! app (make-selection-range (cons x y) (cons x y) #f))
                 was-active?
               ) ;let*
               #f
             ) ;if
            ) ;
            ((drag)
             (let ((sel (pi-app-selection app)))
               (if
                 (and sel
                   (or (eq? (mouse-event-button ev) 'left) (eq? (mouse-event-button ev) 'none))
                 ) ;and
                 (let* ((x (mouse-event-x ev))
                        (y (mouse-event-y ev))
                        (a (selection-anchor sel))
                        (moved?
                          (or (not (= x (car a))) (not (= y (cdr a))))
                        ) ;moved?
                       ) ;
                   (if moved?
                     (begin
                       (pi-app-selection-set! app (make-selection-range a (cons x y) #t))
                       #t
                     ) ;begin
                     #f
                   ) ;if
                 ) ;let*
                 #f
               ) ;if
             ) ;let
            ) ;
            ((release)
             (let ((sel (pi-app-selection app)))
               (if (and sel (selection-active? sel))
                 ;; copy-on-select：松开鼠标即自动复制选区（对齐上游
                 ;; tui-alt-screen.ts 的 copyOnSelect，默认开启）。
                 ;; macOS 终端拦截 Command 键，Command+C 不会作为任何字节
                 ;; 序列上报给程序，因此释放即复制才是主复制路径。
                 (begin
                   (pi-app-copy-selection! app)
                   #t
                 ) ;begin
                 #f
               ) ;if
             ) ;let
            ) ;
            ((click)
             (if (pi-app-selection-active? app) (begin (pi-app-selection-clear! app) #t) #f)
            ) ;
            (else #f)
      ) ;case
    ) ;define

    ;; 派发鼠标事件到 doc 组件树：命中返回 #t，未命中返回 #f
    (define (pi-app-dispatch-mouse! app ev)
      (let* ((sv (pi-app-scroll-view app))
             (h (scroll-view-height sv))
             (y (mouse-event-y ev))
            ) ;
        (if (or (<= h 0) (>= y h))
          #f
          (component-handle-mouse! (%pi-app-doc app)
            (make-mouse-event (mouse-event-type ev)
              (mouse-event-button ev)
              (mouse-event-x ev)
              (+ y (scroll-view-offset sv))
              (mouse-event-shift? ev)
              (mouse-event-ctrl? ev)
              (mouse-event-alt? ev)
              (mouse-event-time ev)
            ) ;make-mouse-event
          ) ;component-handle-mouse!
        ) ;if
      ) ;let*
    ) ;define

    ;; 检测鼠标事件是否命中“跳到最新消息”提示行热区。
    ;; 若左键按下（press + left）且坐标落在热区内，则触发跳转到底部、
    ;; 清空已有选区并返回 #t；未命中或非左键按下返回 #f。
    (define (pi-app-handle-scroll-hint-mouse! app ev)
      (let ((rect (pi-app-scroll-hint-rect app)))
        (if (and rect
              (eq? (mouse-event-type ev) 'press)
              (eq? (mouse-event-button ev) 'left)
            ) ;and
          (let ((row (car rect))
                (col (cadr rect))
                (width (caddr rect))
                (x (mouse-event-x ev))
                (y (mouse-event-y ev))
               ) ;
            (if (and (= y row) (>= x col) (< x (+ col width)))
              (begin
                (pi-app-scroll-bottom! app)
                (pi-app-selection-clear! app)
                #t
              ) ;begin
              #f
            ) ;if
          ) ;let
          #f
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
