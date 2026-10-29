(import (scheme base)
        (liii check)
        (liii tui mouse)
        (liii tui component)
        (liii tui box))

(check-set-mode! 'report-failed)

;; component-handle-mouse! 基础契约
;; 未提供 :handle-mouse 的组件返回 #f（未处理）
(let ((c (make-component :render (lambda (self w) '("x")))))
  (check (component-handle-mouse! c (make-mouse-event 'click 'left 0 0)) => #f)
) ;let

;; 提供 :handle-mouse 的组件收到事件本身
(let ((got '()))
  (let ((c (make-component
             :render (lambda (self w) '("x"))
             :handle-mouse (lambda (self ev)
                             (set! got (cons ev got))
                             #t)
           )) ;make-component
       ) ;let
    (check (component-handle-mouse! c (make-mouse-event 'click 'left 3 4)) => #t)
    (check (length got) => 1)
    (check (mouse-event-type (car got)) => 'click)
    (check (mouse-event-x (car got)) => 3)
    (check (mouse-event-y (car got)) => 4)
  ) ;let
) ;let

;; 类型检查：非组件报 type-error
(check-catch 'type-error (component-handle-mouse! "not-comp" (make-mouse-event 'click 'left 0 0)))
;; 类型检查：非 mouse-event 报 type-error
(let ((c (make-component :render (lambda (self w) '("x")))))
  (check-catch 'type-error (component-handle-mouse! c 'click))
) ;let

;; mouse-region 包装器
;; 1. 渲染与被包装组件完全一致
(let* ((hits '())
       (inner (make-component :render (lambda (self w) '("aaa" "bbb"))))
       (mr (make-mouse-region inner
             (lambda (ev) (set! hits (cons (mouse-event-y ev) hits)) #t)))
      ) ;let*
  (check-true (mouse-region? mr))
  (check-true (component? mr))
  (check (component-render mr 10) => '("aaa" "bbb"))
  (check (mouse-region-child mr) => inner)
) ;let*

;; 2. 内层未处理时回落到包装器回调
(let* ((hits '())
       (inner (make-component :render (lambda (self w) '("aaa"))))
       (mr (make-mouse-region inner (lambda (ev) (set! hits (cons 1 hits)) #t)))
      ) ;let*
  (check (component-handle-mouse! mr (make-mouse-event 'click 'left 0 0)) => #t)
  (check hits => '(1))
) ;let*

;; 3. 内层处理了则包装器回调不被调用
(let* ((hits '())
       (inner (make-component
                :render (lambda (self w) '("aaa"))
                :handle-mouse (lambda (self ev) #t)))
       (mr (make-mouse-region inner (lambda (ev) (set! hits (cons 1 hits)) #t)))
      ) ;let*
  (check (component-handle-mouse! mr (make-mouse-event 'click 'left 0 0)) => #t)
  (check hits => '())
) ;let*

;; 4. 内层未处理且回调返回 #f 时整体未处理
(let* ((inner (make-component :render (lambda (self w) '("aaa"))))
       (mr (make-mouse-region inner (lambda (ev) #f)))
      ) ;let*
  (check (component-handle-mouse! mr (make-mouse-event 'click 'left 0 0)) => #f)
) ;let*

;; 5. invalidate 传递给内层
(let* ((invoked 0)
       (inner (make-component
                :render (lambda (self w) '("aaa"))
                :invalidate (lambda (self) (set! invoked (+ invoked 1)) #t)))
       (mr (make-mouse-region inner (lambda (ev) #f)))
      ) ;let*
  (component-invalidate! mr)
  (check invoked => 1)
) ;let*

;; 6. make-mouse-region 类型检查
(check-catch 'type-error (make-mouse-region "not-comp" (lambda (ev) #f)))
(check-catch 'type-error
  (make-mouse-region (make-component :render (lambda (self w) '())) "not-proc"))

(check-report)
