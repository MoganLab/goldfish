(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui box)
) ;import

(check-set-mode! 'report)

;; 1. 基本构造与谓词
(let ((c (make-container)))
  (check-true (component? c))
  (check-true (container? c))
  (check (container-children c) => '())
  (check (component-render c 80) => '())
) ;let

;; 2. 动态追加子组件 (container-add!)
(let ((c (make-container)))
  (container-add! c (make-text "line 1"))
  (check (= (length (container-children c)) 1) => #t)
  (check (component-render c 80) => '("line 1"))

  ;; 字符串自动包装为 text component
  (container-add! c "line 2")
  (check (component-render c 80) => '("line 1" "line 2"))

  (container-add! c (make-text "line 3"))
  (check (component-render c 80) => '("line 1" "line 2" "line 3"))
) ;let

;; 3. 清空容器 (container-clear!)
(let ((c (make-container)))
  (container-add! c "alpha")
  (container-add! c "beta")
  (check (component-render c 80) => '("alpha" "beta"))
  (container-clear! c)
  (check (container-children c) => '())
  (check (component-render c 80) => '())
  ;; 清空后再次添加
  (container-add! c "gamma")
  (check (component-render c 80) => '("gamma"))
) ;let

;; 4. 输入派发与失效传递
(let* ((handled? #f)
       (child (make-component
                :render (lambda (self w) '("child"))
                :handle-input (lambda (self key)
                                (if (eq? key 'enter)
                                  (begin (set! handled? #t) #t)
                                  #f
                                ) ;if
                              ) ;lambda
              ) ;make-component
       ) ;child
       (c (make-container))
      ) ;let*
  (container-add! c child)
  (check (component-handle-input c 'space) => #f)
  (check handled? => #f)
  (check (component-handle-input c 'enter) => #t)
  (check handled? => #t)
  (check (component-invalidate! c) => #t)
) ;let*

;; 5. 异常类型防御
(check-catch 'type-error (container-add! "not-a-container" "text"))
(check-catch 'type-error (container-clear! "not-a-container"))
(check-catch 'type-error (container-children "not-a-container"))

(check-report)
