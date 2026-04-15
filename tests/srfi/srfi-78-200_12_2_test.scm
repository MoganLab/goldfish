(import (liii check))

;; 测试基础功能
(define (test-basic-function)
  (+ 1 2)
) ;define

;; 深度调用链测试
(define (level-3)
  (check 1 => 2)
) ;define

(define (level-2)
  (level-3)
) ;define

(define (level-1)
  (level-2)
) ;define

;; 嵌套测试
(define (test-nested-failure)
  (let ((x 5))
    (check x => 10)
  ) ;let
) ;define

;; 错误类型测试
(define (test-error-propagation)
  (check (car '()) => 'nil)
) ;define

;; 文件位置测试
(display "=== 测试1: 基础调用 ==="
) ;display
(newline)
(check (+ 1 1) => 3)

(display "=== 测试2: 嵌套调用 ==="
) ;display
(newline)
(test-nested-failure)

(display "=== 测试3: 深度调用链 ==="
) ;display
(newline)
(level-1)

(display "=== 测试4: 错误传播 ==="
) ;display
(newline)
(test-error-propagation)

;; 设置不同测试模式
(check-set-mode! 'report-failed)
(check-report)

;; 验证位置信息正确显示
(display "验证stacktrace已捕获实际测试位置而非check-report位置"
) ;display
(newline)
