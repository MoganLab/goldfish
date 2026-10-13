;; simple.scm: 单文件多层函数调用栈（Stack Trace）回溯与出错定位简单示例

(define (step-c x)
  (+ 1 (car x))
) ;define

(define (step-b x)
  (+ 1 (step-c x))
) ;define

(define (step-a x)
  (+ 1 (step-b x))
) ;define

;; 入口：故意传入 '() 引发运行时类型错误
(step-a '())
