;; test-result-ref
;; 读取当前测试结果属性表中的指定属性值。
;;
;; 语法
;; ----
;; (test-result-ref runner pname)
;; (test-result-ref runner pname default)
;;
;; 参数
;; ----
;; runner : test-runner?
;; 测试运行器对象。
;; pname : symbol?
;; 属性名。
;; default : any (可选，默认 #f)
;; 属性不存在时的默认值。
;;
;; 返回值
;; ----
;; any

(import (scheme base) (srfi srfi-64))

(define (%run-tests thunk)
  (let ((r (test-runner-null)))
    (test-runner-on-final! r
      (lambda (runner)
        (when (or (> (test-runner-fail-count runner) 0)
                (> (test-runner-xpass-count runner) 0)
              ) ;or
          (error "SRFI-64 test failed")
        ) ;when
      ) ;lambda
    ) ;test-runner-on-final!
    (test-with-runner r (thunk))
  ) ;let
) ;define

(%run-tests
  (lambda ()
    (test-begin "ref")
    (test-equal 3 (+ 1 2))
    (let ((kind (test-result-ref (test-runner-current) 'result-kind))
          (miss (test-result-ref (test-runner-current) 'missing 'none))
         ) ;
      (test-equal 'pass kind)
      (test-equal 'none miss)
    ) ;let
    (test-end "ref")
  ) ;lambda
) ;%run-tests
