(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

;; bag-search! 函数测试
;;
;; 语法
;; ----
;; (bag-search! bag element failure success)
;;
;; 参数
;; ----
;; bag : bag
;; 目标 bag。
;;
;; element : any
;; 要查找的元素（按 comparator 等价判断）。
;;
;; failure : procedure
;; 未命中时调用，签名：(lambda (insert ignore) ...)
;;
;; success : procedure
;; 命中时调用，签名：(lambda (element update remove) ...)
;;
;; 返回值
;; -----
;; 返回 (values bag obj)，具体 obj 由 failure/success 回调决定。

(let ((yam (bag #\y #\a #\m)))
  (define (failure/insert insert ignore)
    (insert 1))
  (define (failure/ignore insert ignore)
    (ignore 2))
  (define (success/update element update remove)
    (update #\b 3))
  (define (success/remove element update remove)
    (remove 4))

  (call-with-values
    (lambda () (bag-search! (bag-copy yam) #\! failure/insert error))
    (lambda (b obj)
      (check-true (bag-contains? b #\!))
      (check obj => 1)))
  (call-with-values
    (lambda () (bag-search! (bag-copy yam) #\! failure/ignore error))
    (lambda (b obj)
      (check-false (bag-contains? b #\!))
      (check obj => 2)))
  (call-with-values
    (lambda () (bag-search! (bag-copy yam) #\y error success/update))
    (lambda (b obj)
      (check-true (bag-contains? b #\b))
      (check-false (bag-contains? b #\y))
      (check obj => 3)))
  (call-with-values
    (lambda () (bag-search! (bag-copy yam) #\a error success/remove))
    (lambda (b obj)
      (check-false (bag-contains? b #\a))
      (check obj => 4))))

(check-report)
