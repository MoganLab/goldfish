;; (liii uuid) 模块函数分类索引
;;
;; uuid 当前提供 UUID v4 生成功能。
;; 适合生成对象标识、请求 ID、会话 ID 和去中心化唯一键。


;; ==== 常见用法示例 ====
(import (liii check) (liii uuid))

(check-set-mode! 'report-failed)

(define (uuid-hex-digit? ch)
  (or (and (char<=? #\0 ch) (char<=? ch #\9))
      (and (char<=? #\a ch) (char<=? ch #\f))))

(define (uuid4? value)
  (and (string? value)
       (= (string-length value) 36)
       (char=? (string-ref value 14) #\4)
       (not (not (memv (string-ref value 19) '(#\8 #\9 #\a #\b))))
       (let loop ((i 0))
         (or (= i 36)
             (and (if (memv i '(8 13 18 23))
                      (char=? (string-ref value i) #\-)
                      (uuid-hex-digit? (string-ref value i)))
                  (loop (+ i 1)))))))

(let ((first (uuid4)) (second (uuid4)))
  (check (uuid4? first) => #t)
  (check (uuid4? second) => #t)
  (check (not (string=? first second)) => #t))


;; 示例1：生成一个新的 UUID v4
(uuid4)


;; 示例2：连续生成多个 UUID
(list (uuid4) (uuid4))


;; 示例3：把 UUID 放入关联数据中使用
(cons 'request-id (uuid4))


;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/uuid "uuid4"


;; ==== 函数分类索引 ====


;; 一、UUID 生成
;; 用于生成随机唯一标识的函数
;;   uuid4            - 生成一个新的随机 UUID v4 字符串

(check-report)
(if (check-failed?) (exit -1))
