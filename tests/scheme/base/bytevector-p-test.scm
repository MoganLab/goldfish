(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; bytevector?
;; 判断一个对象是否为字节向量类型的谓词。
;;
;; 语法
;; ----
;; (bytevector? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 任意对象。
;;
;; 返回值
;; ------
;; boolean?
;; 如果对象是一个字节向量，返回#t；否则返回#f。
;;
;; 说明
;; ----
;; 1. 用于检查对象是否为字节向量类型
;; 2. #u8()形式创建的也是字节向量，即使为空
;; 3. 能够正确识别所有类型的字节向量实例
;;
;; 错误处理
;; --------
;; wrong-number-of-args
;; 当参数数量不为1时抛出错误。

;; bytevector? 基本测试
(check-true (bytevector? #u8()))
(check-true (bytevector? #u8(0)))
(check-true (bytevector? #u8(255)))
(check-true (bytevector? #u8(1 2 3 4 5)))
(check-true (bytevector? (bytevector)))
(check-true (bytevector? (bytevector 1 2 3)))

;; 类型判别测试
(check-true (bytevector? (bytevector 5 15 25)))
(check-false (bytevector? 123))
(check-false (bytevector? "hello"))
(check-false (bytevector? "list"))
(check-false (bytevector? 'symbol))

;; 错误处理测试
(check-catch 'wrong-number-of-args (bytevector?))
(check-catch 'wrong-number-of-args (bytevector? #u8(1 2 3) #u8(4 5 6)))

(check-report)
