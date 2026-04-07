(import (liii check)
        (liii ascii)
) ;import

;; ascii-bytevector?
;; 判断字节向量是否全部由 ASCII 字节组成。
;;
;; 语法
;; ----
;; (ascii-bytevector? x)
;;
;; 参数
;; ----
;; x : any?
;; 要判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 如果x是只包含 ASCII 字节的字节向量则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 空字节向量也视为 ASCII 字节向量。
;;
;; 示例
;; ----
;; (ascii-bytevector? #u8(0 65 127)) => #t
;; (ascii-bytevector? #u8(0 128)) => #f
;;
;; 错误处理
;; ----
;; 非字节向量输入返回 #f

(check-true (ascii-bytevector? #u8()))
(check-true (ascii-bytevector? #u8(0 65 127)))
(check-false (ascii-bytevector? #u8(0 128)))
(check-false (ascii-bytevector? '(65 66)))

(check-report)
