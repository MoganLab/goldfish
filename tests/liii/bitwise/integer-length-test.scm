(import (liii check)
        (liii bitwise)
) ;import

(check-set-mode! 'report-failed)

;; integer-length
;; 计算整数二进制表示的最小位数。
;;
;; 语法
;; ----
;; (integer-length i)
;;
;; 参数
;; ----
;; i : integer?
;; 整数，要计算最小位数的整数。
;;
;; 返回值
;; -----
;; integer?
;; 返回整数 i 二进制表示的最小位数。
;;
;; 说明
;; ----
;; 1. 计算整数二进制表示所需的最小位数
;; 2. 对于非负整数，返回值为1的最高位的位置加1
;; 3. 对于负整数，返回值为0的最高位的位置加1
;; 4. 对于0，返回0
;; 5. 常用于确定存储整数所需的最小位数
;;
;; 实现说明
;; --------
;; - integer-length 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 计算整数二进制表示所需的最小位数
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。

(check (integer-length 0) => 0)
(check (integer-length 1) => 1)     ; 1
(check (integer-length 3) => 2)     ; 11
(check (integer-length 4) => 3)     ; 100
(check (integer-length -5) => 3)    ; -101 (长度为3)
(check (integer-length #xFFFF) => 16) ; 16位二进制

;;; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg
             (integer-length "string")  ; 字符串参数
) ;check-catch
(check-catch 'wrong-type-arg
             (integer-length 'symbol)   ; 符号参数
) ;check-catch
(check-catch 'wrong-type-arg
             (integer-length 3.14)      ; 浮点数参数
) ;check-catch
(check-catch 'wrong-type-arg
             (integer-length #\a)       ; 字符参数
) ;check-catch
(check-catch 'wrong-type-arg
             (integer-length '(1 2))    ; 列表参数
) ;check-catch


(check-report)
