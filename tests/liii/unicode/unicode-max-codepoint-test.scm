(import (liii check)
  (liii unicode)
  (liii base)
) ;import


(check-set-mode! 'report-failed)


;; unicode-max-codepoint
;; Unicode 最大有效码点常量。
;;
;; 语法
;; ----
;; unicode-max-codepoint
;;
;; 返回值
;; ----
;; integer
;; 返回 Unicode 的最大有效码点值 #x10FFFF（十进制 1114111）。
;;
;; 描述
;; ----
;; 根据 Unicode 标准，有效的码点范围是 0 到 #x10FFFF。
;; 这个常量定义了该范围的上限。


;; 基本值测试
(check unicode-max-codepoint => 1114111)
(check unicode-max-codepoint => 1114111)


;; 与边界相关的测试
(check-true (<= 1114111 unicode-max-codepoint)
) ;check-true
(check-true (< unicode-max-codepoint 1114112)
) ;check-true


;; 验证码点范围
(check-true (integer? unicode-max-codepoint)
) ;check-true
(check-true (positive? unicode-max-codepoint)
) ;check-true


;; 与函数配合使用
(check (codepoint->utf8 unicode-max-codepoint)
  =>
  #u8(244 143 191 191)
) ;check
(check (codepoint->hexstr unicode-max-codepoint
       ) ;codepoint->hexstr
  =>
  "10FFFF"
) ;check


(check-report)
