(import (liii check) (liii string))

;; string-contains?
;; 检查字符串是否包含指定子串。
;;
;; 语法
;; ----
;; (string-contains? str sub-str)
;;
;; 参数
;; ----
;; str : string?
;; 要检查的源字符串。
;;
;; sub-str : string?
;; 要查找的子串。
;;
;; 返回值
;; ----
;; boolean
;; 如果str包含sub-str返回#t，否则返回#f。
;;
;; 注意
;; ----
;; `string-contains?` 是 `string-contains` 的谓词风格别名，更符合布尔判定 API 的命名直觉。
;; 空字符串作为sub-str时总是返回#t。
;;
;; 错误处理
;; ----
;; type-error 当参数不是字符串类型时。

(check-true (string-contains? "0123456789" "3")
) ;check-true
(check-true (string-contains? "0123456789" "34")
) ;check-true
(check-false (string-contains? "0123456789" "24")
) ;check-false
(check-true (string-contains? "" ""))
(check-true (string-contains? "hello" "")
) ;check-true
(check-false (string-contains? "" "a"))
(check-true (string-contains? "中文测试"
              "文测"
            ) ;string-contains?
) ;check-true
(check-false (string-contains? "Hello" "hello")
) ;check-false
(check-catch 'type-error
  (string-contains? 123 "1")
) ;check-catch
(check-catch 'type-error
  (string-contains? "123" 1)
) ;check-catch

(check-report)
