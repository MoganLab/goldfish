(import (liii check) (liii string))

;; pyfmt
;; Python % 风格的具名格式化函数
;;
;; 语法
;; ----
;; (pyfmt format-string)
;; (pyfmt format-string . plist)
;;
;; 参数
;; ----
;; format-string : string
;; 格式字符串，使用 %(key)s 风格的占位符
;;
;; plist : 键值对列表
;; 格式化参数列表。键可以是关键字（如 :name）或符号（如 'name）
;;
;; 返回值
;; ----
;; string
;; 格式化后的字符串

;; 基础字符串替换
(check (pyfmt "hello") => "hello")
(check (pyfmt "User %(name)s" :name "Bob") => "User Bob")
(check (pyfmt "User %(name)s" 'name "Bob") => "User Bob")

;; 整数替换
(check (pyfmt "age=%(age)d" :age 30) => "age=30")
(check (pyfmt "age=%(age)d" 'age 30) => "age=30")

;; 多个字段混合
(check (pyfmt "%(greeting)s %(name)s" :greeting "Hello" :name "World")
  =>
  "Hello World"
) ;check
(check (pyfmt "%(greeting)s %(name)s" 'greeting "Hello" 'name "World")
  =>
  "Hello World"
) ;check

;; 关键字和符号混合使用
(check (pyfmt "%(a)s %(b)s" :a "1" 'b "2") => "1 2")

;; 空 plist，原样返回
(check (pyfmt "no placeholders") => "no placeholders")

;; 重复的占位符
(check (pyfmt "%(x)s %(x)s" :x "repeat") => "repeat repeat")

;; 占位符之间没有空格
(check (pyfmt "%(a)s%(b)s" :a "1" :b "2") => "12")

;; 中文字段值
(check (pyfmt "%(name)s" :name "中文") => "中文")

;; 字段值包含特殊字符
(check (pyfmt "%(path)s" :path "/var/log/app.log") => "/var/log/app.log")

;; 字符串 key
(check (pyfmt "%(name)s" "name" "Bob") => "Bob")

;; #f 是合法字段值，不能被当作缺失字段
(check (pyfmt "%(ok)s" :ok #f) => "#f")

;; 缺失字段时保留完整占位符
(check (pyfmt "%(name)s") => "%(name)s")
(check (pyfmt "%(age)d") => "%(age)d")
(check (pyfmt "hello %(name)s!" :other "Bob") => "hello %(name)s!")

;; 参数错误
(check-catch 'type-error (pyfmt 123))
(check-catch 'type-error (pyfmt "%(name)s" :name))
(check-catch 'type-error (pyfmt "%(name)s" 123 "Bob"))
(check-catch 'type-error (pyfmt "%(age)d" :age "30"))

(check-report)
