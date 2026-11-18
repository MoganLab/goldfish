;; all-in-one-default-config-name
;; 默认 all-in-one 配置文件的展示名称。
;;
;; 语法
;; ----
;; all-in-one-default-config-name
;;
;; 参数
;; ----
;; 无（常量）。
;;
;; 返回值
;; ----
;; string
;; 首次生成 all-in-one.json 时写入根节点 name 字段的展示名。
;;
;; 说明
;; ----
;; 该名称只用于“首次生成默认配置文件”这一场景：已存在的用户配置
;; 不会被改写，因此用户自定义 name 始终优先。名称与默认记录同属
;; 默认配置的单一真值源，改文案只需改这一处。

(import (scheme base) (liii check) (liii pi code all-in-one defaults))

(check-set-mode! 'report-failed)

(check-true (string? all-in-one-default-config-name))
(check-true (> (string-length all-in-one-default-config-name) 0))
(check all-in-one-default-config-name => "高可用长程推进总线")

(check-report)
