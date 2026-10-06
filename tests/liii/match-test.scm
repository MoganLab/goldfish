;; (liii match) 模式匹配模块函数分类索引
;;
;; `(liii match)` 提供卫生宏模式匹配（Pattern Matching）功能，
;; 基于 Alex Shinn 的经典卫生模式匹配实现。
;;
;; ==== 使用说明 ====
;;   (import (liii match))
;;
;; ==== 查看函数文档 ====
;;   bin/gf doc liii/match
;;   bin/gf doc match
;;   bin/gf doc match-lambda
;;
;; ==== 函数与宏分类索引 ====
;;
;; 一、核心匹配宏
;;   match            - 表达式模式匹配，按序测试各分支模式并求值首个命中分支
;;
;; 二、过程式匹配宏
;;   match-lambda     - 创建单参数过程，对其参数进行模式匹配
;;   match-lambda*    - 创建变参过程，对其参数列表整体进行模式匹配
;;
;; 三、绑定式匹配宏
;;   match-let        - 并行解构绑定（支持具名循环 named let）
;;   match-let*       - 顺序依赖解构绑定
;;   match-letrec     - 递归解构绑定（支持相互递归）
