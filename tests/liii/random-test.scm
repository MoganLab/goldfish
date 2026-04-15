;; (liii random) 模块函数分类索引
;;
;; (liii random) 是基于 SRFI-27 的随机数生成模块，
;; 提供随机整数、随机实数、随机源管理等功能。


;; ==== 常见用法示例 ====
(import (liii random))


;; 示例1：生成随机整数
(random-integer 100)


;; 示例2：生成随机实数
(random-real)


;; 示例3：创建独立随机源
(let ((s (make-random-source)))
  (random-source-pseudo-randomize! s 1 2)
  ((random-source-make-integers s) 100)
) ;let


;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/random "random-integer"
;;   bin/gf doc liii/random "make-random-source"


;; ==== 函数分类索引 ====


;; 一、基本随机数生成
;; 使用默认随机源生成随机数
;;   random-integer      - 生成 [0, n-1] 范围内的随机整数
;;   random-real         - 生成 (0, 1) 范围内的随机实数


;; 二、随机源管理
;; 创建和管理独立的随机源
;;   make-random-source              - 创建新的随机源
;;   random-source?                  - 检查是否为随机源
;;   default-random-source           - 默认随机源


;; 三、随机源状态操作
;; 保存、恢复和修改随机源状态
;;   random-source-state-ref         - 获取随机源状态
;;   random-source-state-set!        - 设置随机源状态
;;   random-source-randomize!        - 用当前时间随机化
;;   random-source-pseudo-randomize! - 用索引伪随机化


;; 四、生成器创建
;; 从随机源创建特定的随机数生成器
;;   random-source-make-integers     - 创建整数生成器
;;   random-source-make-reals        - 创建实数生成器
