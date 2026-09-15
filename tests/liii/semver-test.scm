;; (liii semver) 模块函数分类索引
;;
;; semver 提供符合 Semantic Versioning 2.0.0 规范的语义化版本解析与比较。
;; 适合包管理器、工具链和自动更新器判断版本新旧与合法性。

;; ==== 常见用法示例 ====
(import (liii check) (liii semver))

(check-set-mode! 'report-failed)

;; 示例1：版本比较
(check-true (semver>? "2026.3.5" "2026.3.2-rc.1"))
(check-true (semver<? "2026.3.2-rc.1" "2026.3.5"))
(check-true (semver=? "v2026.3.5" "2026.3.5"))

;; 示例2：合法性校验
(check-true (semver-valid? "2026.3.5"))
(check-false (semver-valid? "invalid"))

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/semver "semver-clean"
;;   bin/gf doc liii/semver "semver-parse"
;;   bin/gf doc liii/semver "semver-valid?"
;;   bin/gf doc liii/semver "semver-compare"
;;   bin/gf doc liii/semver "semver>?"

;; ==== 函数分类索引 ====

;; 一、版本清洗与解析
;;   semver-clean   - 去除前后空白与前导 v/V
;;   semver-parse   - 解析为 (semver core-nums pre-list) 结构
;;   semver-valid?  - 校验是否为合法语义化版本

;; 二、版本比较与谓词
;;   semver-compare - 返回 -1、0、1，非法返回 #f
;;   semver>?       - 严格大于
;;   semver<?       - 严格小于
;;   semver=?       - 等于（忽略构建元数据）
;;   semver>=?      - 大于或等于
;;   semver<=?      - 小于或等于

(check-report)
