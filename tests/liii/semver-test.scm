;; (liii semver) 模块测试文件
;;
;; semver 提供符合 Semantic Versioning 2.0.0 规范的语义化版本解析与比较。


;; ==== 常见用法示例 ====
(import (liii check) (liii semver))

;; 示例1：版本比较
;; (semver>? "2026.3.5" "2026.3.2-rc.1") => #t
;; (semver<? "2026.3.2-rc.1" "2026.3.5") => #t
;; (semver=? "v2026.3.5" "2026.3.5") => #t

;; 示例2：合法性校验
;; (semver-valid? "2026.3.5") => #t
;; (semver-valid? "invalid") => #f


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


;; ==== 单元测试 ====

(check-set-mode! 'report-failed)

;; 1. semver-clean
(check (semver-clean "2026.3.5") => "2026.3.5")
(check (semver-clean "v2026.3.5") => "2026.3.5")
(check (semver-clean "V2026.3.5") => "2026.3.5")
(check (semver-clean "  v2026.3.5  ") => "2026.3.5")
(check (semver-clean "\n\tv1.0.0-rc.1\r\n") => "1.0.0-rc.1")
(check (semver-clean "valid") => "valid")
(check (semver-clean "") => "")
(check (semver-clean #f) => "")

;; 2. semver-valid?
(check (semver-valid? "2026.3.5") => #t)
(check (semver-valid? "v2026.3.5") => #t)
(check (semver-valid? "2026.3.2-rc.1") => #t)
(check (semver-valid? "2026.3.2-rc.1+build.123") => #t)
(check (semver-valid? "1.0.0-alpha") => #t)
(check (semver-valid? "1.0.0-alpha.1") => #t)
(check (semver-valid? "1.0.0-0.3.7") => #t)
(check (semver-valid? "1.0.0-x.7.z.92") => #t)
;; 非法格式
(check (semver-valid? "") => #f)
(check (semver-valid? "abc") => #f)
(check (semver-valid? "1.0.0-") => #f)
(check (semver-valid? "1.0.0-rc.01") => #f)
(check (semver-valid? "01.2.3") => #f)

;; 3. 经典用户场景：2026.3.2-rc.1 vs 2026.3.5
(check (semver<? "2026.3.2-rc.1" "2026.3.5") => #t)
(check (semver>? "2026.3.2-rc.1" "2026.3.5") => #f)
(check (semver>? "2026.3.5" "2026.3.2-rc.1") => #t)
(check (semver<=? "2026.3.2-rc.1" "2026.3.5") => #t)
(check (semver>=? "2026.3.5" "2026.3.2-rc.1") => #t)

;; 4. SemVer 2.0.0 规范第 11 条示例链条：
;; 1.0.0-alpha < 1.0.0-alpha.1 < 1.0.0-alpha.beta < 1.0.0-beta < 1.0.0-beta.2 < 1.0.0-beta.11 < 1.0.0-rc.1 < 1.0.0
(check (semver<? "1.0.0-alpha" "1.0.0-alpha.1") => #t)
(check (semver<? "1.0.0-alpha.1" "1.0.0-alpha.beta") => #t)
(check (semver<? "1.0.0-alpha.beta" "1.0.0-beta") => #t)
(check (semver<? "1.0.0-beta" "1.0.0-beta.2") => #t)
(check (semver<? "1.0.0-beta.2" "1.0.0-beta.11") => #t)
(check (semver<? "1.0.0-beta.11" "1.0.0-rc.1") => #t)
(check (semver<? "1.0.0-rc.1" "1.0.0") => #t)

;; 核心段比较
(check (semver<? "1.0.0" "2.0.0") => #t)
(check (semver<? "2.0.0" "2.1.0") => #t)
(check (semver<? "2.1.0" "2.1.1") => #t)
(check (semver>? "2.1.1" "2.1.0") => #t)

;; 预发布纯数字标识符按数值大小比较（防止 rc.10 与 rc.2 倒挂）
(check (semver>? "2026.3.2-rc.10" "2026.3.2-rc.2") => #t)
(check (semver<? "2026.3.2-rc.2" "2026.3.2-rc.10") => #t)

;; 构建元数据在比对时不影响优先级
(check (semver=? "1.0.0+20130313144700" "1.0.0+exp.sha.5114f85") => #t)
(check (semver=? "2026.3.5+001" "2026.3.5+002") => #t)

;; 带前导 'v' 归一后相等
(check (semver=? "v2026.3.5" "2026.3.5") => #t)
(check (semver=? "V2026.3.5" "v2026.3.5") => #t)

;; 相同版本
(check (semver=? "2026.3.5" "2026.3.5") => #t)
(check (semver>=? "2026.3.5" "2026.3.5") => #t)
(check (semver<=? "2026.3.5" "2026.3.5") => #t)

(check-report)
