;; (liii semver) 模块函数分类索引
;;
;; semver 提供符合 Semantic Versioning 2.0.0 规范的语义化版本解析与比较。
;; 适合包管理器、工具链和自动更新器判断版本新旧与合法性。


;; ==== 常见用法示例 ====
(import (liii check) (liii semver))


;; 示例1：版本比较
(semver>? "2026.3.5" "2026.3.2-rc.1")
(semver<? "2026.3.2-rc.1" "2026.3.5")
(semver=? "v2026.3.5" "2026.3.5")


;; 示例2：合法性校验
(semver-valid? "2026.3.5")
(semver-valid? "invalid")


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
;; 构建元数据同样受规范约束：点分标识符，仅限 ASCII 字母数字与连字符，且不得为空
(check (semver-valid? "1.0.0+build.1") => #t)
(check (semver-valid? "1.0.0+20130313144700") => #t)
(check (semver-valid? "1.0.0+exp.sha.5114f85") => #t)
(check (semver-valid? "1.0.0+!!!") => #f)
(check (semver-valid? "1.0.0+build..1") => #f)
;; 核心段允许 1~3 段（缺省段补零），但不得超过三段
(check (semver-valid? "0") => #t)
(check (semver-valid? "1") => #t)
(check (semver-valid? "1.0") => #t)
(check (semver-valid? "1.2.3.4") => #f)

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

;; 5. 非法版本：semver-compare 与各比较谓词一律返回 #f
(check (semver-parse "invalid") => #f)
(check (semver-parse "1.0.0-") => #f)
(check (semver-parse "01.2.3") => #f)
(check (semver-compare "1.0.0" "invalid") => #f)
(check (semver-compare "invalid" "1.0.0") => #f)
(check (semver-compare "invalid" "also-invalid") => #f)
(check (semver>? "1.0.0" "invalid") => #f)
(check (semver<? "1.0.0" "invalid") => #f)
(check (semver=? "1.0.0" "invalid") => #f)
(check (semver>=? "1.0.0" "invalid") => #f)
(check (semver<=? "1.0.0" "invalid") => #f)

;; 6. 移植自 m2nlight/SemVer（SemVer.Tests/SemanticVersionTest.cs）的用例
;;    原仓库为 C#/xUnit；此处只移植与 (liii semver) 公开 API 对应的部分，
;;    略去构造函数校验、IsStable、ToString 格式化等本库未提供的接口。
;;    原仓库的大小写不敏感相等（"1.0-rc" = "1.0.0-RC"）未移植：
;;    规范第 11.4 条要求 ASCII 字典序，故 "RC" < "rc"，二者不相等。

;; 6.1 合法：1~3 段核心 + 预发布 + 构建元数据
(check (semver-valid? "1") => #t)
(check (semver-valid? "1.0") => #t)
(check (semver-valid? "1.2.3-alpha.1-1.0+exp.sha.5114f85-001") => #t)
(check (semver-valid? "1.0.0-alpha+exp.sha.5114f85") => #t)
(check (semver-valid? "1.0.0-alpha-0.0.0+exp.sha.5114f85") => #t)
(check (semver-valid? "1.0.0-alpha-0.0.0+exp.sha.5114f85-001") => #t)
(check (semver-valid? "1.0.0+001") => #t)
(check (semver-valid? "1.0.0-x.7.z.92") => #t)

;; 6.2 非法字符串
(check (semver-valid? "-1") => #f)
(check (semver-valid? "1.") => #f)
(check (semver-valid? "1.-0") => #f)
(check (semver-valid? "1.0.0-") => #f)
(check (semver-valid? "1.0.0-alpha.01") => #f)
(check (semver-valid? "1.0.0-alpha..1") => #f)
(check (semver-valid? "1.0.0-alpha%") => #f)
(check (semver-valid? "1.0.0-alpha+") => #f)
(check (semver-valid? "1.0.0+") => #f)
(check (semver-valid? "1.0.0+exp%") => #f)
(check (semver-valid? "1.0.0+exp.sha.5114f85+") => #f)
(check (semver-valid? "1.0.0.0") => #f)

;; 6.3 数值段按数值比较，而非字典序
(check (semver<? "2.0.0" "10.0.0") => #t)
(check (semver<? "2.2.0" "2.10.0") => #t)
(check (semver<? "2.2.2" "2.2.10") => #t)
(check (semver<? "1.0.0" "1.0.1") => #t)

;; 6.4 预发布段：数值 vs 数值、数值 vs 非数值
(check (semver<? "1.0.0-rc.1.0" "1.0.0-rc.1.1") => #t)
(check (semver<? "1.0.0-alpha.2" "1.0.0-alpha.10") => #t)
(check (semver<? "1.0.0-alpha.2" "1.0.0-alpha.beta") => #t)
(check (semver>? "1.0.0-rc.1.a" "1.0.0-rc.1.1") => #t)
(check (semver>=? "1.0.0-rc.1.a" "1.0.0-rc.1.1") => #t)
(check (semver<=? "1.0.0-rc.1.a" "1.0.0-rc.1.1") => #f)

;; 6.5 相等：核心段缺省补零，构建元数据不参与比较
(check (semver=? "1.0" "1.0.0") => #t)
(check (semver=? "1.0-rc" "1.0.0-rc") => #t)
(check (semver=? "1.0+exp.sha.5114f85" "1.0.0+001") => #t)
(check (semver-compare "1.0" "1.0.0") => 0)
(check (semver-compare "1" "1.0.0") => 0)

(check-report)
