# (liii goldtest) 模块函数分类索引

`gf test` 子命令在 Scheme 层的实现入口：解析测试参数、筛选目标文件、
执行测试并汇总结果。

使用时导入：

```scheme
(import (liii goldtest))
```

## 常见用法示例

```scheme
;; 示例1：解析 gf test 命令行参数
(parse-test-args '("bin/gf" "test" "json"))

;; 示例2：按模式过滤测试文件
(filter-test-files '("tests/liii/json-test.scm" "tests/liii/list-test.scm")
  'pattern
  "json"
) ;filter-test-files
;; => '("tests/liii/json-test.scm")

;; 示例3：查找整个 tests 目录中的测试文件
(find-test-files "tests/liii")
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldtest "函数名"
```

## 函数分类索引

### 一、参数解析

- `parse-test-args` - 解析测试命令参数

### 二、测试发现与过滤

- `filter-test-files` - 按模式过滤测试文件列表
- `find-test-files` - 递归查找测试文件

### 三、执行与入口

- `run-goldtest` - 执行测试主流程
- `main` - 命令行入口
