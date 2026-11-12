# (liii goldtest-cache) 模块函数分类索引

`gf test` 的 tools 目录测试缓存：借鉴 Pants ProcessCacheScope.SUCCESSFUL 模型，
Action Cache Key 由解释器二进制哈希、测试文件哈希、tools 内部递归依赖哈希与
执行参数联合构成；仅缓存成功（exit-code=0）的测试结果。

使用时导入：

```scheme
(import (liii goldtest-cache))
```

## 常见用法示例

```scheme
;; 示例1：判断测试文件是否启用缓存（仅 tools/ 目录启用）
(tools-test-path? "tools/pi/tests/liii/pi/main-test.scm")  ; => #t

;; 示例2：递归解析测试文件依赖的 tools 内部源文件
(find-tools-dependencies "tests/liii/pi/code/cli/run-pi-test.scm")

;; 示例3：计算 Action Cache Key
(test-action-key "tests/liii/pi/code/cli/run-pi-test.scm" "-m r7rs -I tools/pi")

;; 示例4：缓存读写（exit-code 为 0 时才持久化）
(test-cache-record! (test-cache-path (test-action-key tf)) 0)
```

## 函数分类

### 作用域判定

- `tools-test-path?` - 判断测试路径是否属于 tools/ 目录

### 依赖解析与指纹

- `extract-file-imports` - 从源文件提取 (import ...) 库名列表
- `resolve-tool-import-path` - 将库名解析为 tools 内部源文件路径
- `find-tools-dependencies` - 递归收集 tools 内部依赖并按字母序排列
- `compute-tools-deps-hash` - 对依赖文件内容计算串联指纹
- `compute-test-action-key` - 纯函数：四因子联合计算 Cache Key
- `test-action-key` - 综合计算单测试文件的 Action Key

### 缓存读写

- `test-cache-base-dir` - 缓存根目录（~/.cache/goldfish/test/<version>/）
- `test-cache-path` - 由 Key 计算分片缓存文件路径
- `test-cache-hit?` - 检查缓存是否命中
- `test-cache-touch!` - 创建缓存条目（空文件）
- `test-cache-record!` - 按退出码记录缓存（失败绝不缓存）
- `test-cache-clear!` - 清空测试缓存目录
