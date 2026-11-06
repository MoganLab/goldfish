# (liii goldtool cache) 模块函数分类索引

`(liii goldtool cache)` 提供 Goldfish 工具链共享的缓存设施：缓存键为文件内容的
SHA-256，缓存目录按工具名、Goldfish 版本与额外路径段隔离，缓存条目为空文件，
存在即表示对应内容已通过检查或处理。

使用时导入：

```scheme
(import (liii goldtool cache))
```

## 常见用法示例

```scheme
;; 示例1：计算缓存条目路径
(goldtool-cache-path "fmt" "src/main.cpp")
;; => "/Users/me/.cache/goldfish/fmt/18.11.52/ab/cdef..."

;; 示例2：命中判断与写入
(if (not (goldtool-cache-hit? entry))
  (goldtool-cache-touch entry)
) ;if
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldtool/cache "函数名"
```

## 函数分类索引

### 一、缓存目录

- `goldtool-cache-base-dir` - 计算指定工具的缓存根目录

### 二、缓存条目

- `goldtool-cache-path` - 按文件内容哈希计算缓存条目路径
- `goldtool-cache-hit?` - 判断缓存条目是否存在（命中）
- `goldtool-cache-touch` - 创建缓存条目文件（必要时创建父目录）
