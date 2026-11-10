# (liii goldfix cache) 模块函数索引

`(liii goldfix cache)` 提供 gf fix 的缓存机制：按文件内容的 SHA-256 摘要计算缓存条目路径，并判断/写入缓存条目。

使用时导入：

```scheme
(import (liii goldfix cache))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldfix/cache "函数名"
```

## 函数分类索引

### 缓存路径
- `fix-cache-base-dir` - 返回缓存基目录路径（用户主目录下的 .cache/goldfish/fix/<版本号>）
- `fix-cache-path` - 根据文件内容的 SHA-256 摘要计算缓存条目路径

### 缓存读写
- `fix-cache-hit?` - 判断指定文件是否已有缓存条目
- `fix-cache-touch` - 写入或刷新指定文件的缓存条目
