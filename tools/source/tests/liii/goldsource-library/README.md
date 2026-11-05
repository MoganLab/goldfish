# (liii goldsource-library) 模块函数索引

`(liii goldsource-library)` 提供 `gf source` 子命令定位库源文件路径的能力。

使用时导入：

```scheme
(import (liii goldsource-library))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldsource-library "函数名"
```

## 函数分类索引

### 库路径定位
- `find-visible-library-root` - 在当前 `*load-path*` 中查找可见库对应的根目录
- `source-library-path` - 根据库查询字符串计算库源码文件的完整绝对/相对路径
