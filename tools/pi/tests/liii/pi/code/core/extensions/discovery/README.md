# (liii pi code core extensions discovery) 模块函数分类索引

`(liii pi code core extensions discovery)` 提供扩展目录扫描与多层级扩展发现能力。

使用时导入：

```scheme
(import (liii pi code core extensions discovery))
```

## 查看函数文档

```bash
gf doc pi-scan-extension-dir
gf doc pi-discover-extensions
gf doc pi-default-builtin-extensions-dir
```

## 函数分类索引

### 扩展发现
- `pi-scan-extension-dir` - 扫描扩展目录，返回其中的扩展入口脚本路径
- `pi-discover-extensions` - 按内置、用户级、项目级与显式路径合并发现扩展
- `pi-default-builtin-extensions-dir` - 获取内置扩展目录的默认位置
