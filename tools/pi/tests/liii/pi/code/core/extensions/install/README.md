# (liii pi code core extensions install) 模块函数分类索引

`(liii pi code core extensions install)` 提供从 Git 仓库安装扩展的能力，包含扩展名解析、目标目录解析与克隆安装。

使用时导入：

```scheme
(import (liii pi code core extensions install))
```

## 查看函数文档

```bash
gf doc pi-parse-git-extension-name
gf doc pi-valid-extension-name?
gf doc pi-resolve-install-target
gf doc pi-git-clone-extension
gf doc pi-install-extension
```

## 函数分类索引

### 扩展安装
- `pi-parse-git-extension-name` - 从 Git 仓库地址解析扩展名
- `pi-valid-extension-name?` - 判断扩展名是否符合命名规范
- `pi-resolve-install-target` - 解析扩展安装的目标目录路径
- `pi-git-clone-extension` - 将 Git 仓库克隆到扩展目录
- `pi-install-extension` - 从 Git 仓库地址安装扩展到扩展目录
