# (liii pi ai template) 模块函数索引

`(liii pi ai template)` 提供配置项值中的环境变量查找与模板展开功能，支持 `$VAR`、`${VAR}` 语法以及 `$$` 转义。

使用时导入：

```scheme
(import (liii pi ai template))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-resolve-config-value
```

## 函数分类索引

### 模板插值与环境查找
- `pi-resolve-config-value` - 解析并插值字符串中的环境变量模板
- `lookup-env-var` - 查找自定义环境或系统环境变量的值
