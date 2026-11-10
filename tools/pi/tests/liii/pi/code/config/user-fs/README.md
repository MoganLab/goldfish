# (liii pi code config user-fs) 模块函数分类索引

`(liii pi code config user-fs)` 提供用户配置目录的文件读取与字典访问工具。

使用时导入：

```scheme
(import (liii pi code config user-fs))
```

## 查看函数文档

```bash
gf doc pi-user-config-path
gf doc pi-user-read-json
gf doc pi-user-dict-ref
```

## 函数分类索引

### 文件读取
- `pi-user-config-path` - 拼接配置目录下的文件路径
- `pi-user-read-json` - 读取 JSON 配置文件并解析为数据
- `pi-user-vector->list` - 将 JSON 解出的向量转换为列表

### 字典访问
- `pi-user-dict-ref` - 从用户配置字典中读取指定键的取值

### 警告输出
- `pi-user-warn` - 向标准输出打印警告文本
