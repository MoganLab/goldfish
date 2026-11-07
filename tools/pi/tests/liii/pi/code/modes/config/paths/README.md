# (liii pi code modes config paths) 模块函数分类索引

`(liii pi code modes config paths)` 提供配置文件的相对路径构造与原文读取能力。

使用时导入：

```scheme
(import (liii pi code modes config paths))
```

## 查看函数文档

```bash
gf doc pi-config-rel-provider
gf doc pi-config-rel-model
gf doc pi-config-abs
gf doc pi-config-read-text
```

## 函数分类索引

### 路径与读取
- `pi-config-rel-provider` - 生成 provider 配置文件相对配置目录的路径
- `pi-config-rel-model` - 生成模型配置文件相对配置目录的路径
- `pi-config-abs` - 将相对配置目录的路径拼接为绝对路径
- `pi-config-read-text` - 读取配置目录中指定相对路径文件的原文
