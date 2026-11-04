# (liii pi code config settings) 模块函数索引

`(liii pi code config settings)` 提供全局 `settings.json` 与压缩设置的读写、以及 JSON 字典安全读取等工具过程。

使用时导入：

```scheme
(import (liii pi code config settings))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc json-dict-ref
```

## 函数分类索引

### JSON 与字典读取
- `json-dict-ref` - 从 JSON 字典对象中按符号或字符串键安全读取字段值
- `pi-read-json-file` - 安全读取并解析 JSON 文件

### 设置加载与保存
- `pi-load-settings` - 从 settings.json 文件加载全局设置
- `pi-save-default-thinking-level!` - 持久化默认思考等级到 settings.json
- `pi-save-default-model!` - 持久化默认模型到 settings.json
- `pi-save-hide-thinking-block!` - 持久化是否隐藏思考块到 settings.json
- `pi-load-compaction-settings` - 从 settings.json 加载会话压缩设置

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/code/config/settings/
```
