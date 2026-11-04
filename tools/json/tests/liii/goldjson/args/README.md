# (liii goldjson args) 模块导览

`(liii goldjson args)` 提供 `gf json` 命令行参数解析与帮助文本输出，
与具体 JSON 转换逻辑解耦。

使用时导入（load-path 需包含 `tools/json`）：

```scheme
(import (liii goldjson args))
```

## 函数分类索引

### 一、参数解析

| 函数 | 说明 |
|---|---|
| `parse-json-args` | 解析命令行参数为选项关联列表，非法参数返回 #f |

### 二、帮助文本

| 函数 | 说明 |
|---|---|
| `display-json-help` | 向指定端口输出帮助文本 |
