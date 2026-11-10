# (liii pi ai models list) 模块函数分类索引

`(liii pi ai models list)` 提供了 Pi Coding Agent 的模型列表查询、表格排版与搜索过滤功能。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/pi/ai/models/list "函数名"
```

## 函数分类索引

### 一、模型查询与过滤
- `pi-list-models` - 查询并排序全部或指定 provider 的可用及未激活模型行列表
- `pi-usable-models` - 已激活模型列表（与 config TUI、--list-models 的 ● 判定一致，供 /model 选择器）
- `pi-split-search-tokens` - 将搜索关键词字符串按空格拆分为 token 列表
- `pi-fuzzy-match-model?` - 判断搜索词是否与模型名称或 provider 进行子序列模糊匹配

### 二、终端表格格式化
- `pi-format-models-table` - 将模型行列表排版为对齐的终端文本表格，支持状态符号与高亮/暗显色彩渲染
- `pi-format-token-count` - 将 token 数值格式化为人类可读格式（如 128K、1.0M）
