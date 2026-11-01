# (liii goldcheck config) 模块函数分类索引

`(liii goldcheck config)` 提供了 `gf check` 工具的配置文件读取、解析与规则参数查询功能。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/goldcheck/config "函数名"
```

## 函数分类索引

### 一、配置加载与校验
- `load-check-config` - 读取并解析 gf_check.json 配置文件，并严格校验字段类型与取值范围

### 二、规则参数查询
- `check-rule-enabled?` - 查询指定规则是否在配置中处于启用状态
- `check-rule-limit` - 查询指定规则在配置中的数值阈值上限
- `check-rule-excludes` - 从检查配置中解析指定规则的 exclude 路径模式列表
