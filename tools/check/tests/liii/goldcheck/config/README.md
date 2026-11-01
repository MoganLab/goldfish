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

### 一、配置存在性与版本
- `check-config-exists?` - 检查指定目录或工程根目录下是否存在 gf_check.json 配置文件
- `check-config-goldfish-version` - 获取检查配置中声明的 goldfish_version 字符串

### 二、配置加载与校验
- `load-check-config` - 读取并解析 gf_check.json 配置文件，并严格校验字段类型与取值范围
- `validate-check-config!` - 严格校验检查配置对象的格式、类型与取值范围

### 三、规则配置与参数查询
- `check-config-rules` - 获取检查配置中声明的 rules 对象
- `check-rule-enabled?` - 查询指定规则是否在配置中处于启用状态
- `check-rule-limit` - 查询指定规则在配置中的数值阈值上限
- `check-rule-excludes` - 从检查配置中解析指定规则的 exclude 路径模式列表
- `check-rule-suffixes` - 查询指定规则在配置中声明的 suffix 文件后缀名列表
- `check-rule-paths` - 查询指定规则在配置中声明的 path 目录白名单列表
