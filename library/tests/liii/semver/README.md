# (liii semver) 模块函数分类索引

`(liii semver)` 提供符合 Semantic Versioning 2.0.0 规范的语义化版本解析与比较能力，适合包管理器、工具链和自动更新器判断版本新旧与合法性。

## 查看函数文档

```bash
gf doc liii/semver
```

或者查看具体函数的文档和用例：

```bash
gf doc liii/semver "semver-parse"
```

## 函数分类索引

### 一、版本清洗与解析
- `semver-clean` - 去除前后空白与前导 v/V
- `semver-parse` - 将语义化版本字符串解析为版本结构
- `semver-valid?` - 判断字符串是否为合法的语义化版本

### 二、版本比较
- `semver-compare` - 比较两个语义化版本的大小
- `semver=?` - 判断两个语义化版本是否相等
- `semver<?` - 判断左侧版本是否小于右侧版本
- `semver<=?` - 判断左侧版本是否小于等于右侧版本
- `semver>?` - 判断左侧版本是否大于右侧版本
- `semver>=?` - 判断左侧版本是否大于等于右侧版本
