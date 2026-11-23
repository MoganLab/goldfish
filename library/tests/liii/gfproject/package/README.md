# (liii gfproject package) 模块函数分类索引

`(liii gfproject package)` 负责解析 `gfproject.json` 中可选的 `package` 元数据。

## 配置格式

```json
"package": {
  "name": "lang",
  "organization": "liii",
  "description": "Goldfish Scheme programming language and standard libraries"
}
```

- `name` 与 `organization` 必填，组成显示标识 `lang@liii`。
- `description` 可选，默认为空字符串。
- `name` 与 `organization` 使用 `^[a-z][a-z0-9_-]*$`，不得包含 `@`、斜杠或空白。
- `package` 可整体省略，旧项目无需修改。
- 完整约束见 `library/resources/gfproject.schema.json`。

## 函数

- `gfproject-package` - 构造包元数据记录
- `gfproject-package?` - 判断是否为包元数据记录
- `gfproject-package-name` - 返回包名
- `gfproject-package-organization` - 返回发布组织
- `gfproject-package-description` - 返回包描述
- `gfproject-package-id` - 返回 `name@organization`
- `json->gfproject-package` - 从配置 JSON 解析可选包记录

## 查看函数文档

```bash
gf doc liii/gfproject/package
```

## 运行测试

```bash
bin/gf test library/tests/liii/gfproject/package/
```
