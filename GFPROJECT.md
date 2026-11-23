# gfproject.json

`gfproject.json` 描述 Goldfish 项目的包身份、内部 domains 和 `gf` 子命令。
标准库资源 [`library/resources/gfproject.schema.json`](./library/resources/gfproject.schema.json) 使用 JSON Schema Draft 2020-12
定义配置结构，编辑器可通过顶层 `$schema` 字段启用自动补全和校验。

## package

`package` 是可选的项目发行包元数据。它只描述当前项目，不负责声明外部依赖，也不改变
Scheme `import`、domains 或工具加载行为。

```json
"package": {
  "name": "lang",
  "organization": "liii",
  "description": "Goldfish Scheme programming language and standard libraries"
}
```

| 字段 | 必需 | 说明 |
| --- | --- | --- |
| `name` | 是 | 包名，匹配 `^[a-z][a-z0-9_-]*$` |
| `organization` | 是 | 发布组织，匹配 `^[a-z][a-z0-9_-]*$` |
| `description` | 否 | 简短描述，省略时解析为空字符串 |

包标识的展示形式为 `name@organization`。Goldfish 是 `lang@liii`，Pi coding agent 是
`pi@liii`。包名与仓库目录名、Scheme library 名及工具命令名互不推导。

`package` 对象不允许未知字段。版本、下载地址和外部依赖不属于当前 Schema，后续分别由独立
设计引入。未声明 `package` 的现有项目仍然有效。

## domains 与 tools

`domains` 描述项目内部板块及其加载根，`depends-on` 表示内部 domain 依赖；`tools` 注册
`gf` 子命令。两者的既有语义保持不变。顶层允许扩展字段，以便后续配置演进。

完整结构及字段约束以 `library/resources/gfproject.schema.json` 为准。

## Scheme 解析接口

```scheme
(import (liii gfproject package))

(define config (string->json "{\"package\":{\"name\":\"lang\",\"organization\":\"liii\"}}"))
(define package (json->gfproject-package config))

(gfproject-package-name package)         ; => "lang"
(gfproject-package-organization package) ; => "liii"
(gfproject-package-description package)  ; => ""
(gfproject-package-id package)           ; => "lang@liii"
```

没有 `package` 字段时，`json->gfproject-package` 返回 `#f`。非法元数据会报告 `type-error`
或 `value-error`，不会根据项目目录、domains 或 tools 猜测包身份。

## 校验

任意兼容 JSON Schema Draft 2020-12 的校验器均可使用。例如：

```bash
python3 -m pip install check-jsonschema
check-jsonschema --schemafile library/resources/gfproject.schema.json gfproject.json
```

Goldfish 自带的 `(liii json schema)` 可校验配置结构；当前版本没有正则表达式后端，因此
`pattern` 仅作为注解处理。`name` 与 `organization` 的字符集由
`json->gfproject-package` 强校验。测试中使用 `minLength` 和 `anyOf` 补充覆盖命名字符集，
标准 `pattern` 语义由 Draft 2020-12 校验器验证。
