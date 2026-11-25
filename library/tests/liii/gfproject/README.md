# (liii gfproject) 模块函数分类索引

`(liii gfproject)` 负责读取与合并项目配置（gfproject.json），并解析工具注册信息。

## 查看函数文档

```bash
gf doc liii/gfproject/config
gf doc liii/gfproject/tools
gf doc liii/gfproject/external
```

## 子模块

- `(liii gfproject config)` - 配置读取与合并：`gfproject-root`、`gfproject-load-config`、`gfproject-deep-merge` 等
- `(liii gfproject package)` - 包元数据解析：`json->gfproject-package`、`gfproject-package-id` 等；JSON Schema 见 `library/resources/gfproject.schema.json`
- `(liii gfproject tools)` - 工具注册解析与执行：`gfproject-resolve-tool`、`gfproject-prepare-and-run-tool` 等
- `(liii gfproject default)` - 无参数 `gf` 的默认工具加载：`gfproject-run-default-tool` 仅使用用户级注册
- `(liii gfproject external)` - 外部项目加载：`gfproject-find-project-root`、`gfproject-external-project-load-path!`
- `(liii gfproject get ...)` - 用户级包安装（`gf get` 支撑库）：来源解析、存储、获取与安装编排
- `(liii gfproject remove)` - 用户级包卸载（`gf remove` 支撑库）：`gfproject-remove-package`

## 配置约定

`gfproject.json` 分三层，`tools` 按 `lib -> user -> local` 深度合并：

| 层级 | 路径 | 作用 |
| --- | --- | --- |
| 内置层 | Goldfish 安装目录 / 仓库根 | 内置工具与内置 domains |
| 用户层 | `~/.gf/gfproject.json` | 用户全局注册的外部工具 |
| 项目层 | 当前项目根 | 项目自己的工具与 domains |

- 显式命令解析时，项目层覆盖用户层；内置工具名始终优先（同名自定义注册会被忽略并提示）。
- 无参数 `gf` 默认启动 `pi` 时只使用用户级注册，不受当前项目覆盖；未注册时提示用 `gf get` 安装 gfcode。
- `domains` **不跨层合并**：每份配置中的 `domains.{name}.root` 都相对该 `gfproject.json` 所在目录解析。
- `gfproject-root` 仍只表示当前项目根，用户层配置不会改变 `g_project-root`，因此 `gf fmt` / `gf test` / `gf check` 语义不变。

## 用户全局工具

`~/.gf/gfproject.json` 只需登记工具位置，不需要复制外部项目的 domains：

```json
{
  "tools": {
    "pi": {
      "tools_dir": "~/git/gfcode",
      "tool": "code",
      "organization": "liii",
      "module": "pi/code/cli"
    }
  }
}
```

用户层的相对 `tools_dir` 以 `~/.gf` 为基准，项目层以项目根为基准；推荐直接写绝对路径或 `~/...`。
运行带 `tools_dir` 的外部工具时，会从工具根向上找到外部项目的 `gfproject.json`，把该项目根及其 domains
加入 `*load-path*`，从而加载工具的跨 domain 依赖。
