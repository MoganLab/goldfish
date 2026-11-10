# (liii gfproject) 模块函数分类索引

`(liii gfproject)` 负责读取与合并项目配置（gfproject.json），并解析工具注册信息。

## 查看函数文档

```bash
gf doc liii/gfproject/config
gf doc liii/gfproject/tools
```

## 子模块

- `(liii gfproject config)` - 项目配置的读取与合并：`gfproject-root`、`gfproject-load-config`、`gfproject-deep-merge` 等
- `(liii gfproject tools)` - 工具注册解析与执行：`gfproject-resolve-tool`、`gfproject-prepare-and-run-tool` 等

## 配置约定

项目根目录与标准库目录下都可放置 `gfproject.json`，标准库配置在前、本地配置在后做深度合并，本地配置覆盖同名键。
