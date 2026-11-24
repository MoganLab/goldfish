# (liii gfproject get ...) 模块函数分类索引

`gf get` 的支撑库：来源解析、用户级存储、远程获取与安装编排。
命令入口为 `(liii goldget)`（tools/get/）。

## 子模块

- `(liii gfproject get source)` - 来源解析：`get-source-kind`（path/url/package）、`get-source-local-path`、`get-package-id-part`
- `(liii gfproject get store)` - `~/.gf` 布局与读写：`get-user-dir`、`get-site-lisp-dir`、`get-package-dir`、`get-package-root`、`get-depends-entry`、`get-lock-entry`、`get-json-set`（upsert）、`get-json-sub`
- `(liii gfproject get fetch)` - 远程获取：`get-git-clone`（浅克隆到临时目录）、`get-git-commit`、`get-read-project-package`
- `(liii gfproject get install)` - 安装编排：`get-install`

## 约定

- 包标识为 `name@organization`（如 `highlight@liii`），与库名、工具名解耦。
- `~/.gf/gfproject.json` 的 `depends` 记录来源声明（url 或 path/local），
  同时是短包名的本地注册表；`tools.<name>.package` 绑定提供该工具的包。
- `~/.gf/gfproject.lock` 记录事实：远程包 version=ref（或 HEAD）+ commit；本地包 version=dev。
- 远程包完整项目安装到 `~/.gf/site-lisp/<org>/<name>/<version>/`（version 为 ref 或 HEAD）；
  本地开发包不复制源码，直接引用工作副本。
- 未指定 ref 时取远端 HEAD；指定 ref 时按分支/标签/commit 检出（commit 走全量 clone+checkout）。重装即更新，commit 相同幂等。

## 运行测试

```bash
bin/gf test library/tests/liii/gfproject/get/
bin/gf test tools/get/tests/
```
