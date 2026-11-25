# gf get

安装带 `gfproject.json` 的完整多 domain 项目为用户级依赖，并自动编辑
`~/.gf/gfproject.json` 注册其 `gf` 子命令。

## 用法

```bash
gf get -g <url>                # 首次安装：仓库 URL（含 file://，缺省 --ref @latest）
gf get -g <path>               # 本地开发模式：直接引用工作副本
gf get -g <name@org>           # 已注册包：重装/更新（复用登记 ref）
gf get -g <name@org> --from <url-or-path>   # 显式身份 + 来源
gf get -g <url> --ref <branch|tag|commit>  # 按指定引用检出
gf get -g <url> --ref @latest               # 最新稳定 release tag（排除 pre-release）
gf get -g <name@org> --force   # 更换来源 / 覆盖工具归属
```

## 示例

```bash
gf get -g ~/git/gf-highlight   # 注册 highlight@liii，gf cat 全局可用
gf get -g ~/git/gfcode         # 注册 pi@liii，gf pi 全局可用
gf get -g https://gitee.com/MoganLab/gf-highlight --ref v1.0.0   # 安装指定 tag
gf get -g https://github.com/MoganLab/gfcode.git --ref @latest   # 安装最新稳定 release
gf get -g highlight@liii       # 按登记 ref 更新（缺省 @latest）
```

## 行为

- 首次安装从仓库 `gfproject.json` 读取 `package` 元数据（name/organization），
  建立短包名到来源的本地映射；后续短名操作不再需要 URL。
- 远程包完整项目落盘 `~/.gf/site-lisp/<org>/<name>/<version>/`，
  version 为解析后的 ref；lock 记录解析到的 commit；本地包 lock 记 version=dev，不锁 commit。
- `--ref` 缺省为 `@latest`；显式指定时按分支/标签/commit 检出，短名重装复用登记 ref。
- `@latest` 为伪 ref：经 `git ls-remote --tags` 列出全部 tag，过滤出合法稳定
  （非 pre-release）semver 并取最高者。depends 记录声明 `@latest`，install 时解析为
  真实 tag，安装目录与 lock 记该 tag（version/ref）及 commit；短名重装复用 `@latest`，
  每次重新解析到最新稳定 tag。无任何稳定 tag 时报错，不回退 HEAD。
  仅依赖 git 协议，对 GitHub/Gitee/自建/SSH 仓库通用。
- 包声明的 tools 自动写入用户配置并加 `package` 绑定；运行工具时由
  depends/lock 解析项目根，激活其全部 domain roots。
- 同 id 不同来源、不同 ref、工具归属冲突时拒绝，`--force` 显式覆盖。
- 第一版仅支持单包用户级（-g）操作；遍历更新、--sync、项目级 depends 为后续任务。
