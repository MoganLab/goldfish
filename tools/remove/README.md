# gf remove

`gf get -g` 的逆操作：按短包名卸载用户级依赖，删除安装目录（本地开发包只清注册），
并清理 `~/.gf/gfproject.json` 的 depends/tools 注册与 `gfproject.lock` 条目。

## 用法

```bash
gf remove -g <name@org>        # 卸载用户级包并注销其 gf 子命令
gf remove -h                   # 查看帮助
```

## 示例

```bash
gf remove -g pi@liii           # 卸载 gfcode，gf pi 不再可用
gf remove -g highlight@liii    # 本地开发包：只清注册，工作副本保留
```

## 行为

- 与 `gf get` 对称，第一版只做单包用户级操作，`-g` 必填；不提供 `rm` 别名
  （避免与系统 `rm` 混淆）。
- 远程包：删除 `~/.gf/site-lisp/<org>/<name>/<version>/` 安装目录，
  已空的 `<name>`/`<org>` 目录一并清理，`site-lisp` 根保留。
- 本地开发包：只清 depends/lock 与工具注册，**不触碰工作副本**。
- 只注销 `tools.<name>.package` 等于该包 id 的工具绑定；他人归属或
  手工登记（无 package 绑定）的工具不受影响。
- 未注册包报错并列出已装包；卸载后可用 `gf get -g` 重新安装。
