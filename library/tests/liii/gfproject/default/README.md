# (liii gfproject default) 模块测试

无参数 `gf` 的默认工具加载。

- `gfproject-run-default-tool` - 只使用 `~/.gf/gfproject.json` 中注册的默认工具；未注册时输出英文安装与帮助提示。

测试同时验证无参数 `gf` 使用用户级 `pi`、显式 `gf pi` 保留项目级覆盖，以及未注册时返回非零退出码并提示 `gf get ... --ref @latest` 和 `gf help`。
