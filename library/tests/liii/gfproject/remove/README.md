# (liii gfproject remove) 模块函数分类索引

卸载编排：查 depends → 删远程安装目录 → 清 depends/lock → 注销自有工具绑定。
命令入口为 `(liii goldremove)`（tools/remove/）。

## 函数

- `gfproject-remove-package` - 卸载单个用户级包（短包名 name@organization）

## 约定

- 远程包删除 site-lisp 安装目录，已空的 `<name>`/`<org>` 目录一并清理，site-lisp 根保留。
- 本地开发包只清注册，不触碰工作副本。
- 仅注销 `tools.<name>.package` 等于该包 id 的工具绑定，他人归属与无绑定工具保留。
- 未注册包与非法 id（URL/路径/无 @）抛 value-error；未注册时提示已装包列表。

## 运行测试

```bash
bin/gf test library/tests/liii/gfproject/remove/
```
