# (liii gfproject get install) 模块函数分类索引

安装编排：来源解析 → 获取 → 身份校验 → 落盘/登记 → 写配置与 lock。

## 函数

- `get-install` - 安装/重装/更新单个包（URL、本地路径、短包名）

## 约定

- 本地路径为开发模式：不复制源码，lock 记 version=dev。
- 远程 URL 安装到 site-lisp/<org>/<name>/<version>/，version 为 ref 或 HEAD（未指定 ref 时），lock 记 commit。
- 同 id 不同来源、不同 ref、工具归属冲突拒绝；--force 覆盖。
- 短名重装复用 depends 中登记的 ref。

## 运行测试

```bash
bin/gf test library/tests/liii/gfproject/get/install/
```
