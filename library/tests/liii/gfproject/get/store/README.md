# (liii gfproject get store) 模块函数分类索引

`~/.gf` 用户级存储：目录布局、depends/lock 读写、JSON upsert 辅助。

## 函数

- `get-user-dir` / `get-site-lisp-dir` / `get-lock-path` - 目录与 lock 路径
- `get-package-dir` - 远程包安装目录（version 缺省 HEAD）
- `get-package-root` - 已安装包项目根（本地 path / site-lisp）
- `get-read-json` / `get-write-json` - JSON 文件读写
- `get-depends-entry` / `get-lock-entry` - 单包声明/事实提取
- `get-declared-ref` - 取 depends 中登记的 ref（仅远程）
- `get-resolve-declared-source` - 短包名 → 来源解析
- `get-json-set` - upsert（已有键替换、缺失键新增）
- `get-json-sub` - 取子对象，缺失返回空对象

## 运行测试

```bash
bin/gf test library/tests/liii/gfproject/get/store/
```
