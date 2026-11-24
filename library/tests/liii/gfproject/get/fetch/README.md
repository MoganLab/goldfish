# (liii gfproject get fetch) 模块函数分类索引

远程包获取：浅克隆、commit 解析、包元数据读取。

## 函数

- `get-git-clone` - 按 ref 浅克隆到临时目录（失败回退全量 clone+checkout；失败自动清理）
- `get-git-commit` - 解析 HEAD commit
- `get-read-project-package` - 读取项目 package 元数据，缺失报错

## 运行测试

```bash
bin/gf test library/tests/liii/gfproject/get/fetch/
```
