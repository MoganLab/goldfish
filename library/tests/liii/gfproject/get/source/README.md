# (liii gfproject get source) 模块函数分类索引

来源解析：区分本地路径、仓库 URL 与 `name@organization` 短包名。

## 函数

- `get-source-kind` - 判定来源类别：'path / 'url / 'package
- `get-source-local-path` - 本地路径展开与归一化（~、相对路径）
- `get-package-id-part` - 校验包标识组件字符集

## 运行测试

```bash
bin/gf test library/tests/liii/gfproject/get/source/
```
