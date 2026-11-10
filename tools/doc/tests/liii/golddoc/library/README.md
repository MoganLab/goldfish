# (liii golddoc library) 模块导览

`(liii golddoc library)` 根据库查询字符串和当前 `*load-path*` 定位可见库、对应测试根目录与库级文档。库级文档优先使用库目录中的 `README.md`，并兼容旧版 `<library>-test.scm` 文件。

## 查看函数文档

```bash
bin/gf doc liii/golddoc/library "函数名"
```

## 函数分类索引

### 库定位
- `find-visible-library-root` - 查找可见库所在的源码加载根目录
- `find-tests-root-for-load-root` - 根据源码加载根目录查找测试根目录

### 文档定位
- `library-doc-path` - 查找库的 README.md 或旧版测试文档
- `excluded-test-group?` - 判断测试分组是否不属于文档搜索范围
