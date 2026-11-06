# (liii golddoc index-build) 模块导览

`(liii golddoc index-build)` 依据当前 `*load-path*` 推导每个库加载根目录对应的
`tests` 根目录，并把扫描得到的库导出索引写入 `<tests 根目录>/function-library-index.json`。

## 重建函数索引

```bash
bin/gf doc --build-json
```

## 函数分类索引

### 索引生成
- `build-function-indexes!` - 扫描当前 `*load-path*`，写出各 `tests` 根目录的函数索引
