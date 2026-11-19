# (liii gfproject external) 模块函数分类索引

`(liii gfproject external)` 负责在运行外部项目注册的工具时，把该项目根及其 domains 注入 `*load-path*`。

## 查看函数文档

```bash
gf doc liii/gfproject/external
gf doc liii/gfproject/external "gfproject-find-project-root"
```

## 函数分类索引

### 外部项目定位
- `gfproject-find-project-root` - 从目录向上查找最近的 gfproject.json 所在项目根

### 外部项目加载
- `gfproject-external-project-load-path!` - 注入外部项目根与全部 domain 根，幂等去重

## 说明

外部项目（如 `~/git/gfcode`）的 domains 由它自己的 `gfproject.json` 提供，不复制到
`~/.gf/gfproject.json`，也不与当前项目 domains 跨层合并。
