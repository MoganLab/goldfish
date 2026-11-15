# (liii pi ai config runtime)

运行时配置 `<pi-config>` 记录与读写合并原语。

## 测试命令

```bash
bin/gf test library/tests/liii/pi/ai/config/runtime/
```

## 函数分类

- `pi-config?` - 判断对象是否为 `<pi-config>` 记录
- `make-pi-config` - 构造带默认值与关键字参数的 `<pi-config>` 记录
- `pi-cfg-ref` - 读取配置中指定键的取值
- `pi-cfg-set` - 返回更新指定键后的新配置
- `pi-cfg-merge` - 批量合并多个配置键值对
