# 分阶段 JSON 性能探针

四个独立原生进程分别测量对象解析、序列化、查键和键枚举。每个脚本使用同一个
200 键对象，在计时窗口外构造并预热数据；计时后验证结果并输出
`JSON-PHASE-OK`。运行单项，例如：

```sh
GOLDFISH_CACHE_DIR=/tmp/gf-startup-cache/cache \
  ./bin/gf -m liii bench/json-phases/parse.scm
```

带 CPU profile 运行单项（输出目录必须为空或不存在）：

```sh
bash bench/json-phases/profile.sh /tmp/gf-json-profile/parse \
  /tmp/gf-startup-cache/cache parse
```

`profile.sh` 用 perf FIFO 控制，仅在 `PHASE-BEGIN` 与 `PHASE` 之间采样，因此
启动、导入和 fixture 构造不进入 CPU 样本。基线的四份原始采样和调用报告在
`profile-before/`；枚举实现更新后的复测在 `profile-after/enumerate/`。所有采样
均使用 perf 7.1.6、`cpu-clock:u`、99 Hz、DWARF 调用栈，无丢样。两个时间值都由
脚本的单调时钟记录，profile 结果只是成本分布，不是跨机器性能承诺。

## 基线与候选

| 场景 | 操作次数 | 阶段时间 | CPU 样本 | 观察 |
|---|---:|---:|---:|---|
| parse | 80 | 5.216 秒 | 511 | `utf8_length` 6.26%、`utf8_byte_offset` 3.72%；可能受逐字符 UTF-8 索引影响 |
| serialize | 200 | 6.509 秒 | 637 | evaluator/GC 为主，没有单个 JSON 专属热点 |
| lookup | 20,000 | 21.916 秒 | 2,161 | evaluator 17.21%、GC_free 15.96%、continuation frame 压入 7.13%、环境查找 6.20% |
| enumerate（旧实现） | 8,000 | 5.701 秒 | 561 | `json-keys` 先验证对象，再遍历取键；存在可消除的一次完整遍历 |
| enumerate（新实现） | 8,000 | 4.040 秒 | 395 | 同一操作和采样窗口，结果断言仍通过 |

基于旧枚举路径实现的 `enumerate-ab.scm` 交替运行三轮。每轮 4,000 次：旧实现
中位数 2.872 秒；候选中位数 2.040 秒，约快 29%。候选先以 `list?` 保持 improper
和 circular 输入的拒绝行为，再在一次循环中检查每个 alist 项并收集键，最后
reverse。它替代 `json-object?` 的全量 pair 验证加 `map` 两次遍历。

此改动落在 `goldfish/liii/json.scm` 的 `json-keys`。对应目录测试 20 个文件全部
通过，共含本次补充的 4 个空值、非对象、improper 和 circular 边界断言。直接候选
A/B 的逐轮结果保存在 `enumerate-ab.before.stdout`；profile `run.stdout` 含阶段
标记与结果检查，`control.log` 记录 perf 启停确认。

键枚举现已完成一次有稳定收益的优化。下一项应转向 parse：先评估逐字符 `string-ref`
触发的 UTF-8 扫描成本，再设计保留 Unicode 与转义行为的候选并独立做 A/B。当前
profile 不足以证明应该先重写解析器或改 evaluator / GC。

`gf-symbols.gz` 与本目录所有 profile 使用的 runtime 二进制匹配。解压到
`SYMFSDIR/home/jinser/vie/projet/lang/goldfish/bin/gf`，并把 `/nix` 链接到
`SYMFSDIR/nix` 后，可使用 `perf report --stdio --symfs SYMFSDIR -i DATA` 重放。
