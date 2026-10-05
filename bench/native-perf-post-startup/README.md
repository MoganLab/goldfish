# 两项优化后的热点复测

基于 `ccc60e66`、runtime 二进制 SHA256 `ed1629d1…5c5ad9`，复测热启动和
原有 50,000 元素宽集合工作负载。两次使用同一个有效缓存
`v5123ede839a7`、默认 GC 与优化级别。`perf` 7.1.6 使用
`cpu-clock:u`、99 Hz、DWARF 调用栈；两份采样均无丢失。采样执行的仍是
`bin/gf`。归档中的符号二进制来自相同 release objects，`.text` 和
`.rodata` 均逐字节匹配。

## 当前采样

| 工作负载 | 结果 | CPU 样本 | 主要自身 CPU 占比 |
|---|---:|---:|---|
| R7RS 热启动 `(+ 1 1)` | 2；启动 6.338 秒 | 621 | evaluator 13.20%、`GC_free` 10.63%、环境查找 6.92%、GC mark 6.76%、`GC_malloc` 5.15% |
| 50k 宽集合构造及检查 | `PROFILE-OK`；阶段 6.489 秒 | 633 | evaluator 13.43%、`GC_free` 12.32%、GC mark 6.32%、continuation 帧压入 6.00%、环境查找 5.37%、`GC_malloc` 5.06% |

宽集合中 `Evaluator::vector_values` 不再出现在报告阈值 0.8% 以上的
调用路径中。之前相同输入的旧 profile 将该函数列为 **27.73% inclusive**；
旧样本包括向量访问修复之前的路径。这直接核验了该复制热点的消失。

当前调用树还给出了可操作的分配目标。`Evaluator::proper_list` 占 **7.27%
inclusive**，其 `std::vector<Value>` 扩容约占 **6.64%**，扩容路径中可见
内存申请与回收。`KontFrame` 析构占 7.11% inclusive，帧向量压入占 6.00%；
`std::vector<Value>` move assignment 自身占 3.32%。这些数值来自嵌套调用树，
不能相加。环境查找当前占 5.37% self。查找沿父环境逐层访问哈希表；任何
缓存都必须保留 `define`、`set!`、链接绑定和词法遮蔽后的可见性。

`GC_free`、GC mark 和 GC 分配合计有明显成本，但不能据此去掉释放：
`gc_alloc.cpp` 的历史受控观测显示，延迟释放会扩大保守扫描并显著拖慢运行。
优先追踪 `proper_list` 的短命 vector 分配和 move assignment 的调用点，
做相同输入的 A/B 采样，再决定是否改 environment lookup 或 continuation 帧。
`proper_list` 的精确预留需要额外遍历；应实测短列表和宽调用两种情况，避免只
凭分配数推断速度收益。

旧 profile (`5e57ad12`) 与当前二进制和时间样本不同，只用于确认此前的
`vector_values` 路径是否仍出现，不作为第三项优化的严格前后速度对照。
启动前三次 R7RS 热运行中位数为 6.46 秒，详见
[启动优化记录](../native-startup-cache/README.md)。

## 分配优化筛查（2026-10-05）

尝试在 `Evaluator::proper_list` 中先遍历计数，再为结果向量一次性预留空间。
它减少扩容，但把链表遍历从一次变成两次。以同一宽集合输入作隔离 A/B，基线三次
阶段耗时为 6.680、6.690、6.721 秒，中位数 6.690 秒；候选五次为 6.515、
6.537、6.621、6.964、6.997 秒，中位数 6.621 秒。候选波动更大，进程墙钟中位数
只从 15.60 秒变为 15.34 秒，峰值 RSS 中位数从 57,516 KiB 变为 57,460 KiB。
约 1% 的阶段差距不足以证明稳定收益，因此没有保留代码改动。

恢复原实现并重建后，以相同环境复测现有优化：R7RS 热启动三次均返回 `2`，墙钟
中位数 6.07 秒、峰值 RSS 中位数 32,048 KiB；50,000 元素宽集合三次均通过自检，
阶段中位数 6.569 秒、进程墙钟中位数 15.50 秒。向量访问回归测试通过：
`vector-ref` 15/15，`vector-length` 9/9。候选的原始运行记录保存在本机
`/tmp/gf-proper-list-ab` 与 `/tmp/gf-round-close`，输入摘要见上文重现输入。

本轮没有找到值得保留的第三项性能改动。下一轮应从新的、针对真实应用负载的
profile 开始；若 continuation 帧或环境查找仍然突出，再为具体调用路径设计局部
候选，避免仅凭 inclusive CPU 百分比改动通用解释器机制。

## 重现

在仓库根目录运行，缓存必须对应当前 runtime；先编译两个工作负载库，
并为 profile 输出选一个新目录：

```sh
GOLDFISH_CACHE_DIR=/tmp/gf-startup-cache/cache \
  perf record -e cpu-clock:u -F 99 --call-graph dwarf,8192 --clockid mono \
  -o /tmp/startup.data -- timeout --kill-after=5s 60 \
  env GOLDFISH_CACHE_DIR=/tmp/gf-startup-cache/cache GOLDFISH_DEBUG=timing \
./bin/gf -m r7rs -e '(+ 1 1)'

GOLDFISH_CACHE_DIR=/tmp/gf-startup-cache/cache \
  ./bin/gf -m r7rs -I bench \
  -e '(import (native-scale timing) (liii set))'

timeout --kill-after=5s 150 bash bench/native-perf-rehash/profile-set.sh \
  /tmp/gf-startup-profile \
  /tmp/gf-startup-cache/cache \
  "$PWD/bench/native-perf-rehash/set-wide/input.scm"
```

完整 stdout/stderr、FIFO 控制确认、输入、报告、压缩后的原始采样及符号
文件都在本目录。`metadata.tsv` 记录二进制和数据摘要；`checksums.sha256` 校验
归档内容。重放报告时，把 `gf-symbols.gz` 解压到
`SYMFSDIR/home/jinser/vie/projet/lang/goldfish/bin/gf`，并将
`SYMFSDIR/nix` 链接到 `/nix`，再把压缩的采样解压后交给 `perf report --symfs`。
