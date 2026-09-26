#!/usr/bin/env python3
#
# Copyright (C) 2026 The Goldfish Scheme Authors
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations
# under the License.
#

import os
import time
from multiprocessing import Pool, cpu_count

# seq_fib 纯 CPU 密集型朴素递归计算（无缓存）
def seq_fib(n: int) -> int:
    if n <= 1:
        return n
    return seq_fib(n - 1) + seq_fib(n - 2)

# expand_tree 将 Fibonacci 树展开指定深度，生成并发叶子任务列表
def expand_tree(n: int, depth: int) -> list:
    if depth <= 0 or n <= 1:
        return [n]
    return expand_tree(n - 1, depth - 1) + expand_tree(n - 2, depth - 1)

# parallel_fib 基于 Python 原生多进程池与队列的树形分治并行算法
def parallel_fib(n: int, depth: int, pool: Pool) -> int:
    leaves = expand_tree(n, depth)
    # 将 32 个子树叶子任务分发给进程池并行计算，收集结果并累加
    results = pool.map(seq_fib, leaves)
    return sum(results)

def main():
    workers = cpu_count()

    print("========================================================")
    print("  Python 原生多进程 2分钟多核挑战：能算到 fib 几？")
    print("  算法：基于 multiprocessing 树形分治并行 Fibonacci")
    print("========================================================\n")

    print(f"检测到可用 CPU 工作核心数: {workers}")

    target_duration_sec = 120
    print(f"挑战目标时长: {target_duration_sec} 秒 (2 分钟)")
    print("并发机制：利用 multiprocessing 绕过 GIL，每阶展开为 32 个叶子任务打满 20 个核心！")
    print("请在另一个终端执行 htop 查看 CPU 占用（整整 2 分钟全部打满 2000%）！\n")

    start_time = time.time()
    deadline = start_time + target_duration_sec

    # 从 35 阶开始冲刺（展开 5 层 = 32 个并发子任务）
    current_n = 35
    depth = 5
    max_completed_n = 0
    max_completed_val = 0

    print("多核分治引擎已全线开火，开始冲击更高阶 Fibonacci...\n")

    # 预创建常驻进程池，避免频繁创建销毁子进程
    with Pool(processes=workers) as pool:
        while time.time() < deadline:
            t0 = time.time()
            ans = parallel_fib(current_n, depth, pool)
            t1 = time.time()

            cost = t1 - t0
            total_elapsed = t1 - start_time
            rem_sec = int(max(0, target_duration_sec - total_elapsed))

            max_completed_n = current_n
            max_completed_val = ans

            print(f"  [剩余 {rem_sec}s | 耗时 {cost:.4f}s] 攻克 fib({current_n}) = {ans}")

            current_n += 1

    print("\n2 分钟挑战时间到！正在生成统计战报...")

    total_sec = time.time() - start_time

    print("\n========================================================")
    print("  2 分钟多核极限冲刺完成！")
    print(f"实际总运行时间: {total_sec:.2f} 秒")
    print(f"利用 {workers} 个核心，2 分钟内成功攻克的最大阶数是:\n")
    print(f"    >>>  fib({max_completed_n}) = {max_completed_val}  <<<\n")
    print("========================================================")

if __name__ == "__main__":
    main()
