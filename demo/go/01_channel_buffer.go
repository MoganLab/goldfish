//
// Copyright (C) 2026 The Goldfish Scheme Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations
// under the License.
//

package main

import (
	"fmt"
	"time"
)

// producer 生产者：向通道发送数据
//
// 参数类型 chan<- int 为单向写通道。
// 通过在发送前后打印日志，可清晰观察何时发生阻塞：
// - 若通道有空余缓冲空间（或有接收者在等待），发送立即完成；
// - 若通道无缓冲且无接收者，或缓冲已满，则当前 goroutine 阻塞在 `ch <- i` 处。
func producer(name string, ch chan<- int, count int) {
	defer func() {
		close(ch)
		fmt.Printf("[%s producer] 全部数据发送完毕，通道已关闭\n", name)
	}()

	for i := 1; i <= count; i++ {
		fmt.Printf("[%s producer] 准备发送: %d\n", name, i)
		ch <- i
		fmt.Printf("[%s producer] 发送成功: %d\n", name, i)
	}
}

// consumer 消费者：从通道接收并处理数据
//
// 参数类型 <-chan int 为单向读通道。
// 通过 sleep 模拟数据处理耗时，让生产者的缓冲填满与阻塞现象更清晰地呈现。
func consumer(name string, ch <-chan int) {
	for v := range ch {
		// 模拟耗时处理，使缓冲被填满的情景更容易观察
		time.Sleep(50 * time.Millisecond)
		fmt.Printf("[%s consumer] 成功接收: %d\n", name, v)
	}
	fmt.Printf("[%s consumer] 检测到通道关闭，消费结束\n", name)
}

// runDemo 演示指定通道的生产与消费行为
func runDemo(name string, ch chan int, capacity int) {
	fmt.Printf("=== 演示 %s (缓冲容量: %d) ===\n", name, capacity)
	// 启动后台生产者 goroutine
	go producer(name, ch, 5)
	// 主 goroutine 作为消费者同步消费
	consumer(name, ch)
	fmt.Println()
}

func main() {
	// ch0: 无缓冲通道（容量为 0）
	// 发送与接收必须同步交会（rendezvous）：发送者会阻塞直到消费者接收
	ch0 := make(chan int)
	runDemo("ch0", ch0, 0)

	// ch1: 1 个缓冲的通道
	// 缓冲区可容纳 1 个元素：第 1 个元素发送不会阻塞，发第 2 个元素时若未被取走则阻塞
	ch1 := make(chan int, 1)
	runDemo("ch1", ch1, 1)

	// ch3: 3 个缓冲的通道
	// 缓冲区可容纳 3 个元素：前 3 个元素连续发送不阻塞，发第 4 个元素时缓冲已满而阻塞
	ch3 := make(chan int, 3)
	runDemo("ch3", ch3, 3)
}
