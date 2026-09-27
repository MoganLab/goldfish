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

import "fmt"

// producer 生产者函数：向通道发送数据
//
// 参数类型 chan<- int 为单向通道（只写通道），通过 Go 编译期类型检查
// 确保生产者只能发送数据，无法接收数据，增强了代码的安全性和意图表达。
//
// 在 Go 并发哲学中，通常由数据的发送方（生产者）负责关闭通道，
// 接收方则负责检测通道何时关闭，避免向已关闭的通道发送数据引发 panic。
func producer(ch chan<- int) {
	// defer 保证在 producer 函数退出（所有数据发送完毕）时关闭通道。
	// 关闭通道向接收方发出信号：后续不再有新数据发送。
	defer close(ch)

	// 依次发送数字 1 到 5
	for i := 1; i <= 5; i++ {
		// 因为 ch 是无缓冲通道，向 ch 发送数据会阻塞，
		// 直到接收方（consumer）准备好接收该数据（即同步交会 rendezvous）。
		ch <- i
	}
}

// consumer 消费者函数：从通道接收并处理数据
//
// 参数类型 <-chan int 为单向通道（只读通道），保证消费者只能从中读取数据，
// 无法向通道写入或关闭通道。
func consumer(ch <-chan int) {
	// for-range 循环是消费通道数据的标准 Go 惯用法：
	// 1. 当通道有数据时，持续接收数据并赋值给 v，进入循环体处理；
	// 2. 当通道为空但未关闭时，当前 goroutine 阻塞等待新数据；
	// 3. 当通道被发送方 close 且通道内无残留数据时，循环自动安全退出。
	for v := range ch {
		fmt.Println("consume:", v)
	}
}

func main() {
	// 1. 创建一个无缓冲的整型通道（unbuffered channel，容量为 0）。
	//    无缓冲通道的发送与接收必须成对出现并同步完成（同步交会 rendezvous）：
	//    发送操作会一直阻塞直到有 goroutine 接收，反之接收也会阻塞直到有数据发送。
	ch := make(chan int)

	// 2. 启动一个独立的 goroutine（轻量级协程）异步执行 producer。
	//    producer 会在后台开始生成数据并发送到通道中。
	go producer(ch)

	// 3. 在当前主 goroutine 中直接同步运行 consumer。
	//    主 goroutine 会在此持续接收并打印数据，直到 producer 发送完毕并 close(ch)。
	//    由于 consumer 在主线程上同步执行，天然起到了等待后台 goroutine 完成的作用，
	//    避免了 main 函数过早退出导致后台 producer 尚未执行完毕的问题。
	consumer(ch)
}
