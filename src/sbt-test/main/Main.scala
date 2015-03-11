object Main {
	def main(args: Array[String]): Unit = {
		println("start")
		Thread.sleep(5000)
		println("hello world")
		new Test
	}
}

class Test {
	println("test")
}
