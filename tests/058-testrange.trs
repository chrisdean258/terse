class TestRange {
	field current, stop, stride

	fn __init__(self, start, stop, stride) {
		self.current = start
		self.stop = stop
		self.stride = stride
	}

	fn __iter__(self) {
		val = self.current
		if val >= self.stop
			null
		else {
			self.current += self.stride
			val
		}
	}
}

a = TestRange(0, 10, 1)
while i <- a.__iter__() {
	print(i)
}
