class testiter {
	field val
	fn __init__(self) {
		self.val = 1
	}

	fn __iter__(self) {
		self
	}

	fn __next__(self) {
		if self.val == 1 {
			self.val = 0
			1
		} else {
			null
		}
	}
}

for a in testiter() {
	print(a)
}
