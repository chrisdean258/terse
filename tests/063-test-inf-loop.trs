class infloop {
	field state
	fn __init__(self) {
		self.state = 0
	}

	fn __iter__(self) self

	fn __next__(self) self.state++

}

for i in infloop() {
	if i == 10000 {
		print(i)
		break
	}

}
