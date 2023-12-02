if false && { print("Should not print #1") false } {
	 print("Should not print #2")
}

if false && { print("Should not print #3") true } {
	 print("Should not print #4")
}

if true && { print("Should print #1") false } {
	 print("Should not print #5")
}

if true && { print("Should print #2") true } {
	 print("Should print #3")
}
