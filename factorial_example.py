import sys

def factorial (n):
	if n == 0:
		return 1
	else:
		return (n * factorial(n-1))

num = int(sys.argv[1])
print(factorial(num))