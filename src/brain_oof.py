def f(x):
	def g():
		nonlocal x
		x = x + 1
		return x
	return g

g = f(2)
h = f(20)
