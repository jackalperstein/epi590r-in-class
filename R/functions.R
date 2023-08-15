# 1 Square a number. You’re tired of writing x^2 when you want to square x, so you want a function
# to square a number. You can call it square(). I showed this in the slides, now try on your own!

# start out with a number to test
x <- 3
# you'll want your function to return this number
x^2

square <- function(x) {
	suqared_value <- x^2
	return(suqared_value)
}
# test it out
square(x)
square(53)
53^2 # does this match?


# 2 Raise to any power. You don’t just want to square numbers, you want to raise them to higher powers too.
# Make a function that uses two arguments, x for a number, and power for the power. Call it raise().


raise <- function(x, power=2) {
	value <- x^power
	return(value)
}

# test with
raise(x = 2, power = 4)

raise(2,4)

raise(2)

# should give you
2^4
