# Conversion and Formatting

# Date
d <- as.Date("2018-01-01")
d
# to make sure it's date
d <- d-1
d

# integer
i <- as.integer("9")
i

# double
r <- as.double("1222.3")
r

# character
c <- as.character(1)
c

# is() examples
# note that you have to use the proper case for the class as well 
# one of the every two below will return the wrong result! 
is(d, "date")
is(d, "Date")
is(i, "integer")
is(i, "Integer")

# note the use of word "logical" instead of "boolean"
is(TRUE, "logical")

# specialized is() functions
# note that it returns the true type, not the compatibility!
is.character("1")
is.character(1)

is.numeric(1)
is.numeric("1")

is.logical(FALSE)
is.logical("FALSE")

# checking for NA data
is.na(NA | FALSE)

# format function
d <- Sys.time()
format(d, "%Y %B %d")
format(d, "%Y %b %d")
format(d, "%a, %b %d, %Y")
format(d, "%y %b %d")
format(d, "%a, %b %d, %Y")
format(d, "%A, %b %d, %Y")
# you can combine them with any other characters or words
format(d, "The year was %Y and the day was %A")

# with time
format(d, "%y %b %d %H:%M")
format(d, "%y %b %d %I:%M %P")
