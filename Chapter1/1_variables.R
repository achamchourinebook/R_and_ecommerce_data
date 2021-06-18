# Constants and variables

# any variable is a vector
n <- 1
n
n[1]

c <- "a"
c

# names are case sensitive! N and n contain different values
N <- 2
N
n

# let's create some vectors
i <- 1:3
i
i <- c(1,3)
i
# note that 3 here will be converted to a character!
i <- c(3,"a")
i

# NA, TRUE, FALSE are constants! 
na <- NA
na
b.true <- TRUE
b.true
b.false <- FALSE
b.false
this.is.notboolean <- "FALSE"
this.is.notboolean

# some naming examples 
n.1 <- 1
n.1
n_1 <- 2
n_1

# a special name 
`xo xo` <- "xo xo xo"
`xo xo`

x <- "multi line 
string"
x

# escape characters
e <- "dwdwqd4dew9\\nasdas"
writeLines(e)
e <- "dwdwqd4dew9\nasdas"
writeLines(e)

# quotes inside quotes
dq <- "ieqw\"ueiwq"
dq
writeLines(dq)

