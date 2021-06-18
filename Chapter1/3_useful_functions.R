# Basic useful functions

print(10)

# this will result in a vector of numerics!
c(TRUE, 1)
# this two will be a vector of numerics
c(TRUE, 0.5, 11.4124)
# this will result in a vector of characters!
c(TRUE, "a")
# this will be boolean
c(TRUE, FALSE)

# check the x value
x <- ifelse(ifelse(1==1,"!","?")=="!","Exclamation","Question")
x

# simple and useful
paste0(1,2,3," ","is a number")
paste0(c(1,2,3), collapse="+")

seq(1, 100, 20)

# compare the outputs
rep(c(1,2,3), each = 2)
rep(c(1,2,3), times = 2)
rep(c(1,2,3), times = 2, each = 2)



