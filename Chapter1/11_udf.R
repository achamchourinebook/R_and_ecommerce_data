# functions

f <- function(x,y=NA){
  print(paste0("Inside the function 1: z=",z," r=",r))
  
  z <- 0 
  if(!is.na(x) & !is.na(y))
  {
    r <- x*y 
  }
  print(paste0("Inside the function 2: z=",z," r=",r))

  if(is.na(x) | is.na(y))
  {
    return(-1)
  }
  r
}

r <- 1111
z <- 3333
print(paste0("Before the function call: z=",z," r=",r))
f(10,10)
print(paste0("After the first function call: z=",z," r=",r))
f(10)
print(paste0("After the second function call: z=",z," r=",r))


