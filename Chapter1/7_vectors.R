# Vectors

v <- c(10,20,30,40)
v

# accessing the elements by their ids
v[2]
v[c(2,3)]                   
# the same, but instead of id a corresponding bit is set
v[c(FALSE,TRUE,TRUE,FALSE)] 

# accessing the elements by their value
which(v=="20" | v=="30")     # << functions that returns a vector of ids
v[which(v=="20" | v=="30")]
v=="20" | v=="30"            # << direct logical expression
v[v=="20" | v=="30"]

# you can use aggregates as constants, the following returns min and max values
v <- c(10,20,30,40)
v[v==min(v) | v==max(v)]

# find matching (or vice-versa) elements by using %in% function
v1 <- c(1,2,3,4,50,60,7,8,9)
v2 <- c(50,60,70)
v2 %in% v1         # << matching
v2[(v2 %in% v1)]   # << matching
!(v2 %in% v1)      # << the other way around
v2[!(v2 %in% v1)]  # << the other way around

# operations on vectors
v <- c(10,20,30,40)
v <- 0 # << assigning a vector of different size will result in a new vector 
v

v <- c(10,20,30,40)
# the following three will be applied to all elements! no need for a loop!
v <- v+1
v
v <- v*10
v
v <- v-v+1
v

# update some elements only
v <- c(10,20,30,40)
v[v==10 | v==40] <- c(9, 39)
v

v <- c(10,20,30,40)
v[v==10 | v==40] <- v[v==10 | v==40]-1  
v

# operations on multiple vectors of the same size and type
# note that again, no need for a loop!
v1 <- c(10,20,30,40)
v2 <- c(1,2,3,4)
v3 <- c(1,10,100,1000)
  
v1 <- (v1+v2)*v3 
v1

vc1 <- c("a", "b", "c")
vc2 <- c("1", "2", "3")
vc1 <- paste0(vc1,vc2)
vc1

# sorting 
v <- c(10,20,30,40)
order(v, decreasing = TRUE) # << returns sorted ids
v <- v[order(v, decreasing = TRUE)] # << returns sorted values
v




# >>>>
# the same applicable to matrices
m1 <- cbind(c(10,20),c(30,40))
m1
m1*10

m2 <- cbind(c(1,2),c(3,4))
m2
(m1+m2)*10

# with matrices we can use vectors as well
m1
m1 *c(0,2) # << note how it's updated!  





