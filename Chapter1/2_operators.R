# Operators and loops

i <- 21
# this is modulus
i %% 10
# thisis integer division
i %/% 10

b1 <- c(TRUE, TRUE)
b2 <- c(FALSE, TRUE)

# note the difference between the two:
b1 & b2
b1 && b2

i <- 3

if(i==1)
{
  "one"
} else if(i==2)
{
  "two"
} else
{
  i
}

# note that only print works: 
for(i in 1:2)
{
  i
  print(i)
}

while(1==1)
{
  print("Done once!")
  
  break
}


