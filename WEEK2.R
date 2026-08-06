# Addition with data vectors

c(2,3,5,7)+c(-2,-3,-5,8)

c(2,3,5,7) + c(8,9)

c(2,3,5,7) + c(8,9,10)


#Subtraction with vctors

c(2,3,5,7) - c(-2,-3,-5, 8)

c(12,13, 15, 17) - c(8,9)

c(12, 13, 15, 17) - c(8, 9, 10)


#Multiplication with data vectors

c(2, 3, 5, 7)* c(-2, -3, -5, 8)

c(2, 3, 5, 7) * c(8, 9)

c(2, 3, 5, 7) * c(8, 9, 7)


#Division with data vectors

c(24, 20, 8, 16)/c(3, 4, 2, 8)

c(24, 20, 8, 16)/c(4, 2)

c(24, 20, 8, 16)/c(4, 2, 8)


#Assignment operator

#1. Assignment of numbers and characters
x<-20
x

x = 20
x

y = x*2
y

z=x+y
z

x<- apple
x

x<- "apple"
x

x<- 'apple'
x


#Knowing numbers and characters

x= 20
is.numeric(x)

is.character(x)

y = as.character(x)
is.numeric(y)

is.character(y)

y


#Converting numbers and characters

y = "apple"

is.numeric(y)

is.character(y)

z= as.numeric(y)
z

is.numeric(z)

is.character(z)


x<- 20
x

y = 1, 2, 3, 4, 5

y= (1,2,3,4,5)

y= c(1,2,3,4,5)
y


#Mode
x=6
x

mode(x)

y="apple"

mode(y)

storage.mode(x)

x= TRUE
storage.mode(x)


#Infinity

3/0

5+Inf

x= 5+Inf
is.finite(x)

is.infinite(x)


#R as a calculator

2+3

2*3

2-3

3/2

2*3-4+5/6


#Addition with Scalar

c(2,3,5,7) +10


# Subtraction with scalar

c(12, 13 , 14, 15)- 10

#Multiplication with scalar

c(2,3,4,6)*10

#Division with scalar

c(12,13,15,17)/10


























