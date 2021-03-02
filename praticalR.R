sink("practicalRoutput.txt")
#2. Write a program that prints 'Hello World' to the screen.
print(paste("Hello World"),quote=FALSE)

#3. Write a program that asks the user for a number n and prints the sum of the numbers 1 to n .
n<-readline(prompt = " Enter the number ")
n<-as.integer(n)
sum_n<-(n*(n+1))/2
print(paste(" sum upto ",n," is ",sum_n),quote = FALSE)

#4. Write a program that prints a multiplication table for numbers up to 12.
for(i in 1:12)
  for(j in 1:10)
    print(paste(i,"*",j,"=",i*j),quote = FALSE)

#5. Write a function that returns the largest element in a list.
maximum_of_list<-function(){
  print("Enter the list , press 2 times enter to stop ")
  x<-scan()
  return(max(x))
}
maximum_of_list()

#6. Write a function that computes the running total of a list.
running_time<-function(){
  print("Enter the list , press 2 times enter to stop ")
  x<-scan()
  return(cumsum(x))
  }
running_time()

#7. Write a function that tests whether a string is a palindrome.

diss1_palindrome<-function(){
  str<-readline(prompt = "enter the string ")
  str<-strsplit(str,NULL)
  return(TRUE&&(rev(str[[1]])==str[[1]]))
}

diss1_palindrome()

diss2_palindrome<-function(){
  str<-readline(prompt = "enter the string ")
  str<-strsplit(str,NULL)
  return(TRUE&&(str[[1]][length(str[[1]]):1]==str[[1]]))
}

diss2_palindrome()

palindrome<-function(){
  str<-readline(prompt = "Enter the string ")
  return(str==paste(rev(strsplit(str,"")[[1]]),collapse = ''))
}

palindrome()

#8. Implement linear search.
linear_search<-function(){
  print("Enter the array ")
  x<-scan()
  n<-as.numeric(readline(prompt = "Enter the element you want to search "))
  for(i in 1:length(x)){
    if(x[i]==n){
      print(paste(" found at ",i," postion"),quote = FALSE)
      flag<-1
      break
      }
    else{
      flag<-0
      }
  }
  if(flag==0){
    print(paste("element not found "),quote = FALSE)
  }
}

linear_search()

#9. Implement binary search.
binarySearch <- function()
{
  print("Enter the array ")
  arr<-scan()
  item<-as.numeric(readline(prompt = "Enter the element you want to search "))
  pos <- -1
  mid <- 0
  l <- 1
  u <- length(arr)
  
  while(l<=u)
  {
    mid <- as.integer((l+u)/2)
    if(arr[mid]==item)
    {
      pos <- mid
      break
    }
    else if(arr[mid]>item)
    {
      u <- mid-1
    }
    else
    {
      l <- mid+1
    }
  }
  if(pos!=-1)
  {
    print(paste("Element found at",pos),quote = FALSE)
  }
  else
  {
    print("Element not found",quote=FALSE)
  }
}

binarySearch()

#10. Implement matrices addition , subtraction and Multiplication.
matrix_calc<-function(){
  print("FIRST MATRIX")
  n = as.numeric(readline("Enter number of rows : "))
  m = as.numeric(readline("Enter number of cols : "))
  print("Enter values : ")
  A = matrix(scan(),n,m,TRUE)
  
  print("SECOND MATRIX")
  p = as.numeric(readline("Enter number of rows : "))
  q = as.numeric(readline("Enter number of cols : "))
  print("Enter values : ")
  B = matrix(scan(),p,q,TRUE)
  
  print("First Matrix")
  print(A)
  print("Second Matrix")
  print(B)
  
  if(n==p && m==q)
  {
    print("Sum of Matrices")
    print(A+B)
    print("Difference of Matrices")
    print(A-B)
  }
  else
  {
    print("Invalid Dimensions for Sum and Difference of Matrix")
  }
  
  if(m==p)
  {
    print("Product of Matrices")
    print(A %*% B)
  }
  else
  {
    print("Invalid Dimensions for Product of Matrix")
  }

}

matrix_calc()

#11. Fifteen students were enrolled in a course. There ages were:
#  20 20 20 20 20 21 21 21 22 22 22 22 23 23 23
#i. Find the median age of all students under 22 years
#ii. Find the median age of all students
#iii. Find the mean age of all students
#iv. Find the modal age for all students
#v. Two more students enter the class. The age of both students is 23. What is now
#mean, mode and median?

age <- c(rep(20,5),rep(21,3),rep(22,4),rep(23,3))
freq<-table(age)
modeAge <- names(freq)[freq==max(freq)]
cat("median of all the ages under 22 is ",median(age[age<22]))
cat("\nmedian of all the student is ",median(age))
cat("\nmean age of all students is ",mean(age))
cat("\nmodalAge is ",modeAge)
age<-c(age,c(23,23))
cat("\n new ages and their data ",sort(age))
cat("\nnew mean is ",mean(age))
cat("\nnew median is ",median(age))
freq<-table(age)
modeAge <- names(freq)[freq==max(freq)]
cat("\nnew mode is ",modeAge)
