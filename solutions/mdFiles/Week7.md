# Week 7: Programming Fundamentals 1
Michelle Evans  
`r format(Sys.Date())`  



This document contains the solutions for Data Carpentry for Ecologists (ECOL 8030) week 7: Programming Fundamentals 1.

# 1. Use and Modify

## 1.1

```r
get_mass_from_length_theropoda <- function(length){
  #takes the length of a theropoda (cool dinosaur) and returns its mass based on an allometric approach
  #' @param length length of the dinosaur in meters
  #' @return mass of the dinosaur in kg
  mass <- 0.73 * length ** 3.63
  return(mass)
}
```

## 1.2 

```r
get_mass_from_length_theropoda(length=16)
```

```
## [1] 17150.56
```

## 1.3

```r
get_mass_from_length <- function(length, a,b){
    #takes the length of any dinosaur and returns its mass based on an allometric approach (mass= a * length ^ b)
  #' @param length length of the dinosaur in meters
  #' @a scaling of allometric equation
  #' @b exponent in allometric equation
  #' @return mass of the dinosaur in kg
  mass <- a * length ** b
  return(mass)
}

get_mass_from_length(length=26, a = 214.44, b = 1.46)
```

```
## [1] 24955.54
```

# 2. Writing Functions


```r
poundsToGrams <- function(lbs){
  #' converts pounds to grams
  #' @param lbs pounds
  #' @return weight in grams
  grams <- 453.592 * lbs
  return(grams)
}

round(poundsToGrams(3.75)) #round the conversion
```

```
## [1] 1701
```

# 3. Nested Functions


```r
#create conversion function
kgToLB <- function(kgs){ 
  #' converts kg to lb
  #' @param kgs kilograms
  #' @return weight in pounds
  pounds <- 2.205 * kgs
  return(pounds)
}

#nest dino function inside conversion
kgToLB(kgs=get_mass_from_length(length=12, a=10.95, b=2.64)) 
```

```
## [1] 17055.37
```

# 4. Choice Operators


```r
w <- 10.2
x <- 1.3
y <- 2.8
z <- 17.5
dna1 <- "attattaggaccaca"
dna2 <- "attattaggaacaca"
```

1.  w is greater than 10


```r
w>10
```

```
## [1] TRUE
```


2.  w + x is less than 15  


```r
(w+x) < 15
```

```
## [1] TRUE
```

3.  x is greater than y


```r
x>y
```

```
## [1] FALSE
```

4.  2 * x + 0.2 is equal to y


```r
2*x +0.2 ==y 
```

```
## [1] FALSE
```

5.  dna1 is the same as dna2


```r
dna1 == dna2
```

```
## [1] FALSE
```

6.  dna1 is **not** the same as dna2


```r
dna1 != dna2 #note the use of !=
```

```
## [1] TRUE
```

7.  The number of occurrences of the base t is the same in dna1 and dna2


```r
library(stringr) #load stringr package
str_count(dna1, pattern="t") == str_count(dna2, pattern="t") 
```

```
## [1] TRUE
```

8.  w is greater than x, and y is greater than z


```r
(w>x & y>z)
```

```
## [1] FALSE
```

9.  x times w is between 13.2 and 13.5


```r
(x*w>13.2 & x*w<13.5)
```

```
## [1] TRUE
```

10. dna1 is longer than 5 bases, or z is less than w * x


```r
(str_length(dna1)>5 | z<w*x) #use str_length to get length of dna1
```

```
## [1] TRUE
```

11. The combined length of dna1 and dna2 is greater than or equal to 30


```r
str_length(dna1) + str_length(dna2) >=30
```

```
## [1] TRUE
```

12. (w + x + y) divided by the logarithm (base 10) of 100 is equal to 7.15


```r
(w+x+y)/log10 (100) ==7.15
```

```
## [1] TRUE
```

13. The GC content (which is always a percentage) of dna1 is not the same as the GC content of dna2


```r
(str_count(dna1, "g")+str_count(dna1,"c"))/str_length(dna1) != (str_count(dna2, "g")+str_count(dna2,"c"))/str_length(dna2)
```

```
## [1] TRUE
```

#5. Complete the Code

Filled in with improved documentation:

```r
near <- function(lat1, long1, lat2, long2){
  #' Check proximity of two points
  #' This functon checks if two geographic points are near to each other. It returns TRUE if the absolute value of the difference in their latitudes is less than one and the absolute value of the difference in their longitudes is less than one
  #' @param lat1 latitude of point 1
  #' @param lat2 latitude of point 2
  #' @param long1 longitude of point 1
  #' @param long2 longitude of point 2
  #' @return TRUE if near, FALSE if not
    # Check if two geographic points are near each other 
    if ((abs(lat1 - lat2) < 1) & (abs(long1 - long2) < 1)){
        near <- TRUE
    } else {
        near <- FALSE
    }
    return(near)
}
```

## 5.3. Check if Point 1 (latitude = 29.65, longitude = -82.33) is near Point 2 (latitude = 41.74, longitude = -111.83).


```r
near(lat1=29.65, long1=-82.33, lat2=41.74, long2=-111.83)
```

```
## [1] FALSE
```

## 5.4. Check if Point 1 (latitude = 29.65, longitude = -82.33) is near Point 2 (latitude = 30.5, longitude = -82.8).


```r
near(lat1=29.65, long1=-82.33, lat2=30.5, long2=-82.8)
```

```
## [1] TRUE
```

## 5.5 & 5.6

Create a new version of the function that improves it by allowing the user to pass in a parameter that sets what “near” means. To avoid changing the existing behavior of the function (since some of your lab mates are using it already) give the parameter a default value of 1. Adjust the documentation.


```r
nearChoice <- function(lat1, long1, lat2, long2, distance=1){
    #' Check proximity of two points
  #' This function checks if two geographic points are near to each other. It returns TRUE if the absolute value of the difference in their latitudes and the absolute value of the difference in their longitudes is less than the supplied distance
  #' @param lat1 latitude of point 1
  #' @param lat2 latitude of point 2
  #' @param long1 longitude of point 1
  #' @param long2 longitude of point 2
  #' @param distance user inputed threshold for nearness, default is 1
  #' @return TRUE if near, FALSE if not
  
    if ((abs(lat1 - lat2) < distance) & (abs(long1 - long2) < distance)){
        near <- TRUE
    } else {
        near <- FALSE
    }
    return(near)
}
```

## 5.7. Check if Point 1 (latitude = 48.86, longitude = 2.35) is near Point 2 (latitude = 41.89, longitude = 2.5), when near is set to 7.


```r
nearChoice(lat1=48.86, long1=2.35, lat2=41.89, long2=2.5, distance=7)
```

```
## [1] TRUE
```

# 6. Function with Choices

Write a function that concatenates and prints:

'The ultimate answer to the ultimate question of life, the universe, and everything is: XXX.'


```r
galaxy <- function(XXX=42){
  #' Ultimate answer
  #' This function prints out the phrase, ONLY IF the input is 42 (I think that's what the exercise means?)
  #' @param XXX the string or numeric input, default is 42
  #' @return The phrase, or nothing
  if(XXX==42){
    return(paste0("The ultimate answer to the ultimate question of life, the universe, and everything is: ", XXX))
  } else return("")
}
```


```r
print(galaxy(XXX=2))
```

```
## [1] ""
```

```r
print(galaxy(XXX=42))
```

```
## [1] "The ultimate answer to the ultimate question of life, the universe, and everything is: 42"
```

## Cool Thing I learned this week

You may have noticed the `@param` and `@returns` in the decription of functions. This is the documentation used by `roxygen2` to create those manual pages in the help files.  For your own functions, this may seem like overkill, but if you ever decide to combine them all into a package, it will save you a lot of time.  I also like how it colors things in markdown to make it easier to read.

Find out more on this [help page](http://r-pkgs.had.co.nz/man.html).
