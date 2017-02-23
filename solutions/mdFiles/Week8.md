# Week 8: Programming Fundamentals 2
Michelle Evans  
`r format(Sys.Date())`  



This document contains the solutions for Data Carpentry for Ecologists (ECOL 8030) week 8: Programming Fundamentals 2.

#1. for Loop

For this, you need a function from last week.


```r
get_mass_from_length <- function(length, a,b){
  #' allometric mass equation
  #' takes the length of any dinosaur and returns its mass based on an allometric         approach (mass= a * length ^ b)
  #' @param length length of the dinosaur in meters
  #' @param a scaling of allometric equation
  #' @param b exponent in allometric equation
  #' @return mass of the dinosaur in kg
  mass <- a * length ** b
  return(mass)
}
```

Now we can run a for-loop over the lengths


```r
lengths <- c(10.1, 9.5, 11.2, 9.8, 10.4, 12.0, 11.5, 9.5, 9.8, 10.0, 10.7, 10.2, 11.9, 9.7, 11.2, 11.8, 10.7)

for (i in lengths){
 mass <- get_mass_from_length(length=i, a=10.95, b=2.64)
 print(mass)
}
```

```
## [1] 4907.073
## [1] 4174.5
## [1] 6446.882
## [1] 4531.594
## [1] 5301.297
## [1] 7734.863
## [1] 6912.839
## [1] 4174.5
## [1] 4531.594
## [1] 4779.848
## [1] 5714.619
## [1] 5036.381
## [1] 7565.856
## [1] 4410.537
## [1] 6446.882
## [1] 7399.163
## [1] 5714.619
```

Or just try applying the function to a vector:


```r
get_mass_from_length(length=lengths, a=10.95, b=2.64)
```

```
##  [1] 4907.073 4174.500 6446.882 4531.594 5301.297 7734.863 6912.839
##  [8] 4174.500 4531.594 4779.848 5714.619 5036.381 7565.856 4410.537
## [15] 6446.882 7399.163 5714.619
```

They are the same!

#2. stringr

Read the csv:


```r
sequences <- read.csv("https://mvevans89.github.io/ECOL-8030/data/dna-sequences-1.txt", stringsAsFactors = F) #note we need the strings to not be factors
```

This code was sent by a colleague:


```r
library(stringr)

sequence <- "attggc"
Gs <- str_count(sequence, "g")
Cs <- str_count(sequence, "c")
gc_content <- (Gs + Cs) / str_length(sequence) * 100 
```

## 2.1 Write a function to calculate the GC content of a dna sequence


```r
library(stringr) #load package

#' Calculate GC content of a sequence
#' @param sequence dna sequence
#' @return the percentage of g and c in the sequence
getGCcontent <- function(sequence){
  Gs <- str_count(sequence, "g")
  Cs <- str_count(sequence, "c")
  gc_content <- (Gs + Cs) / str_length(sequence) * 100 
  return(gc_content)
}
```

## 2.2 Use a for loop to get GC content of multiple sequences

We also need to store the results in a new vector.


```r
content <- c() #create empty object to write too. not 100% sure this is necessary

for (i in 1:nrow(sequences)){ #iterate over each row
  seq <- sequences[i,1]  # select out the sequence to use in this for loop
  content[i]<- getGCcontent(seq) #write to the appropriate spot in the empty object
}
```

## 2.3 Save results of a for loop in a dataframe

To do this, we just fill in the code that was provided:


```r
# pre-allocate the memory with one row for each sequence
gc_contents <- data.frame(gc_content = numeric(nrow(sequences)))

# loop over sequences using an index for the row and
# store the output in gc_contents
for (i in 1:nrow(sequences)){
  gc_contents[i,] <- getGCcontent(sequences[i,1])
}
```


# 3. DNA or RNA

Write a function, dna_or_rna(sequence), that determines if a sequence of base pairs is DNA, RNA, or if it is not possible to tell given the sequence provided. Since all the function will know about the material is the sequence the only way to tell the difference between DNA and RNA is that RNA has the base Uracil ("u") instead of the base Thymine ("t"). Have the function return one of three outputs: "DNA", "RNA", or "UNKNOWN".


```r
#' DNA or RNA
#' This function determines if a sequences is DNA or RNA based on u or t
#' @param sequence sequence you want to test, must be in lowercase
#' @return "DNA", "RNA", or "UNKNOWN"
dna_or_rna <- function(sequence){
  if (str_count(sequence, "t")>0){ #use str_count to count number of t's
    return("DNA")
  } else if (str_count(sequence, "u")>0){
    return("RNA")
  } else return("UNKNOWN") #returns unknown if neither t nor u are there
}
```

We can also make it so that it works even if the sequences are reported in uppercase.


```r
#' DNA or RNA
#' This function determines if a sequences is DNA or RNA based on u or t
#' @param sequence sequence you want to test, accepts all cases
#' @return "DNA", "RNA", or "UNKNOWN"
dna_or_rna <- function(sequence){
  if (str_count(tolower(sequence), "t")>0){ #tolower switches strings to lowercase
    return("DNA")
  } else if (str_count(tolower(sequence), "u")>0){
    return("RNA")
  } else return("UNKNOWN")
}
```

## 3.1 Use a for loop


```r
sequences = c("ttgaatgccttacaactgatcattacacaggcggcatgaagcaaaaatatactgtgaaccaatgcagcg", "gauuauuccccacaaagggagugggauuaggagcugcaucauuuacaagagcagaauguuucaaaugcau","gaaagcaagaaaaggcaggcgaggaagggaagaagggggggaaacc", "guuuccuacaguauuugaugagaaugagaguuuacuccuggaagauaauauuagaauguuuacaacugcaccugaucagguggauaaggaagaugaagacu", "gauaaggaagaugaagacuuucaggaaucuaauaaaaugcacuccaugaauggauucauguaugggaaucagccggguc")

for (sq in sequences){
  print(dna_or_rna(sq))
}
```

```
## [1] "DNA"
## [1] "RNA"
## [1] "UNKNOWN"
## [1] "RNA"
## [1] "RNA"
```

## 3.2 Use an `sapply` statement


```r
sapply(sequences, dna_or_rna)
```

```
##                                 ttgaatgccttacaactgatcattacacaggcggcatgaagcaaaaatatactgtgaaccaatgcagcg 
##                                                                                                 "DNA" 
##                                gauuauuccccacaaagggagugggauuaggagcugcaucauuuacaagagcagaauguuucaaaugcau 
##                                                                                                 "RNA" 
##                                                        gaaagcaagaaaaggcaggcgaggaagggaagaagggggggaaacc 
##                                                                                             "UNKNOWN" 
## guuuccuacaguauuugaugagaaugagaguuuacuccuggaagauaauauuagaauguuuacaacugcaccugaucagguggauaaggaagaugaagacu 
##                                                                                                 "RNA" 
##                       gauaaggaagaugaagacuuucaggaaucuaauaaaaugcacuccaugaauggauucauguaugggaaucagccggguc 
##                                                                                                 "RNA"
```

#4. Multiple Files

First, check out the [wikipedia page on archaea](https://en.wikipedia.org/wiki/Archaea) because wow are they cool. You'll need this [data](https://mvevans89.github.io/ECOL-8030/data/archaea-dna.zip) for the exercise.

Then install the necessary software (this can take a while):


```r
source("https://bioconductor.org/biocLite.R")
biocLite("ShortRead")
```

Why is this different than normally installing packages? Bioconductor, from what I can tell, is a collection of Biostats tools and packages for R. The first line of the code above is the function that runs the download of a package called `ShortRead`. This package reads `.fasta` files, which I guess are some kind of genetics thing??


```r
library(ShortRead)
```

```
## Loading required package: BiocGenerics
```

```
## Loading required package: parallel
```

```
## 
## Attaching package: 'BiocGenerics'
```

```
## The following objects are masked from 'package:parallel':
## 
##     clusterApply, clusterApplyLB, clusterCall, clusterEvalQ,
##     clusterExport, clusterMap, parApply, parCapply, parLapply,
##     parLapplyLB, parRapply, parSapply, parSapplyLB
```

```
## The following objects are masked from 'package:stats':
## 
##     IQR, mad, xtabs
```

```
## The following objects are masked from 'package:base':
## 
##     anyDuplicated, append, as.data.frame, cbind, colnames,
##     do.call, duplicated, eval, evalq, Filter, Find, get, grep,
##     grepl, intersect, is.unsorted, lapply, lengths, Map, mapply,
##     match, mget, order, paste, pmax, pmax.int, pmin, pmin.int,
##     Position, rank, rbind, Reduce, rownames, sapply, setdiff,
##     sort, table, tapply, union, unique, unsplit, which, which.max,
##     which.min
```

```
## Loading required package: BiocParallel
```

```
## Loading required package: Biostrings
```

```
## Loading required package: S4Vectors
```

```
## Loading required package: stats4
```

```
## 
## Attaching package: 'S4Vectors'
```

```
## The following objects are masked from 'package:base':
## 
##     colMeans, colSums, expand.grid, rowMeans, rowSums
```

```
## Loading required package: IRanges
```

```
## Loading required package: XVector
```

```
## Loading required package: Rsamtools
```

```
## Loading required package: GenomeInfoDb
```

```
## Loading required package: GenomicRanges
```

```
## Loading required package: GenomicAlignments
```

```
## Loading required package: SummarizedExperiment
```

```
## Loading required package: Biobase
```

```
## Welcome to Bioconductor
## 
##     Vignettes contain introductory material; view with
##     'browseVignettes()'. To cite Bioconductor, see
##     'citation("Biobase")', and for packages 'citation("pkgname")'.
```

```r
reads <- readFasta("data/archaea-dna/A-saccharovorans.fasta")
seq <- sread(reads)

#if you're curious about what this is, just str it
str(seq)
```

```
## Formal class 'DNAStringSet' [package "Biostrings"] with 5 slots
##   ..@ pool           :Formal class 'SharedRaw_Pool' [package "XVector"] with 2 slots
##   .. .. ..@ xp_list                    :List of 1
##   .. .. .. ..$ :<externalptr> 
##   .. .. ..@ .link_to_cached_object_list:List of 1
##   .. .. .. ..$ :<environment: 0x579d7a0> 
##   ..@ ranges         :Formal class 'GroupedIRanges' [package "XVector"] with 7 slots
##   .. .. ..@ group          : int 1
##   .. .. ..@ start          : int 1
##   .. .. ..@ width          : int 1496453
##   .. .. ..@ NAMES          : NULL
##   .. .. ..@ elementType    : chr "integer"
##   .. .. ..@ elementMetadata: NULL
##   .. .. ..@ metadata       : list()
##   ..@ elementType    : chr "DNAString"
##   ..@ elementMetadata: NULL
##   ..@ metadata       : list()
```

```r
head(seq)
```

```
##   A DNAStringSet instance of length 1
##       width seq
## [1] 1496453 GTTTCAACACCATTCCTTGGTTTCCCATTG...CACAGGCCTCCCCTGAGGCCCAGACATCCA
```

We need to modify our above gccontent function to take both upper and lower case.


```r
#' Calculate GC content of a sequence
#' @param sequence dna sequence
#' @return the percentage of g and c in the sequence
getGCcontent <- function(sequence){
  Gs <- str_count(tolower(sequence), "g")
  Cs <- str_count(tolower(sequence), "c")
  gc_content <- (Gs + Cs) / str_length(sequence) * 100 
  return(gc_content)
}
```

Now we will write a for loop to get this for all of the files in that zip file. 


```r
#get list of files in directory. we need the full names so we can load them later
files <- list.files(path="data/archaea-dna", full.names=T) 
#get only the file names for putting species in later
filenames <- list.files(path="data/archaea-dna", full.names=F)

#create empty dataframe
archaeaData <- data.frame(Species=character(), gcContent=numeric(), stringsAsFactors = F)

for (i in 1:length(files)){
  #read in fasta files and format them as above
  reads <- readFasta(files[i])
  seq <- sread(reads)
  #get species name using gsub
  archaeaData[i,1] <- gsub(".fasta", "", filenames[i]) #use gsub to replace .fasta with nothing
  archaeaData[i,2] <- getGCcontent(seq)
}
```

The way I went about getting the shorter filenames was not the best.  Ideally, one would use a regex expression like `grep` to select a certain subset of the character string, but regex confuses me a lot. If you're interested in regex, this [website](http://regexr.com/) allows you to look at your regex commands interactively.

# 5. Data Management Review

![](http://img.memecdn.com/dobby_o_298964.jpg)


## 5.1 Import the Data


```r
ears <- read.csv("https://mvevans89.github.io/ECOL-8030/data/houseelf-earlength-dna-data.csv")
```

## 5.2 Determine if large or small and GC content

I chose to do this using `rowwise()` in `dplyr`. This seems the most efficient method, but may be confusign because it has a lot of moving parts.


```r
#create a function that sorts ears with an if-else statement

#' Ear Sorting
#' This function identifies ears as large (>10cm) or small (<= 10cm)
#' @param length the length of an elf's ears in cm
#' @return if the ear is large or small
earSort <- function(length){
  if (length>10){
    return("large")
  } else return("small")
}

library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following object is masked from 'package:ShortRead':
## 
##     id
```

```
## The following objects are masked from 'package:GenomicAlignments':
## 
##     first, last
```

```
## The following object is masked from 'package:Biobase':
## 
##     combine
```

```
## The following objects are masked from 'package:GenomicRanges':
## 
##     intersect, setdiff, union
```

```
## The following object is masked from 'package:GenomeInfoDb':
## 
##     intersect
```

```
## The following objects are masked from 'package:Biostrings':
## 
##     collapse, intersect, setdiff, setequal, union
```

```
## The following object is masked from 'package:XVector':
## 
##     slice
```

```
## The following objects are masked from 'package:IRanges':
## 
##     collapse, desc, intersect, regroup, setdiff, slice, union
```

```
## The following objects are masked from 'package:S4Vectors':
## 
##     first, intersect, rename, setdiff, setequal, union
```

```
## The following objects are masked from 'package:BiocGenerics':
## 
##     combine, intersect, setdiff, union
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
earsProcessed <- ears %>%
  rowwise() %>% #apply this function by row
  mutate(size=earSort(earlength)) %>% #add a new column sorting using the earSort function
  mutate(GC=getGCcontent(dnaseq)) %>% #create column of GC content 
  select(-earlength, - dnaseq)

earsProcessed
```

```
## Source: local data frame [10 x 3]
## Groups: <by row>
## 
## # A tibble: 10 × 3
##        id  size    GC
##    <fctr> <chr> <dbl>
## 1     17A small    41
## 2     24P small    39
## 3     09Q large    57
## 4     65Y small    40
## 5     19N small    36
## 6     92K large    62
## 7     33W small    52
## 8     98C large    63
## 9     75G small    47
## 10    88Q large    52
```

## 5.3 Save to csv and print means


```r
write.csv(earsProcessed, "grangers_analysis.csv")
```


```r
means <- earsProcessed %>%
  group_by(size) %>%
  summarise(meanGC=mean(GC))
```

```
## Warning: Grouping rowwise data frame strips rowwise nature
```

```r
means
```

```
## # A tibble: 2 × 2
##    size meanGC
##   <chr>  <dbl>
## 1 large   58.5
## 2 small   42.5
```


# Cool Thing I learned this week

The most useful thing I learned this week was the `rowwise()` call to `dplyr`, which I hadn't known before. Often I want to add a column that is conditional on values in another and have struggled about the workflow to go about this. Creating a conditional function, and then using that rowwise inside `dplyr` seems like an easy and straightforward way to do this, and fits will with my workflow already using `dplyr`.
