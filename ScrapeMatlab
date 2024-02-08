# get all  files with extension "txt" in the current directory
file.list <- list.files(path = ".", pattern="*.txt", full.names=TRUE, recursive=TRUE)

# this creates a vector where each element contains one file, sapply() behaves like a while loop 
all.files <- sapply(file.list, FUN = function(x)readChar(x, file.info(x)$size))

# create a dataframe
df <- data.frame( files= all.files, stringsAsFactors=FALSE)
head(df)
library("writexl")
write_xlsx(df,"/Users/~filepath/filename.xlsx")

file.list <- list.files(path = ".", pattern="*.csv", full.names=TRUE, recursive=TRUE)
library(readxl)
# this creates a vector where each element contains one file
all.files <- sapply(file.list, FUN = function(x)readChar(x, file.info(x)$size))
# create a dataframe
df <- data.frame(files= all.files, stringsAsFactors=FALSE)
head(df)
write_xlsx(df,"/Users/~filepath.xlsx")

files <- list.files('.', pattern = '.xlsx', recursive = TRUE)
df1 <- lapply(files, 
              function(x) {readxl::read_xlsx(x, sheet = 1, range="B28:B28" , col_names = FALSE)}) %>% 
  bind_cols %>% 
  t %>% 
  data.frame


data$group<-gsub('[[:digit:]]+', '',data$group)
data$group<- gsub('[0-9.]', '', data$group) #removes numbers from the group names
data$group <- gsub('-', '', data$group)
