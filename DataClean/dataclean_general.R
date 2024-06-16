#general data cleaning 
#cmd shift R makes new section, cmd shift m for pipe

# #subset by row  ---------------------------------------------------------
filtered<-filter(dataset,column=="category of one variable")
filtered<-filter(dataset,column > 10000)
filtered<-filter(dataset,column > 10000 | column=="category of one variable")
#can filter using is.na or !is.na

# #subset by column  ------------------------------------------------------
subsetted<-select(dataset, column1,column2)
subsetted<-select(dataset, 1:2) #same thing columns 1 and 2
subsetted<-select(dataset,starts_with("ex")) #can do contains() or ends_with() 
subsetted<-select(dataset,-column1) #column1 here is the variable name
subsetted<-dataset %>%
  select(-column1) %>% #can just pipe instead cmd shift m


# #reorder row ------------------------------------------------------------
arranged<- dataset %>% arrange(variable1,variable2) #defaults to alphaebetical
arranged<- dataset %>% arrange(desc(variable1,variable2)) #change to descending


# #add/modify columns -----------------------------------------------------
modified<-dataset %>% mutate(newdatacolumn = 0.5 * existingcolumn) #multiply by whatever
modified<-dataset %>% mutate(newdatacolumn = existingcolumn1 / existingcolumn2)


# other stuff -------------------------------------------------------------
#slice() index rows by integer locations you can slice_min() and slice_max()
#bind() to join datasets together
#rename() rename columns

#summaries
#group_by()
#summarise()


#grepl returns boolean for search
object1<-'example text'
grepl('term to search for',object1)
#gives you the index for grepl
example<-c('a','b','c','c','d','e','f','g','2')
grep('c',example)
nchar() #length of string
gsub() #replacement for pattern matching 
gsub('pattern','replacement','hello have you seen the pattern here?')



