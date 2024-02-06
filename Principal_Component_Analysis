library(FactoMineR)
library(factoextra)
library(magrittr)
library(dplyr)  
library(tidyverse)

#check that data frame is tidy and in proper format
groups <- as.factor(data$group)
view(data)
data$group <- rownames(data)
view(data)


#the res.pca generation from prcomp cannot include variable names (must be numeric) so you need to remove the first column
#i named it data1 (without variable names) vs data so i can plot on top with the variable names
data1 <- data[ -c(1) ]
view(data1)
res.pca <- prcomp(data1, scale = TRUE)

#fviz_pca_ind(res.pca)
#fviz_pca_ind(res.pca, label="none", habillage=data$group)

view(res.ind) #see individual values as dataframe

res.pca <- prcomp(df[, -1],  scale = TRUE)
fviz_pca_ind(res.pca)
fviz_pca_ind(res.pca) +
  labs(title ="PCA", x = "PC1", y = "PC2")
fviz_pca_ind(res.pca, label="none", habillage=df$group)
p <- fviz_pca_ind(res.pca, label="none", habillage=df$group,
                  addEllipses=TRUE, ellipse.level=0.95)
p+labs(title ="PCA", x = "PC1", y = "PC2")
p<-p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p


#other plot generation
fviz_pca_ind(res.pca,
             col.ind = group, # color by groups
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)


#check values for dimensions, variables 
eig.val <- get_eigenvalue(res.pca)
eig.val
res.var <- get_pca_var(res.pca)
res.var$contrib 

quanti.sup <- data[1:177, 2:3, drop = FALSE]
view(quanti.sup)
quanti.coord <- cor(quanti.sup, res.pca$x)
quanti.cos2 <- quanti.coord^2
# Graph of variables including supplementary variables
p <- fviz_pca_var(res.pca)
fviz_add(p, quanti.coord, color ="blue", geom="arrow")


var_coord_func <- function(loadings, comp.sdev){
  loadings*comp.sdev
}
# Compute Coordinates
#::::::::::::::::::::::::::::::::::::::::
loadings <- res.pca$rotation
sdev <- res.pca$sdev
var.coord <- t(apply(loadings, 1, var_coord_func, sdev)) 
head(var.coord[, 1:2])

var.cos2 <- var.coord^2
head(var.cos2[, 1:2])

comp.cos2 <- apply(var.cos2, 2, sum)
contrib <- function(var.cos2, comp.cos2){var.cos2*100/comp.cos2}
var.contrib <- t(apply(var.cos2,1, contrib, comp.cos2))
head(var.contrib[, 1:2])

ind.coord <- res.pca$x
head(ind.coord[, 1:2])


contrib <- function(ind.coord, comp.sdev, n.ind){
  100*(1/n.ind)*ind.coord^2/comp.sdev^2
}
ind.contrib <- t(apply(ind.coord, 1, contrib, 
                       res.pca$sdev, nrow(ind.coord)))
head(ind.contrib[, 1:2])


view(data)
groups <- as.factor(data$group)
library(magrittr) # for pipe %>%
library(dplyr)   # everything else
# 1. Individual coordinates
res.ind <- get_pca_ind(res.pca)
# 2. Coordinate of groups
coord.groups <- res.ind$coord %>%
  as_data_frame() %>%
  select(Dim.1, Dim.2) %>%
  mutate(group = groups) %>%
  group_by(group) %>%
  summarise(
    Dim.1 = mean(Dim.1),
    Dim.2 = mean(Dim.2)
  )


#this gives all the individual results displayed in fviz_pca_ind or _Var but for 
#alternate plotting purposes, below is histogram generation  
view(coord.groups)
write.csv(coord.groups, "PCAresults.csv")
view(res.ind$coord)
coord.groups <- res.ind$coord %>%
  as_data_frame() %>%
  select(Dim.1, Dim.2) %>%
  mutate(group = groups) %>%
  group_by(group) %>%
  reframe(
    Dim.1 = Dim.1,
    Dim.2 = Dim.2
  )
view(coord.groups)


# Change histogram plot fill colors by groups
ggplot(df, aes(x=weight, fill=sex, color=sex)) +
  geom_histogram(position="identity")
# Use semi-transparent fill
p<-ggplot(coord.groups, aes(x=Dim.1, fill=group, color=group)) +
  geom_histogram(alpha=0.05, binwidth=0.1) + #draws a traditional histo with bins 
  geom_density(lwd = 1,linetype = 1, colour = 1, alpha=0.5) + #draws the density histogram +
  ylim(0,0.75) + 
  xlim(-3.5,3.5)
p<-p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p

