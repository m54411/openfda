## ------------------------------------------------------------------------
# install ggplot2
if (is.null(grep('ggplot2', rownames(installed.packages())))) {
  install.packages("ggplot2")
}
# install ggpubr
if (is.null(grep('ggpubr', rownames(installed.packages())))) {
  install.packages("ggpubr")
}
# install ggsci
if (is.null(grep('ggsci', rownames(installed.packages())))) {
  install.packages("ggsci")
}
# install devtools
if (is.null(grep('devtools', rownames(installed.packages())))) {
  install.packages("devtools")
}
# install pheatmap
if (is.null(grep('pheatmap', rownames(installed.packages())))) {
  install.packages("pheatmap")
}
# install openfda
if (is.null(grep('openfda', rownames(installed.packages())))) {
  devtools::install_github("ropenhealth/openfda")
}

## ---- message=FALSE, warning=FALSE---------------------------------------
library(openfda)
countries = fda_query("/drug/event.json") %>%
  fda_count("primarysourcecountry") %>%
  fda_limit(1000) %>%
  fda_exec()

head(countries)

nrow(countries)

## ---- message=FALSE, warning=FALSE---------------------------------------
library(ggplot2)
library(ggpubr)
library(ggsci)

g<-ggbarplot(data=countries[1:10,],x='term', y='count', fill='term', palette = pal_d3("category10")(10), title = 'Number of adverse drug reports by country' , xlab = 'Country', ylab = 'Number of reports')
g <- g + guides(fill = guide_legend(title='Country'))
g


## ------------------------------------------------------------------------
aspirin_countries<- fda_query("/drug/event.json") %>%
fda_filter("patient.drug.openfda.generic_name", "aspirin") %>%
fda_count("primarysourcecountry") %>%
fda_limit(1000) %>%
fda_exec()

g<-ggbarplot(data=aspirin_countries[1:10,],x='term', y='count', fill='term', palette = pal_d3("category10")(10), title = 'Number of Aspirin-related adverse drug reports by country' , xlab = 'Country', ylab = 'Number of reports')
g <- g + guides(fill = guide_legend(title='Country'))
g


## ---- fig.height=15, message=FALSE, warning=FALSE------------------------
for(i in seq(10)){
  tmp<- fda_query("/drug/event.json") %>%
    fda_filter("primarysourcecountry", as.character(aspirin_countries$term[i])) %>%
    fda_filter("patient.drug.openfda.generic_name", "aspirin") %>%
    fda_count("patient.reaction.reactionmeddrapt.exact") %>%
    fda_limit(1000) %>%
    fda_exec()
    tmp$count <- tmp$count / aspirin_countries$count[i]
  if(i==1){
    aspirin_countries_reactions  <- cbind(data.frame(country=rep(aspirin_countries$term[i],nrow(tmp))), tmp)
  } else {
    tmp  <- cbind(data.frame(country=rep(aspirin_countries$term[i],nrow(tmp))), tmp)
    aspirin_countries_reactions <- rbind(aspirin_countries_reactions, tmp)
  }
}

reaction_mx <- matrix(0, nrow = 10, ncol=length(unique(aspirin_countries_reactions$term)))

allReactions <- unique(aspirin_countries_reactions$term)
allCountries <- aspirin_countries$term[1:10]

rownames(reaction_mx) <- allCountries
colnames(reaction_mx) <- allReactions

for(i in seq(allCountries)){
  for(j in seq(allReactions)){
    idx <- which(aspirin_countries_reactions[which(aspirin_countries_reactions$country==allCountries[i]),]$term==allReactions[j])
    if(length(idx)!=0){
      reaction_mx[i,j] <- aspirin_countries_reactions[which(aspirin_countries_reactions$country==allCountries[i])[idx],3]
    }  
  }
}

library(pheatmap)
pheatmap(t(reaction_mx[,which(apply(reaction_mx , 2, min)>0)]), scale = 'none', cellheight = 10, cellwidth=20, fontsize = 6)

