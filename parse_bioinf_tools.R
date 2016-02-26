require(pubmed.mineR)
require(rebi)
require(stringr)
require(elife)
require(wordcloud)
require(plyr)
require(phenotypicForest)
library(sjPlot)
library('rcrossref')
setwd("/Users/akostikova/Downloads/mine")
## get all reference into a table
#dd <- search_publications(query='ISSN:1367-4803') #Bio Oxf
#dd <- search_publications(query='ISSN:1176-9343') #Bio Evo Online
#dd <- search_publications(query='ISSN:1471-2105') #BMC
#dd <- search_publications(query='ISSN:1088-9051') #genome research
dd <- search_publications(query='ISSN:1553-734X') #plos comp bio
abstracts <- readabs("plos_compbio.txt")
#abstracts <- readabs("genome_research.txt")
#abstracts <- readabs("bmc_bioinformatics.txt")
#abstracts <- readabs("bioinformatics_oxford.txt")
#abstracts <- readabs("evol_bioinformatics.txt")
#dd_b <- dd
#dd <- dd_b
#abstracts_b <- abstracts

length(abstracts@PMID)
aa<-data.frame(matrix(nrow=4371,ncol=length(names(dd))))
colnames(aa)<-names(dd)

for (i in 1:length(names(dd))){
  print(names(dd)[i])
  dd[[names(dd)[i]]][sapply(dd[[names(dd)[i]]], is.null)] <- NA
  aa[,i]<-(do.call(rbind, dd[[names(dd)[i]]]))

}


aa$coathors<-str_count(aa$authorString, ',')


############# REPOSITORIES
links <- searchabsL(abstracts, include=c("http://","https://"), exclude=c("github.com",'code.google.com','sourceforge.net',
                                                                          'bitbucket.org','bioconductor.org','r-project.org'))

urls<-matrix(nrow=length(links@PMID),ncol=3)
for(i in 1:length(links@PMID)){
    print(i)
    urls[i,1]<-(links@PMID[i])
    cc<-str_replace_all(links@Abstract[i], "[\r\n]" , "")
    urls[i,2]<- (str_match(cc, "http(s?)://\\S+"))[1]
    urls[i,3]<- "selfhosted"
}

repo <- searchabsL(abstracts, include=c("github.com",'code.google.com','sourceforge.net',
                                        'bitbucket.org','bioconductor.org','r-project.org'))

repo_url<-matrix(nrow=length(repo@PMID),ncol=3)
for(i in 1:length(repo@PMID)){
  print(i)
  repo_url[i,1]<-(repo@PMID[i])
  cc<-str_replace_all(repo@Abstract[i], "[\r\n]" , "")
  
  if (!is.na(str_match(cc, "github")[1])){
    repo_url[i,2]<- (str_match(cc, "http(s?)://\\S*github\\S+"))[1]
    repo_url[i,3]<- "github"
  } else if (!is.na(str_match(cc, "code.google.com")[1])){
    repo_url[i,2]<- (str_match(cc, "http(s?)://\\S*code.google\\S+"))[1]
    repo_url[i,3]<- "codegoogle"
  } else if (!is.na(str_match(cc, "sourceforge.net")[1])){
    repo_url[i,2]<- (str_match(cc, "http(s?)://\\S*sourceforge\\S+"))[1]
    repo_url[i,3]<- "sourceforge"
  } else if (!is.na(str_match(cc, "bitbucket.org")[1])){
    repo_url[i,2]<- (str_match(cc, "http(s?)://\\S*bitbucket\\S+"))[1]
    repo_url[i,3]<- "bitbucket"
  } else if (!is.na(str_match(cc, "bioconductor.org")[1])){
    repo_url[i,2]<- (str_match(cc, "http(s?)://\\S*bioconductor\\S+"))[1]
    repo_url[i,3]<- "bioconductor"
  } else if (!is.na(str_match(cc, "project.org")[1])){
    repo_url[i,2]<- (str_match(cc, "http(s?)://\\S*r-project\\S+"))[1]
    repo_url[i,3]<- "rproject"
  }
  
}

allurls <- rbind(repo_url, urls)

colnames(allurls)<-c("pmid","fullurl","repo")

allurls <- data.frame(allurls)

#################### LICENSES


licenses <- searchabsL(abstracts, include=c("GPL",'LGPL','Creative',
                                        'MIT license','apache','artistic', 'bsd', 'x11',
                                        "GNU", "Mozilla", 
                                        'Academic Free license','Open Software'))

licenses_types<-matrix(nrow=length(licenses@PMID),ncol=3)
for(i in 1:length(licenses@PMID)){
  print(i)
  licenses_types[i,1]<-(licenses@PMID[i])
  cc<-str_replace_all(licenses@Abstract[i], "[\r\n]" , "")
  
  if (!is.na(str_match(cc, regex("(\\s|\\()GPL", ignore_case = TRUE))[1])){
    licenses_types[i,2]<- (str_match(cc, regex("GPL(.){10}", ignore_case = TRUE))[1])
    licenses_types[i,3]<- "GPL"
   } else if (!is.na(str_match(cc, regex("(\\s|\\()LGPL", ignore_case = TRUE))[1])){
     licenses_types[i,2]<- (str_match(cc, regex("LGPL(.){10}", ignore_case = TRUE))[1])
     licenses_types[i,3]<- "LGPL"
  } else if (!is.na(str_match(cc, regex("creative", ignore_case = TRUE))[1])){
    licenses_types[i,2]<- (str_match(cc, regex("creative(.){10}", ignore_case = TRUE))[1])
    licenses_types[i,3]<- "creativecommons"
  } else if (!is.na(str_match(cc, regex("MIT license", ignore_case = TRUE))[1])){
    licenses_types[i,2]<- (str_match(cc, regex("MIT license", ignore_case = TRUE))[1])
    licenses_types[i,3]<- "MIT"
  } else if (!is.na(str_match(cc, regex("apache", ignore_case = TRUE))[1])){
    licenses_types[i,2]<- (str_match(cc, regex("apache(.){10}", ignore_case = TRUE))[1])
    licenses_types[i,3]<- "apache"
   } else if (!is.na(str_match(cc, regex("artistic", ignore_case = TRUE))[1])){
     licenses_types[i,2]<- (str_match(cc, regex("artistic(.){10}", ignore_case = TRUE))[1])
     licenses_types[i,3]<- "artistic"
   } else if (!is.na(str_match(cc, regex("(\\s|\\()BSD", ignore_case = TRUE))[1])){
     licenses_types[i,2]<- (str_match(cc, regex("BSD(.){10}", ignore_case = TRUE))[1])
     licenses_types[i,3]<- "BSD"
   } else if (!is.na(str_match(cc, regex("GNU", ignore_case = TRUE))[1])){
     licenses_types[i,2]<- (str_match(cc, regex("GNU(.){10}", ignore_case = TRUE))[1])
     licenses_types[i,3]<- "GNU"
   } else if (!is.na(str_match(cc, regex("Mozilla", ignore_case = TRUE))[1])){
     licenses_types[i,2]<- (str_match(cc, regex("Mozilla(.){10}", ignore_case = TRUE))[1])
     licenses_types[i,3]<- "Mozilla"
   } else if (!is.na(str_match(cc, regex("Academic Free license", ignore_case = TRUE))[1])){
     licenses_types[i,2]<- (str_match(cc, regex("Academic Free license", ignore_case = TRUE))[1])
     licenses_types[i,3]<- "Academic Free license"
   } else if (!is.na(str_match(cc, regex("x11", ignore_case = TRUE))[1])){
     licenses_types[i,2]<- (str_match(cc, regex("x11", ignore_case = TRUE))[1])
     licenses_types[i,3]<- "x11"
   }

}

colnames(licenses_types)<-c("pmid","license","lic_code")

licenses_types <- data.frame(licenses_types)

#################### CATEGORY

category <- searchabsL(abstracts, include=c("algorithm","database","library",
                                            "software","tool","program","programm"))

category_types<-data.frame(matrix(nrow=length(category@PMID),ncol=3))
for(i in 1:length(category@PMID)){
  print(i)
  category_types[i,1]<-(category@PMID[i])
  cc<-str_replace_all(category@Abstract[i], "[\r\n]" , "")
  cc<-str_extract_all(cc, regex("algorithm|database|library|software|tool|program|programm", ignore_case = TRUE))
  category_types[i,2]<-names(sort(table(cc[[1]]),decreasing=T))[1]
  category_types[i,3]<-names(sort(table(cc[[1]]),decreasing=T))[2]
}  

names(category_types) <- c("pmid","key1","key2")

#key1 <- unique(c(category_types[,2],category_types[,3]))
#key1_cor<-c("algorithm","software","software","algorithm","tool",'program',"library","database","algorithm","program","tool","database","library","database","database",'software',"tool",NA,"database","algorithm","tool","database")
#recode_cat<-data.frame(key1,key1_cor)


category_types_recode <- merge(category_types,recode_cat,by="key1",all.x=TRUE)
category_types_recode <- merge(category_types_recode,recode_cat,by.x="key2",by.y="key1", all.x=TRUE)



######### LANGUAGE

langs <- searchabsL(abstracts, include=c("python","java","javascript",
                                            "perl","visual basic","cobol","c\\+\\+","c#",
                                         "php",'ruby',"matlab"))
langs_one_let <- searchabsL(abstracts, include=c(" R ", " C "))

languages<-data.frame(matrix(nrow=length(langs@PMID),ncol=2))
for(i in 1:length(langs@PMID)){
  print(i)
  languages[i,1]<-(langs@PMID[i])
  cc<-str_replace_all(langs@Abstract[i], "[\r\n]" , "")
  cc<-str_extract_all(cc, regex("python|java|javascript|perl|visual basic|cobol|c\\+\\+|c#|php|ruby|matlab", ignore_case = TRUE))
  vv <- names(sort(table(cc[[1]]),decreasing=T))
  if (length(vv) > 0){
    languages[i,2]<-vv[1]
  }
}

languages_one_let<-data.frame(matrix(nrow=length(langs_one_let@PMID),ncol=2))
for(i in 1:length(langs_one_let@PMID)){
  print(i)
  languages_one_let[i,1]<-(langs_one_let@PMID[i])
  cc<-str_replace_all(langs_one_let@Abstract[i], "[\r\n]" , "")
  cc<-str_extract_all(cc, regex(" R | C ", ignore_case = TRUE))
  vv <- names(sort(table(cc[[1]]),decreasing=T))
  if (length(vv) > 0){
    languages_one_let[i,2]<-vv[1]
  }
}

alllang <- rbind(languages,languages_one_let)


names(alllang) <- c("pmid","lang")

#lang1 <- unique(alllang[,2])
#lang1_cor<-c("python","python","matlab","c++","java",'perl',"c#","matlab","php"
#             ,"java","perl","java","ruby","perl","perl",'visual basic',NA,"c++","php","php","ruby",
#             'matlab',"matlab","r","c","r","c")
#recode_lang<-data.frame(lang1,lang1_cor)


alllang_recode <- merge(alllang,recode_lang,by.x="lang",by.y="lang1",all.x=TRUE)

#### web-serivce
webser <- searchabsL(abstracts, include=c("web-service","webservice","web server","webserver"))

webser_support<-data.frame(matrix(nrow=length(webser@PMID),ncol=2))
for(i in 1:length(webser@PMID)){
  print(i)
  webser_support[i,1]<-(webser@PMID[i])
  cc<-str_replace_all(webser@Abstract[i], "[\r\n]" , "")
  cc<-str_extract_all(cc, regex("web-service|webservice|web server|webserver", ignore_case = TRUE))
  vv <- names(sort(table(cc[[1]]),decreasing=T))
  if (length(vv) > 0){
    webser_support[i,2]<-"web_server"
  }
}
colnames(webser_support)<-c("pmid","websrv")


### MPI

mpi <- searchabsT(abstracts, include=c(" MPI"," openMP"))

mpi_support<-data.frame(matrix(nrow=length(mpi@PMID),ncol=2))
for(i in 1:length(mpi@PMID)){
  print(i)
  mpi_support[i,1]<-(mpi@PMID[i])
  cc<-str_replace_all(mpi@Abstract[i], "[\r\n]" , "")
  cc<-str_extract_all(cc, regex("MPI|openMP", ignore_case = TRUE))
  vv <- names(sort(table(cc[[1]]),decreasing=T))
  if (length(vv) > 0){
    mpi_support[i,2]<-vv[1]
  }
}

colnames(mpi_support)<-c("pmid","mpi")

### GALAXY integration

gal <- searchabsL(abstracts, include=c("galaxy"))

galaxy_support<-data.frame(matrix(nrow=length(gal@PMID),ncol=2))
for(i in 1:length(gal@PMID)){
  print(i)
  galaxy_support[i,1]<-(gal@PMID[i])
  cc<-str_replace_all(gal@Abstract[i], "[\r\n]" , "")
  cc<-str_extract_all(cc, regex("galaxy", ignore_case = TRUE))
  vv <- names(sort(table(cc[[1]]),decreasing=T))
  if (length(vv) > 0){
    galaxy_support[i,2]<-"galaxy"
  }
}
colnames(galaxy_support)<-c("pmid","galaxy")


############# MERGING DATA

aa1<-merge(aa, allurls, by="pmid",all.x = TRUE)
aa1<-merge(aa1, alllang_recode, by="pmid",all.x = TRUE)
aa1<-merge(aa1, category_types_recode, by="pmid",all.x = TRUE)
aa1<-merge(aa1, licenses_types, by="pmid",all.x = TRUE)
aa1<-merge(aa1, webser_support, by="pmid",all.x = TRUE)
aa1<-merge(aa1, mpi_support, by="pmid",all.x = TRUE)
aa1<-merge(aa1, galaxy_support, by="pmid",all.x = TRUE)


journals <- rbind(bio_bmc, bio_oxford, bio_evol_online, plos_compbio, gen_res)

# allurls 
# alllang_recode
# category_types_recode
# licenses_types
# webser_support
# mpi_support
# galaxy_support

######## get back the git informationcs
journals <- read.csv("journal.csv",stringsAsFactors=FALSE)
git_summary <- read.csv("gitsresults.csv",stringsAsFactors=FALSE)
journals_git<-merge(journals, git_summary, by="pmid",all.x = TRUE)

journals_git$month_support <-as.integer( difftime(as.Date(journals_git$commit_date_to),
                 as.Date(journals_git$commit_date_from),units="weeks"))


######### KEYWORDS


for (i in 1:dim(journals_git)[1]){
  
  print(paste(i,journals$pmcid[i],sep=" "))
  journals_git$keywords[i] <- paste(get_MESH(journals_git$pmid[i])$descriptorName,collapse=",")
  
}

write.csv(journals_git, file="journals_git.csv",row.names =F)

all_keywords <- paste(journals_git$keywords[journals_git$keywords != "" ], collapse=",")



############ Getting cross-references
# 2024

for (i in 24513:length(journals_git$pmid)){
   print (i)
   uu <- paste(unlist((get_citations(ext_id = journals_git$pmid[i]))$journalAbbreviation), collapse=",")
   if (!is.null(uu)){
    journals_git$crossref[i] <- uu
   } else {
     journals_git$crossref[i] <- NA    
   }

}

plot(head(sort(table(strsplit(paste(journals_git$crossref[!is.na(journals_git$key1_cor.x)], collapse=","), 
                              ",")), decreasing=T),150))
############## ---------------------- PLOTTING

######## WORD CLOUD
pp <- unlist(strsplit(all_keywords, split=","))
tt <- data.frame(table(pp))
wordcloud(tt$pp,tt$Freq,c(8,.3),1000)
wordcloud(tt$pp,tt$Freq,c(2,.9),1000, colors=c('#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f', '#a6cee3'))


########## GRAPHICS

# count how many tools
length(journals_git$key1_cor.x[!is.na(journals_git$key1_cor.x)])


# plot categories
tt <- data.frame(sort(table(journals_git$key1_cor.x),decreasing=T), names(sort(table(journals_git$key1_cor.x),decreasing=T)))
colnames(tt) <- c("Freq","Var1")
tt$position <- cumsum(tt$Freq) - 0.5*tt$Freq
tt$Var1<-factor(tt$Var1,levels(tt$Var1)[c(1,6,2,4,5,3)])
tt$perc<-round(prop.table(tt$Freq)*100,1)

ggplot(tt, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(values = c('#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f', '#a6cee3')) +
  #coord_polar("y", start = 0) +
  labs(title = "Categories of bioinformatics tools") +
  geom_text(aes(x="",y=(position), label = paste(round(prop.table(Freq)*100,1),"%",sep=""), 
                fill="white", size=8))


ggplot(tt, aes(x = Var1, y = Freq, fill=Var1)) + 
   geom_bar(stat="identity", fill="steelblue") +
   theme_minimal() + 
   ylab("Total number of publications") +
   theme(axis.title.x=element_blank()) +
   geom_text(aes(label=paste(perc,"%",sep="")), vjust=1.6, color="white", size=5.5)

#scale_fill_brewer(palette="Blues", direction = -1)

# plot licenses
barplot(sort(table(journals_git$lic_code)))

# correlation team vs co-authors
ggplot(journals_git, aes(x=team_size, y=coathors)) + 
  geom_point()+
  geom_smooth(method=lm, color="black",  se = FALSE)+
  labs(title="(absence) of correlation b/w committers & co-authors",
       x="Code contributors", y = "Co-authors")+
  scale_color_fivethirtyeight("team_size") +
  theme_fivethirtyeight() +
  ylim(0,20)


# polar histogram - repo
d <- (journals_git[!is.na(journals_git$repo),c(10,27)])
a <- ddply(d, .(pubYear), function(d) {data.frame(table(d$repo)/length(d$repo))})
colnames(a) <- c("item","score","value")
a$family <- "all"
a$score<-factor(a$score,levels(a$score)[c(1,2,5,3,7,4,6)])
pp <- polarHistogram(a, familyLabel = FALSE, direction = "outwards")
print(pp +  scale_fill_manual(values= c('#1f78b4','#b2df8a','#33a02c',
                                        '#fb9a99','#fdbf6f','#e31a1c', '#a6cee3')))


# polar histogram - lang
k <- (journals_git[!is.na(journals_git$lang1_cor),c(10,29)])
b <- ddply(k, .(pubYear), function(d) {data.frame(table(d$lang1_cor)/length(d$lang1_cor))})
colnames(b) <- c("item","score","value")
b$family <- "all"
pp2 <- polarHistogram(b, familyLabel = FALSE, direction = "outwards")
print(pp2 +  scale_fill_manual(values= c('#a6cee3','#1f78b4','#b2df8a','#33a02c',
                                          '#fb9a99','#e31a1c','#fdbf6f','#ff7f00',
                                          '#cab2d6','#6a3d9a','#ffff99')))


##
journals_git$citedByCount <- journals_git$citedByCount[,1]
ll <- journals_git[,c(10,17)]
ll$citedRecode <- cut(ll$citedByCount, breaks=c(-1,0,1,2,5,10,10000000), labels = c("0", "1", "2", "2-5", "5-10", "10+"))
set.seed(1234)
Q1 <- as.factor(sample(ll$citedRecode[ll$pubYear >= 2012], 9000))
Q2 <- as.factor(sample(ll$citedRecode[ll$pubYear >= 2010], 9000))
Q3 <- as.factor(sample(ll$citedRecode[ll$pubYear >= 2007], 9000))
Q4 <- as.factor(sample(ll$citedRecode[ll$pubYear >= 2004], 9000))
Q5 <- as.factor(sample(ll$citedRecode[ll$pubYear >= 2000], 9000))
likert_6 <- data.frame(Q1, Q2, Q3, Q4, Q5)
colnames(likert_6) <- c("3 years", "9 years", "9 years", "12 years", "16 years")
levels_6 <- c( "0 citations", "1 citation","2 citations",
               "3-5 citations","6-10 citation", "10+ citations")
items <- list(c("3 years", "5 years", "9 years", "12 years", "16 years"))
sjp.setTheme(theme = "blank")
sjp.likert(likert_6,
           legendLabels = levels_6,
           axisLabels.y = items,
           sort.frq = "pos.desc",
           labelDigits = 0,
           showPercentageSign = TRUE,
           value.labels = "sum.inside")







pie(sort(prop.table(table( journals_git$lic_code[!is.na(journals_git$key1_cor.x)])), decreasing=T), clockwise=T)

radius <- sqrt( crime$population/ pi )
symbols(crime$murder, crime$burglary, circles=radius)


barplot(table( journals_git$key1_cor.x[!is.na(journals_git$key1_cor.x)], journals_git$pubYear[!is.na(journals_git$key1_cor.x)]))

barplot(table( journals_git$journalTitle[!is.na(journals_git$key1_cor.x)], journals_git$pubYear[!is.na(journals_git$key1_cor.x)]))


barplot(table( journals_git$lic_code[!is.na(journals_git$key1_cor.x)], journals_git$pubYear[!is.na(journals_git$key1_cor.x)]))


data3<-cbind(data.frame(table(data1$repo, data1$pubYear)),matrix(as.data.frame.matrix(table(data1$repo, data1$pubYear)))[1:154])

matrix(ff)
reshape(data2, direction="long", varying=list(names(d)[3:7]), v.names="Value", 
        idvar=c("Code","Country"), timevar="Year", times=1950:1954)

a = ddply(journals_git[!is.na(journals_git$key1_cor.x),], .(journalTitle), function(d) {
  data.frame(table(d$lang1_cor)/length(d$lang1_cor))
})


get_MESH(journals$pmcid[2])$descriptorName

ggplot(data=aa1, aes(x=pubYear,col=repo,fill=repo)) +
  facet_wrap(~repo) + geom_bar()


## plotting 
ggplot(data=aa1, aes(x=pubYear,col=repo,fill=repo)) +
  facet_wrap(~repo) + geom_bar()

(prop.table(table(journals$citedByCount[journals$pubYear >=1997])))[1:10]

ggplot(data=aa1) +
facet_wrap(~repo) +
  geom_bar(aes(y = (..count..)/sum(..count..)))
geom_point(data=aa1, aes(y=..count..,stat="bin", x=pubYear))



ggplot(mydataf, aes(x = foo)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)))




qq<- str_extract_all(cc, regex("python|java|javascript|perl|cobol|visual basic|c++|c#", ignore_case = TRUE))


rr <- searchabsL(abstracts, include=c("galaxy"))


























qq<- str_extract_all(cc, regex("algorithm|database|library|software|tool|program|programm|standalone", ignore_case = TRUE))

licenses <- searchabsL(abstracts, include=c("open-source license", "open source license","free for academic","commercial license"))




dl <- data.frame(ID = rep(names(dd$pmid), sapply(dd$pmid, length)),
                 Obs = unlist(dd$pmid))


## read in abstracts

abstracts <- readabs("pubmed_result.txt")
head(Find_conclusion(abstracts))
yy <- (searchabsL(abstracts, include="AVAILABILITY"))
yy <- (searchabsL(abstracts, include="github"))
yy <- (searchabsL(abstracts, include="sourceforge"))
yy <- (searchabsL(abstracts, include="googlecode"))
yy <- (searchabsL(abstracts, include="bitbucket"))
yy <- (searchabsL(abstracts, include="web service"))
yy <- (searchabsL(abstracts, include="web server"))
yy <- (searchabsL(abstracts, include="source code"))
yy <- (searchabsL(abstracts, include="algorithm"))
yy <- (searchabsL(abstracts, include="standalone"))
yy <- (searchabsL(abstracts, include="website"))
yy <- (searchabsL(abstracts, include="database"))
yy <- (searchabsL(abstracts, include="library"))
yy <- (searchabsL(abstracts, include="software"))
yy <- (searchabsL(abstracts, include="tool"))
yy <- (searchabsL(abstracts, include="http://"))
yy <- (searchabsL(abstracts, restrict=c("code"), exclude="source"))
yy <- (searchabsL(abstracts, restrict=c("galaxy")))
Ruby
Bioconductor
python
java
c++

head(word_atomizations(yy))


cc <- Find_conclusion(yy)
