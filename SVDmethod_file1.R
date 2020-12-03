#here we produce some results with the SVD method from Clark. 


install_github("sinafala/svdComp5q0")


library(devtools)
library(tidyverse)
library(svdComp5q0)
library(MortalityLaws)
library(plyr)
library('LifeTables')


predictLT("female",0.05)

predictLT("female",cm=0.05,smooth=TRUE,outlogit=FALSE,out5=TRUE)


# ok so the point here is to look for childhood mortality. Let's see if we find something or not 


library(restatapi)


search_eurostat_toc('Child', lang = "en", verbose = FALSE) %>% tibble() %>% select("title", "dataStart", "dataEnd", "code" , "shortDescription" ) %>% View()

# ok, it does not seem to have any data concernign childhood mortality
# what we can try to do is to extract the data we need from the life tables of marcus
# 
# search_eurostat_toc('educational attainment', lang = "en", verbose = FALSE) %>% tibble() %>%
#   filter(grepl("mother",title)) %>% select(code,shortDescription) %>% View()
# 
# once I have the yearly life table I can also make it a group of 5 years because we just need the survivals
# to then use the LifeTable package to make the full life table. 

# ok we have them in a csv format from the previous project. 


life_tab_edu_ESTAT_F_2016 <- read_csv('life_tab_edu_ESTAT_F.csv')

# pay attention to one thing: the data from this are taken from a specific year, which is 2016 :) 

Child_mort_F_edu_Estat_2016 <- life_tab_edu_ESTAT_F_2016 %>% 
                               select(Country,Edu, Age, lx) %>%  filter(Age == 5) %>% 
                               group_by(Country, Edu) %>% mutate(fiveQzero = 1-lx/100000) %>% 
                               select(Country, Edu, fiveQzero)


life_tab_edu_ESTAT_F_2016 %>% distinct(Country)
edu.countries <- c("BG","CZ","DK","EE","EL","HR","IT","HU", "PL","PT","RO","SI","SK","FI","SE","NO")

# there are also different opportunities for the estimation of the life tables. 
predictLT("female",0.05)

testLT <- predictLT("female",cm=0.00158,smooth=TRUE,outlogit=FALSE,out5=FALSE)

LT <- LifeTable(seq(0,109), qx = testLT[,1])
LT$lt 

# this one comes from the other package but I have the feeling there is not really a difference
# lt.mx(testLT$`cm-0.050.am-0.173`, sex = "female", age = seq(0,109))


ages <- seq(0,109)

test.SVD.females.2016 <- c()
for (country.select in edu.countries) {
  for (edu.select in c("higher","middle","lower")) {
    # country.select <-  'BG'
    # edu.select <- "higher"
    H <- Child_mort_F_edu_Estat_2016 %>% filter(Country==country.select,Edu ==edu.select) %>% pull(fiveQzero)
    mort <- predictLT("female",cm=H,smooth=TRUE,outlogit=FALSE,out5=FALSE)
    LT_list <- LifeTable(ages, qx = mort[,1])
    LT_pivot <- LT_list$lt
    LT_pivot$Country <- country.select
    LT_pivot$EduAtt <- edu.select
    
    test.SVD.females.2016 <- rbind(test.SVD.females.2016,LT_pivot)
  } }


predictLT("female",cm=0.003,smooth=TRUE,outlogit=FALSE,out5=FALSE)



Child_mort_F_edu_Estat_2016 %>% ggplot(aes(x=Country,y=fiveQzero))+geom_point()+geom_hline(yintercept = 0.003)
# ok here we can observe one problem. The problem is that this method seems to be good just 
# for values where the childhood moratlity is >= 0.003 and this is very often not the case for the 
# data we are dealing with.

# To have a way to understand how to use this method, it is then necessary to filter out all the values
# whhich can not be used. I woudl then suggesto to expand the file from marcus and to make all of
# what si doable with the values which are exceptable.

# ok now the idea is to put all the infos together and try to make out a plot of what can be used and what not 


read_csv("./life_tables_reconstructed/life_tab_edu_ESTAT_F_2009.csv")

file_names <- list.files(path = "./life_tables_reconstructed/")
           


life_tables_reconstructed_all <- c()
for (name in file_names) {
 # country.select <-  'BG'
    # edu.select <- "higher"
  pivot_name <- paste0("./life_tables_reconstructed/",name)
 pivot <- read_csv(pivot_name)
 life_tables_reconstructed_all <- rbind(life_tables_reconstructed_all,pivot)
} 

life_tables_reconstructed_all %>% distinct(Sex)

life_tables_reconstructed_all$Sex <- revalue(life_tables_reconstructed_all$Sex, c("FALSE" = "F"))

write_csv(tibble(life_tables_reconstructed_all) , file = 'life_tables_reconstructed_all.csv') 



Child_mort_edu_Estat <- life_tables_reconstructed_all %>% 
                               select(time,Country,Sex,Edu, Age, lx) %>%  filter(Age == 5) %>% 
                               group_by(Country, Edu, time, Sex) %>% mutate(fiveQzero = 1-lx/100000) %>% 
                               select(time, Sex, Country, Edu, fiveQzero)

Child_mort_edu_Estat$time <- as.character(Child_mort_edu_Estat$time)
Child_mort_edu_Estat %>% mutate(child_mort = ifelse(fiveQzero < 0.003, "out", "in"))  %>% 
  ggplot(aes(time,Country))+geom_raster(aes(fill=child_mort))


# 
# ok now you can isolate the cases for which you have values whoch are good and you can make a 
# comparison of at least these two methods.

predictLT("female",cm=0.05,smooth=TRUE,outlogit=FALSE,out5=TRUE)

# toeretically you can get it in the out5 form to compare it with the other method but this is not 
# what you can do now. You can proceed with that in a second moment. Now you can try to 
# make something easier, making just what you have and see how it goes.





life_tables_reconstructed_all$Sex <- revalue(life_tables_reconstructed_all$Sex, c("F" = "female","M" = "male"))


Child_mort_edu_Estat_filtered <- Child_mort_edu_Estat %>% filter(fiveQzero >= 0.003)

Child_mort_edu_Estat_filtered$Sex <- revalue(Child_mort_edu_Estat_filtered$Sex, c("F" = "female","M" = "male"))

ages <- seq(0,109)

test.SVD <- c()
for (i in 1:dim(Child_mort_edu_Estat_filtered)[1]) {
    edu.select <- Child_mort_edu_Estat_filtered[i,]$Edu
    country.select <-  Child_mort_edu_Estat_filtered[i,]$Country
    time.select <-  Child_mort_edu_Estat_filtered[i,]$time
    sex.select <-  Child_mort_edu_Estat_filtered[i,]$Sex
    
    H <-  Child_mort_edu_Estat_filtered[i,]$fiveQzero
    mort <- predictLT(sex.select,cm=H,smooth=TRUE,outlogit=FALSE,out5=FALSE)
    LT_list <- LifeTable(ages, qx = mort[,1])
    LT_pivot <- LT_list$lt
    LT_pivot$Country <- country.select
    LT_pivot$EduAtt <- edu.select
    LT_pivot$time <- time.select
    LT_pivot$sex <- sex.select

    test.SVD <- rbind(test.SVD,LT_pivot)
} 


ex_test_SVD <- test.SVD %>% tibble() %>% select(time, sex, Country, EduAtt,x,ex) %>% filter(x<86)

life_tables_reconstructed_all

# now if I make an inner join all should be fine, in the sense that I avoid to take the life tables
# where the childhood mortality is too big.

# I have to inner join ex_test_SVD and life_tables_reconstructed_all


test_results <- inner_join(ex_test_SVD,life_tables_reconstructed_all, 
                           by = c('time'='time', 'sex' = 'Sex', 'Country' = 'Country', 
                                  'EduAtt' = 'Edu', 'x' = 'Age')) %>% select('time', 'sex',   'Country', 'EduAtt',     'x'  ,  'ex', 'ex.original')


colnames(test_results) <- c('time','sex','Country','EduAtt','age','ex.SVD','ex.original')

test_results_diff <- test_results %>% group_by(time,sex,Country,EduAtt,age) %>% 
                                      mutate(diff.orig.SVD = ex.original-ex.SVD)



test_results_diff %>% head()


test_results_diff %>% ungroup() %>% 
  mutate(EduAtt = factor(EduAtt, levels=c('lower','middle','higher'))) %>% 
  select("EduAtt","diff.orig.SVD")%>% 
  ggplot(aes(x = diff.orig.SVD ,fill = EduAtt, color = EduAtt ))+ facet_wrap(~ EduAtt)+geom_histogram(binwidth = 1, show.legend = FALSE)



