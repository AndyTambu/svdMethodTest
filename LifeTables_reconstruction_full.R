# life tables reconstruction
# 
# I take teh code of markus and I adapt it to the new API for the data from Eurostat
library(dplyr)
library(restatapi)


data_base <- get_eurostat_data( "demo_mlexpecedu",
                           filters = NULL,
                           exact_match = TRUE, date_filter = NULL,
                           label = FALSE,
                           select_freq = NULL,
                           cache = TRUE,
                           update_cache = FALSE, cache_dir = NULL, compress_file = TRUE, stringsAsFactors = TRUE, keep_flags = FALSE,
                           cflags = FALSE,
                           check_toc = FALSE, local_filter = TRUE, force_local_filter = FALSE,
                           verbose = FALSE)

data_base$isced11 <- as.character(data_base$isced11)
data_base$isced11 <- ifelse(data_base$isced11=="ED0-2", "lower", data_base$isced11) 
data_base$isced11 <- ifelse(data_base$isced11=="ED3_4", "middle", data_base$isced11) 
data_base$isced11 <- ifelse(data_base$isced11=="ED5-8", "higher", data_base$isced11) 
data_base$isced11 <- ifelse(data_base$isced11=="TOTAL", "total", data_base$isced11)
data_base$age <- as.character(data_base$age)
data_base$age <- ifelse(data_base$age=="Y_LT1", "Y0", data_base$age) 
data_base$age <- ifelse(data_base$age=="Y_GE85", "Y85", data_base$age) 
data_base$age <- substring(data_base$age, 2)
data_base <- data_base[,-1]
colnames(data_base) <- c("sex","age","edu","country","year","ex") 
data_base$age <- as.numeric(data_base$age)
#As we focus on the year 2016




# pay attention, this is done for just one year !! 

# The following function has the arguments “country.select”, “edu.select” and “sex.select”. Thus, the funcation allows to derive life tables for each educational level (high, middle, low, and total), for each country with available data (16 European countries), separated for men and women.


my.function <- function(country.select, edu.select, sex.select) {
  select.country <- arrange(filter(data, country==country.select ,edu==edu.select &sex==sex.select),age)
  #smooth to get more decimals by applying the loess function
  #and then predicting ex with more decimals
  grab.LE <- select.country$ex
  smooth.it <- loess(grab.LE~select.country$age, span=0.2)
  
  predict.it <- predict(smooth.it, seq(0,85,1)) 
  select.country$ex.decimals <- predict.it
  LT.derive <- data.frame(Age=0:85) 
  LT.derive$lx <- NA
  LT.derive$Tx <- NA
  LT.derive$ex <- select.country$ex.decimals 
  LT.derive$lx[1] <- 100000
  LT.derive$Tx[1] <- 100000*select.country$ex.decimals[1]
  #this loop refers to equation 1 in the paper
  for (j in 2:86) {
    upper <- (LT.derive$lx[j-1]^2)-2*LT.derive$lx[j-1]*LT.derive$Tx[j-1] 
    bottom <- (LT.derive$ex[j-1]-LT.derive$ex[j])*2*LT.derive$lx[j-1]-2*LT.derive$Tx[j-1]-LT.derive$lx[j-1] 
    LT.derive$Tx[j] <- upper/bottom*LT.derive$ex[j]
    LT.derive$lx[j] <- upper/bottom
  }
  #I check, whether lx is monotonic decreasing, i.e., no resurrection in the life table
  lx.diff <- diff(LT.derive$lx) 
  lx.diff <- round(lx.diff, 5)
  if (all(diff(lx.diff) < 0)) {
    px <- c(LT.frame$lx[-1]/LT.frame$lx[-86],0)
  }else{
    #sometimes, it is not, then I force it =)
    #please note, this occurs usually at very young ages and won't affect #LE at age 30 or older
    lx.diff[lx.diff>=0] <- -runif(length(lx.diff[lx.diff>=0]), 1, 5) 
    lx.monotonic <- cumsum(c(100000, lx.diff))
    px <- c(lx.monotonic[-1]/lx.monotonic[-86],0)
  }
  #from here, the life table is constructed very standard
  lx <- round(c(100000, (cumprod(px)*100000)[1:(length(px)-1)])) 
  dx <- round(c(-diff(lx), lx[length(lx)]))
  LT.derive$lx <- lx
  LT.derive$dx <- dx
  LT.derive$px <- px
  Lx1 <- lx[-1]+0.5[-length(px)]*dx[-length(dx)]
  Lx.open <- LT.derive$Tx[1]-sum(Lx1)
  LT.derive$Lx <- round(c(Lx1, Lx.open))
  LT.derive$Tx <- rev(cumsum(rev(LT.derive$Lx))) 
  LT.derive$ex.derived <- LT.derive$Tx/LT.derive$lx 
  LT.derive$ex.original <- select.country$ex
  LT.derive$diff <- LT.derive$ex.original-LT.derive$ex.derived 
  LT.derive$Country <- country.select
  LT.derive$Edu <- edu.select
  LT.derive$Sex <- sex.select
  
  
  return(LT.derive[,c("Country","Edu","Sex","Age","px","lx","dx","Lx", "Tx","ex.derived","ex.original","diff")])
}












for (y in seq(2007,2017)) {
  

data <- filter(data_base, year==y)


#these are the country codes
edu.countries <- data %>% distinct(country) %>% pull()
###Females###
out.females <- c()
for (country.select in edu.countries) {
  for (edu.select in c("higher","middle","lower")) {
    out.females <- rbind(out.females,my.function(country.select, edu.select, "F"))
  } }

###Males###
out.males <- c()
for (country.select in edu.countries) {
  for (edu.select in c("higher","middle","lower")) {
    out.males <- rbind(out.males,my.function(country.select, edu.select, "M"))  } }

name_F <- paste0('life_tab_edu_ESTAT_F_',y,'.csv')
name_M <- paste0('life_tab_edu_ESTAT_M_',y,'.csv')
out.females$time = y
write_csv(tibble(out.females) , file = name_F)    
out.males$time = y
write_csv(as.data.frame(out.males) , file = name_M)    

}


# ok this was a quick and dirty solution for all the different life tables for all the different years
# which can be useful to make other stuff.

