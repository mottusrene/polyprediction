require(tidyverse)

source("helpers.R")

Data = read_csv("../cleanedData.csv")
Data[, 1:240] = (Data[,1:240] + Data[,241:480]) / 2        ## combine self and informant ratings
Data = subset(Data, rowSums(!is.na(Data[,1:240])) == 240 ) ## throw out those with missing data
Data = Data %>% mutate(
  Walking = log(Walking + 1), 
  Exercise = round(Exercise),
  BMI = log(BMI),
  AlcUnitsLastYear = round(Data$AlcUnitsLastYear*100),
  Veg = rowMeans(select(Data, "FreshVeg", "BoiledVeg", "FreshFruits"), na.rm = T),
  Smoking = recode(Smoking, 'never' = 0, 'current' = 1, 'former' = 1))

dem = Data %>% select(Age, Sex)

items = Data %>% select(1:240) 

# items = as.list(items) %>%
#   map(~residuals(lm(.x ~ as.matrix(dem)))) %>%
#   bind_cols %>% `names<-`(names(items)) 

facets = (.fn = names(items) %>% strtrim(2) %>% unique) %>%
  map( ~rowMeans(select(items, grep(., names(items)))) ) %>%
  bind_cols %>% `names<-`(.fn)

domains = (.dn = names(items) %>% strtrim(1) %>% unique) %>%
  map( ~rowMeans(select(items, grep(., names(items)))) ) %>%
  bind_cols %>% `names<-`(.dn)

res = names(items) %>%
  map(~select(items, -.)) %>%  
  map2(as.list(names(items)), ~ rowMeans(select(.x, contains(strtrim(.y,2))))) %>% 
  map2(as.list(names(items)), ~ tibble(.x, select(facets, -contains(strtrim(.y,2))))) %>%
  map2(as.list(items), ~ residuals(lm(.y ~ as.matrix(.x)))) %>%
  bind_cols %>% `names<-`(names(items)) 

outcomes = Data %>% select(Education, BMI, Exercise, Walking, Alcohol, AlcUnitsLastYear, SoftDrinks, Veg, Sweets, Smoking)
families = list("gaussian", "gaussian", "poisson", "gaussian", "gaussian", "poisson", "gaussian", "gaussian", "gaussian", "binomial")

result = replicate(2, 
                   list(
                     x = list(dem, domains, facets, items,res) %>% rep(times = 10),
                     y1 = rep(outcomes, each = 5),
                     fam = families %>% rep(each = 5)) %>%
                     pmap_dbl(foo))

result[is.na(result)] = 0

(tab1 = result %>% rowMeans %>% matrix(ncol=5, byrow = T) %>% 
    `colnames<-`(c("dems","domains","facets","items", "residuals")) %>% 
    as_tibble %>% mutate(outcome = names(outcomes))) %>% 
  select(-outcome) %>% sapply(median)

list(domains, facets, items) %>% 
  map(~replicate(2, foo(., Data$BMI, y2 = Data$Smoking, fam="gaussian", fam2 = "binomial"))) %>%
  bind_cols %>% colMeans

result2 = replicate(2, 
                    list(
                      x = list(dem, domains, facets, items, res) %>% rep(times = 10),
                      y1 = Data$Education %>% list %>% rep(50),
                      y2 = rep(outcomes, each = 5),
                      fam = rep("gaussian", 50) %>% as.list,
                      fam2 = families %>% rep(each = 5)) %>%
                      pmap_dbl(foo))

result2[is.na(result2)] = 0

(tab2 = result2 %>% rowMeans %>% matrix(ncol=5, byrow = T) %>% 
    `colnames<-`(c("dems","domains","facets","items", "residuals")) %>% 
    as_tibble %>% mutate(outcome = names(outcomes))) %>% 
  select(-outcome) %>% sapply(median)

