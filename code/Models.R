library(readr)
library(dplyr)
library(sf)
library(tmap)
library(purrr)
library(spdep)
library(caret)
library(tidyr)


spw <- read_csv("Data/COVID/spatialweights.csv")

covid <- read_csv("Data/COVID/COVID_total11_06_2020.txt")
covid <- covid %>% mutate(CaseDensity= if_else(is.na(SVI), NA_real_, CaseDensity),
                              CaseDensity= if_else(is.na(SumUHVI), NA_real_, CaseDensity),
                          CaseDensity= if_else(Geo_FIPS %in%c(36005033400,36061014300), NA_real_, CaseDensity))
spwJoin <- left_join(spw, covid %>% select( IDInt,CaseDensity, cases), by = c("NID" = "IDInt"))


COVIDWeights <- spwJoin %>% mutate(caseWeighted = cases * WEIGHT,
                                   spatialLag = CaseDensity * WEIGHT) %>% 
  group_by(IDINT) %>% 
  summarize(caseWeighted = sum(caseWeighted, na.rm = TRUE),
            spatialLag = sum(spatialLag, na.rm = TRUE))

covid <- read_csv("Data/COVID/COVID_total11_06_2020.txt")
covid <- covid %>% mutate(CaseDensity= if_else(is.na(SVI), NA_real_, CaseDensity),
                          CaseDensity= if_else(is.na(SumUHVI), NA_real_, CaseDensity),
                          CaseDensity= if_else(Geo_FIPS %in%c(36005033400,36061014300), NA_real_, CaseDensity))
covid <- covid %>% left_join(COVIDWeights,by = c("IDInt" = "IDINT")) %>% 
    select(-c(OBJECTID, SidewalkCount, sdSidewalkWidth, Shape_Area, 
              Shape_Length, parksDensity, parkRecDist, flagshipParkDist, IDInt)) 
covidCols <- names(covid)
library(stringr)
names(covid) <- str_replace_all(covidCols, "Dist", "Proximity")

covid <- covid%>% 
  rename(ParksProximity = NewParksProximity,
         ParksDensity = NewParksDensity,
         PharmacyProximity = PharmProximity,
         UHVI = SumUHVI,
         SoupKitchenProximity = SoupKitchProximity,
         MeanSidewalkWidth = meanSidewalkWidth,
         HospitalDensity = hospitalDensity,
         GroceryDensity = supermarketDensity,
         GroceryProximity = GorceryProximity,
         SoupKitchenDensity = soupKitchenDensity,
         PharmacyDensity = pharmacyDensity,
         SubwayDensity = subwayDensity,
         NursingHomeDensity = nursingHomeDensity,
         SpatialLag =spatialLag)

nyct <- st_read("shapes/BaseLayers/nyct2010/")

nyct <- nyct %>% left_join(covid %>% 
                               mutate(Geo_FIPS = as.character(Geo_FIPS)),
                           by = c("Geo_FIPS" = "Geo_FIPS")) 

nybb <- st_read("shapes/BaseLayers/nybb/")
hospital <- st_read("shapes/Health/Hospitals.shp")
pharmacy <- st_read("shapes/Health/Pharmacies.shp")
nursingHomes <- st_read("shapes/Health/NursingHomes.shp")

cases <- tm_shape(nyct %>% mutate(CaseDensity = CaseDensity * 100000)) + 
  tm_fill(col = "CaseDensity", style = "jenks", n= 5, title = "COVID-19 Case Rate (per 100,000)")+
  tm_borders(lwd = .4,col = "white") +
  tm_style("watercolor", frame = FALSE) + 
  tm_layout(title ="(a)", title.size = 1,legend.title.size = 0.9,
            legend.text.size = 0.6,title.position = c('center', 'bottom'))+
tm_scale_bar(position = c("right", "bottom")) 
### SVI
svi <- tm_shape(nyct %>% mutate(CaseDensity = CaseDensity * 100000)) + 
  tm_fill(col = "SVI", style = "jenks", n= 5, title = "Social Vulnerability Index (SVI)")+
  tm_borders(lwd = .4,col = "white") +
  tm_style("watercolor", frame = FALSE) + 
  tm_layout(title ="(b)", title.size = 1,legend.title.size = 0.9,
            legend.text.size = 0.6,title.position = c('center', 'bottom'))+
tm_scale_bar(position = c("right", "bottom")) 
### UHVI
uhvi <- tm_shape(nyct %>% mutate(CaseDensity = CaseDensity * 100000)) + 
  tm_fill(col = "UHVI", style = "jenks", n= 5, title =  "Urban Health Vulnerability Index (UHVI)")+
  tm_borders(lwd = .4,col = "white") +
  tm_style("watercolor", frame = FALSE) + 
  tm_layout(title ="(b)", title.size = 1,legend.title.size = 0.9,
            legend.text.size = 0.6,title.position = c('center', 'bottom'))+
tm_scale_bar(position = c("right", "bottom")) 


tmap_arrange(cases, uhvi, svi)

### hospital
tm_shape(nybb) + 
  tm_borders(lwd = .4,col = "gray") +
  tm_shape(hospital) +
  tm_dots(size = .1, shape = 1,col = "blue", alpha = .5)+
  tm_style( "watercolor", frame = FALSE,title = "Hospitals", title.size = 1) +
  tm_scale_bar(position = c("right", "bottom")) 
### Pharmacies
tm_shape(nybb) + 
  tm_borders(lwd = .4,col = "gray") +
  tm_shape(pharmacy) +
  tm_dots(size = .1, shape = 1,col = "blue", alpha = 0.5)+
  tm_style( "watercolor", frame = FALSE,title = "Pharmacies", title.size = 1) +
  tm_scale_bar(position = c("right", "bottom")) 
### Nursing Homes
tm_shape(nybb) + 
  tm_borders(lwd = .4,col = "gray") +
  tm_shape(nursingHomes) +
  tm_dots(size = .1, shape = 1,col = "blue", alpha = 0.5)+
  tm_style( "watercolor", frame = FALSE,title = "Nursing Homes", title.size = 1) +
  tm_scale_bar(position = c("right", "bottom")) 

# Summary stats table
covid %>% 
  group_by(Borough) %>% select(UHVI, SVI) %>% 
  pivot_longer(-Borough, names_to = "Variable", values_to = "Val") %>% 
  group_by( Variable, Borough) %>% 
  summarize(mean =  mean(Val, na.rm=TRUE),
            median = median(Val, na.rm=TRUE),
            max = max(Val, na.rm=TRUE),
            min = min(Val, na.rm=TRUE),
            sd = sd(Val, na.rm=TRUE)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  knitr::kable()

covid %>% ggplot(aes(x=SVI)) +
  geom_histogram() +
  facet_wrap(~Borough) +
  theme_minimal()

covid %>% 
  ggplot(aes(x = Borough, y = SVI)) +
  geom_violin(aes(fill = Borough), color= NA) +
  theme_minimal()

covid %>% 
  ggplot(aes(x = Borough, y = UHVI)) +
  geom_violin(aes(fill = Borough), color= NA) +
  theme_minimal()


covid %>% mutate(CaseDensity = CaseDensity * 1000,
                 SpatialLag = SpatialLag * 100000) %>% 
  mutate_at(vars(contains("Density")), function(x) x*100) %>% 
  select(-c(Geo_FIPS,caseWeighted,cases, deaths, DeathDensity)) %>% 
pivot_longer(-Borough, names_to = "variable", values_to = "Val") %>% 
  group_by(variable) %>% 
  summarize(mean =  mean(Val, na.rm=TRUE),
            median = median(Val, na.rm=TRUE),
            max = max(Val, na.rm=TRUE),
            min = min(Val, na.rm=TRUE),
            sd = sd(Val, na.rm=TRUE)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  knitr::kable()

library(glmnet)
library(useful)
library(coefplot)
scale_this <- function(x) as.vector(scale(x))

modelDF <- covid  %>% 
    filter(complete.cases(.)) %>% 
    mutate_at(vars(-c(Geo_FIPS, Borough)),scale_this)

### Correlations

z <- modelDF %>% 
  select_if(is.numeric) %>% 
  select(-c(Geo_FIPS,caseWeighted,cases, deaths, DeathDensity)) %>% 
  cor

z[lower.tri(z,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
z=as.data.frame(as.table(z))  #Turn into a 3-column table
z=na.omit(z)  #Get rid of the junk we flagged above
z=z[order(-abs(z$Freq)),] 

z %>%rename(`Variable 1` = Var1,
            `Variable 2` = Var2,
            Correlation = Freq) %>% 
  head(10) %>%  
  knitr::kable()

formulaCasesCI <- CaseDensity ~ MeanSidewalkWidth  + ParksProximity + SubwayProximity +
    SoupKitchenProximity + PharmacyProximity + NursingHomeProximity + HospitalProximity + GroceryProximity + 
    HospitalDensity + GroceryDensity + SoupKitchenDensity +
    PharmacyDensity + SubwayDensity + NursingHomeDensity + ParksDensity -1

formulaCases_CI_SVI <- CaseDensity ~ MeanSidewalkWidth  + ParksProximity + SubwayProximity +
    SoupKitchenProximity + PharmacyProximity + NursingHomeProximity + HospitalProximity + GroceryProximity + 
    SVI  + HospitalDensity + GroceryDensity + SoupKitchenDensity +
    PharmacyDensity + SubwayDensity + NursingHomeDensity + ParksDensity -1

formulaCases_CI_UHVI <- CaseDensity ~ MeanSidewalkWidth  + ParksProximity + SubwayProximity +
  SoupKitchenProximity + PharmacyProximity + NursingHomeProximity + HospitalProximity + GroceryProximity + 
  UHVI  + HospitalDensity + GroceryDensity + SoupKitchenDensity +
  PharmacyDensity + SubwayDensity + NursingHomeDensity + ParksDensity -1

formulaCasesSVI_UHVI <- CaseDensity ~ SVI + UHVI -1

# formulaCasesCI_Density <-  CaseDensity ~
#   HospitalDensity + GroceryDensity + SoupKitchenDensity +
#   PharmacyDensity + SubwayDensity + NursingHomeDensity + ParksDensity -1
# 
# formulaCasesCI_Distance <-  CaseDensity ~ MeanSidewalkWidth  + ParksProximity + SubwayProximity +
#   SoupKitchenProximity + PharmacyProximity + NursingHomeProximity + HospitalProximity + GroceryProximity  -1

formulaCasesLag_all <- CaseDensity ~ MeanSidewalkWidth  + ParksProximity + SubwayProximity +
  SoupKitchenProximity + PharmacyProximity + NursingHomeProximity + HospitalProximity + GroceryProximity +
  SVI + UHVI + HospitalDensity + GroceryDensity + SoupKitchenDensity +
  PharmacyDensity + SubwayDensity + NursingHomeDensity + ParksDensity + SpatialLag -1

formulaCasesALL <- CaseDensity ~ MeanSidewalkWidth  + ParksProximity + SubwayProximity +
    SoupKitchenProximity + PharmacyProximity + NursingHomeProximity + HospitalProximity + GroceryProximity + 
    SVI + UHVI + HospitalDensity + GroceryDensity + SoupKitchenDensity +
    PharmacyDensity + SubwayDensity + NursingHomeDensity + ParksDensity -1

formulaList <- list("CI Proximity + Density" = formulaCasesCI, 
                    # "CI density" = formulaCasesCI_Density,
                    # "CI distance " = formulaCasesCI_Distance,
                    "CI + SVI" = formulaCases_CI_SVI, 
                    "CI + UHVI" = formulaCases_CI_UHVI,
                    "SVI + UHVI" = formulaCasesSVI_UHVI, 
                    "CI + SVI + UHVI" = formulaCasesALL,
                    "CI + SVI + UHVI + Spatial Lag" = formulaCasesLag_all
)

formulaDF <- map2_dfr(formulaList, formulaList %>% names(), function(x,y)tibble(name = y,formula = list(x)))

## Functions for analysis
xFunc <- function(formula,data) build.x(formula, data, contrasts=FALSE, sparse=TRUE)
yFunc <- function(formula,data) build.y(formula, data) %>% as.numeric()

### Elastic Net
myControl <- trainControl(method = "cv", number = 5)
myGrid <- expand.grid(
  alpha = seq(0,1,.1), lambda = seq(0,0.2,.05)
)
alphaFunc <- function(formula,modelDF){
  set.seed(990)
  out <- train(formula, data = modelDF,
        method = "glmnet",tuneGrid = myGrid,
  tfControl =myControl)
  out$bestTune$alpha
}
modelFunc <- function(x, y, HP){
  set.seed(990)
    mod <- cv.glmnet(x=x,
                     y=y,alpha = HP,
                     family='gaussian', nfolds=5, standardize = FALSE)
}

coefFunc <- function(mod, name){
    coefplot(mod, sort='magnitude',lambda='lambda.1se', intercept = FALSE) + 
        ggtitle(name) +
        theme_minimal()
}

predictFunc <- function(mod, x, data)data %>% bind_cols(predict = as.vector(predict(mod,x)))
residFunc <- function(data, y)data %>% mutate(residual = predict - y) %>% select(Geo_FIPS, residual)
MSEFunc <- function(mod)mod$cvm[mod$lambda==mod$lambda.1se]
ctSF <- function(residual) nyct %>% 
  left_join(residual %>% mutate(Geo_FIPS = as.character(Geo_FIPS)) %>% select(Geo_FIPS, residual)) %>% 
  select(Geo_FIPS, residual)

mapFunc <- function(name,ctSF){
  tm_shape(ctSF) +
    tm_fill(col = "residual",
                title= "Residual")+
    tm_borders(lwd = .4,col = "white") +
    tm_style("watercolor",frame = FALSE,
             title= name, title.size = 1) + 
    tm_scale_bar(position = c("right", "bottom"))
}

moransI <- function(ctSF){
  ctSP <- as(ctSF %>% filter(!Geo_FIPS %in%c(36005051600, 36061000100, 36085990100,36081107201, 36081091601), !is.na(residual)),"Spatial")
  w <- poly2nb(ctSP, row.names=ctSP$Geo_FIPS)
  ww <-  nb2listw(w, style='B')
  Moran <- moran(ctSP$residual, ww, n=length(ww$neighbours), S0=Szero(ww))
  Moran$I
}

## Create table
modList <- formulaDF %>% mutate(x = map(formula,~xFunc(.,modelDF)),
                                y = map(formula,~yFunc(.,modelDF)),
                                alpha = map_dbl(formula,~alphaFunc(.x,modelDF)),
                                mods = map2(x,y, ~modelFunc(.x,.y,alpha)),
                                coef = map2(mods, name, ~coefFunc(.x,.y)),
                                prediction = map2(mods, x,~predictFunc(.x,.y,modelDF)),
                                residual = map2(prediction,y, ~residFunc(.x,.y)),
                                MSE = map_dbl(mods, ~MSEFunc(.x)),
                                ctSF =map(residual,~ctSF(.x)),
                                residMap = map2(name,ctSF, ~mapFunc(.x,.y)),
                                moransI = map_dbl(ctSF, ~moransI(.x)),
                                lambda = map_dbl(mods, function(x)x$lambda.1se))

modList$mods[[1]] %>% coefplot(sort='magnitude',lambda='lambda.1se', intercept = FALSE, pointSize = 2) + 
  ggtitle("name") +
  theme_minimal()+
  theme(axis.text.y = element_text(size = 10))
  
modList$coef

modList$residMap

modList %>% select(name,MSE, moransI, alpha, lambda) %>% rename(models = name) %>% knitr::kable()

modList %>% select(name,MSE, moransI) %>% filter(name != "formulaCasesCI_Borough") %>% 
  ggplot(aes(moransI, MSE)) +geom_point(color = "Blue") + theme_minimal() + xlab("Moran's I")


tm_shape(modList$ctSF[[7]]) +
  tm_fill(col = "residual", title = "Residuals")+
  tm_borders(lwd = .4,col = "white") + 
  tm_style("watercolor",frame = FALSE, title= "CI + SVI + UHVI + Spatial Lag", title.size = 1) + tm_scale_bar(position = c("right", "bottom"))
