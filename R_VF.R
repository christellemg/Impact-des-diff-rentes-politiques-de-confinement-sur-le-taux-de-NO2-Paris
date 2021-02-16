

if (Sys.getenv("USERNAME")=="David"){
  setwd("C:\\Users\\David\\OneDrive\\1_Ecole\\HEC\\Msc\\Aut 2020\\Apprentissage Statistique\\Travail d'équipe")
}else if (Sys.getenv("USERNAME")=="Christelle"){
  setwd("~/HEC - MASTER/A2020/APPRENTISSAGE STATISTIQUE/PROJET")
}else 
{  setwd("/Users/quentintabourin/Desktop/HEC_A2020/M60603 - apprentissage statistiques")
}

#htmltools::img(src = knitr::image_uri(".\\Images\HEC Montreal.jpg"),
#             alt = 'logo',
#              style = 'position:absolute; top:0; right:25%;width:20vw%;height:auto; padding:10px;')

knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE,
                      fig.align = "center",
                      fig.height = 2)

# Pour la reproductabilité
set.seed(11118064)


library(dplyr)
library(ropenaq) 
library(lubridate)
library(purrr)
library(ggplot2)
library(grid)
library(zoo)
library(gridExtra)
library(randomForest)
library(caret)
library(sp)
library(leaflet)
library(reshape2)
library(readxl)
library(magrittr)
library(e1071)
library(kableExtra)
library(knitr)
library(bookdown)

# Traitement des données

mobility<- read.csv(file = "DATA/2020_FR_Region_Mobility_Report.csv",header =TRUE,encoding = "UTF-8")
mobility <- mobility %>% filter( sub_region_2=="Paris") %>% select(-c(country_region_code,country_region,sub_region_1,metro_area,iso_3166_2_code,census_fips_code))
mobility$date <- as.Date(mobility$date)
mobility <- mobility %>% select(-sub_region_2)
colnames(mobility)[1] <- "Date"

weather<- read.csv2(file = "DATA/Weather_Data.csv",header =TRUE,encoding = "UTF-8")
weather <- weather %>% select(Date, Direction.du.vent.moyen.10.mn,
                              Précipitations.dans.les.3.dernières.heures,
                              Vitesse.du.vent.moyen.10.mn,
                              Humidité,Pression.station,
                              Température...C.) %>% mutate(date= as.Date(Date)) %>% select(-Date)
weather <- weather[,c(7,6,1,2,3,4,5)]
cols <- 2:7
weather[,cols] = apply(weather[,cols], 2, function(x) as.numeric(as.character(x)))

weather <- weather %>% group_by(date) %>% summarise(Moy_dir_vent=mean(Direction.du.vent.moyen.10.mn,na.rm = TRUE),
                                                    Moy_pre= mean(Précipitations.dans.les.3.dernières.heures,na.rm = TRUE),
                                                    Moy_vit_vent= mean(Vitesse.du.vent.moyen.10.mn,na.rm = TRUE),
                                                    Moy_humitite= mean(Humidité,na.rm = TRUE),
                                                    Moy_pression= mean(Pression.station,na.rm = TRUE),
                                                    T_max= max(Température...C.,na.rm = TRUE),
                                                    T_min= min(Température...C.,na.rm = TRUE))
colnames(weather)[1] <- "Date"

confinement <- read_excel("DATA/ConfinementBIS.xlsx")
confinement$Date <- as.Date(confinement$Date)
colnames(confinement)[2] <- "Conf_level"

aq <- readRDS("DATA/Paris_aq.rds")

aq_keep <- aq %>% filter(location %in% c("FR04143","FR04071","FR04141"))
ggplot(data=aq_keep)+
  geom_point(mapping=aes(x=Date,y=value,color=location),alpha=0.35)+ 
  labs(y="Taux de NO2 (ppm)", x = "Date",color="Location")+
  ggtitle("Taux de NO2 horaire relevé à Paris\n selon 3 points de prélèvement en fonction du temps")+
  theme(legend.position = "top",
        legend.justification="left",
        legend.margin=ggplot2::margin(0,0,0,0),
        legend.box.margin=ggplot2::margin(-10,-10,-10,-10))

aq_keep <- aq_keep %>% filter(location =="FR04071")

aq_keep <- aq_keep %>% filter(value<200,value>0)
aq_keep$hour <- factor(hour(aq_keep$Date))

aq_keep$simpleDate <- as.Date(aq_keep$Date)
aq_final <- aq_keep  %>% group_by(simpleDate) %>% summarise(mean_no2=mean(value,na.rm = T))
colnames(aq_final)[1] <- "Date"

ggplot(data=aq_final)+geom_point(mapping=aes(x=Date,y=mean_no2),color="green")+ labs(y="Taux de NO2 (ppm)", x = "Date")+ ggtitle("Taux moyen quotidien de NO2 au point FR04071")

data <- merge(aq_final,weather,by = "Date",all.x=T)
data <- merge(data,confinement, by="Date",all.x=TRUE)
data$Conf_level[is.na(data$Conf_level)] <- 0
data <- merge(data,mobility, by="Date",all.x=T)

data$day <- day(data$Date)
data$weekday <- factor(weekdays(data$Date))
data$month <- factor(months(data$Date))
data$julian <- julian(data$Date, origin = min(data$Date))


ggplot(data)+
  geom_histogram(mapping = aes(x=mean_no2),color="green") + labs(y="# Observations", x = "NO2 moyen quotidien")+
  ggtitle("Valeurs de NO2 moyennes quotidienne")


jour_ferie <- read.csv("DATA/jours_feries_metropole.csv",header=FALSE)
jour_ferie <- jour_ferie %>% select(V1)
data$jour_ferie <- 0
data$jour_ferie[data$Date %in% as.Date(jour_ferie$V1)] <- 1
data$jour_ferie <- factor(data$jour_ferie)

data$no2_lag1 <- lag(data$mean_no2,1)
data$no2_lag7 <- lag(data$mean_no2,7)
data$no2_lag28 <- lag(data$mean_no2,28)
data <- data %>% filter(!is.na(no2_lag28))

ggplot(data=data)+geom_point(mapping=aes(y=mean_no2 - no2_lag1,x=Date),color="darkgreen")+ 
  ggtitle("Delta à la moyenne quotidienne de No2")+
  ylab("No2 à t - No2 à t-1")

data$Moy_dir_vent <- 1+ sin(data$Moy_dir_vent +pi/4)

data$hdd = sapply(data$T_min,FUN=function(x){ max(10-x,0)})
data$cdd = sapply(data$T_max,FUN=function(x){ max(x-15,0)})

data <- data %>% select(-c(T_max,T_min))

ggplot(data=data %>% filter(month %in% c("avril","février","juin")) )+geom_point(mapping=aes(y=mean_no2,x=cdd),color="green")+facet_grid(month~.)+ 
  labs(y="Taux moyen de NO2", x = "Température maximale linéarisée")+
  ggtitle("Relation entre le taux moyen de NO2 et\nla température maximale linéarisée pour 3 mois")  

data$Conf_level <- factor(data$Conf_level,ordered=TRUE)

# Modèles

#train test split

DF_1 <- data %>% select(-c(retail_and_recreation_percent_change_from_baseline,
                           grocery_and_pharmacy_percent_change_from_baseline,
                           parks_percent_change_from_baseline,
                           transit_stations_percent_change_from_baseline,
                           workplaces_percent_change_from_baseline,
                           residential_percent_change_from_baseline))

train_id_1 <- sample(1:nrow(DF_1),0.75*nrow(DF_1))
test_id_1 <- setdiff(1:nrow(DF_1),train_id_1)
train_df_1 <- DF_1[train_id_1,]
test_df_1 <- DF_1[test_id_1,]

DF_11 <- data %>% select(-c(retail_and_recreation_percent_change_from_baseline,
                            grocery_and_pharmacy_percent_change_from_baseline,
                            parks_percent_change_from_baseline,
                            transit_stations_percent_change_from_baseline,
                            workplaces_percent_change_from_baseline,
                            residential_percent_change_from_baseline,Conf_level))

train_id_11 <- sample(1:nrow(DF_11),0.75*nrow(DF_11))
test_id_11 <- setdiff(1:nrow(DF_11),train_id_11)
train_df_11 <- DF_11[train_id_11,]
test_df_11 <- DF_11[test_id_11,]

DF_2 <- na.omit(data)
train_id_2 <- sample(1:nrow(DF_2),0.75*nrow(DF_2))
test_id_2 <- setdiff(1:nrow(DF_2),train_id_2)
train_df_2 <- DF_2[train_id_2,]
test_df_2 <- DF_2[test_id_2,]

# Regression linéaire

lm_model <- lm(mean_no2~.,data=train_df_1 %>% select(-Date))

df_lm <- train_df_1 %>% select(mean_no2,Date)
df_lm$pred_lm <- lm_model$fitted.values
df_lm$diff_lm <- df_lm$mean_no2-df_lm$pred

ggplot(data= df_lm)+
  geom_point(mapping=aes(x=Date, mean_no2, colour="Observation"))+
  geom_point(mapping=aes(x=Date,y=pred_lm, colour="Prédiction"))+ 
  labs(y="Taux moyen de NO2", x = "Date")+
  ggtitle("Valeurs du taux de NO2 moyen observées et prédites par\nle modèle linéaire avec confinement")+
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.justification="left",
        legend.margin=ggplot2::margin(0,0,0,0),
        legend.box.margin=ggplot2::margin(-10,-10,-10,-10))

reslinplot <- ggplot(data= df_lm)+
  geom_point(mapping=aes(x=Date, diff_lm),color="blue")+
  ggtitle("Résidus du modèle linéaire en\nfonction du temps") + 
  labs(y="Résidus", x = "Date")

linvarIMP <- as.data.frame(varImp(lm_model))
linvarIMP <- add_rownames(linvarIMP, "variable")
linvarIMP <- linvarIMP[order(linvarIMP$Overall),]
linvarIMPplot <- ggplot(linvarIMP, aes(y = reorder(linvarIMP$variable,linvarIMP$Overall), x= linvarIMP$Overall)) + 
  geom_bar(stat='identity') +
  theme(axis.text.y = element_text( hjust = 1,size=5),
        legend.title = element_blank(),
        legend.position = "top",
        legend.justification="left",
        legend.margin=ggplot2::margin(0,0,0,0),
        legend.box.margin=ggplot2::margin(-10,-10,-10,-10))+
  labs(title = "Importance des variables\nmodèle linéaire", x = "Importance", y = "Variables" )  


grid.arrange(reslinplot,linvarIMPplot,ncol=2)

lm_model11 <- lm(mean_no2~.,data=train_df_11)

df_lm11 <- train_df_11 %>% select(mean_no2,Date)
df_lm11$pred_lm <- lm_model11$fitted.values
df_lm11$diff_lm <- df_lm11$mean_no2-df_lm11$pred


ggplot(data= df_lm11)+
  geom_point(mapping=aes(x=Date, mean_no2, colour="Observation"))+
  geom_point(mapping=aes(x=Date,y=pred_lm, colour="Prédiction"))+ 
  labs(y="Taux moyen de NO2", x = "Date")+
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.justification="left",
        legend.margin=ggplot2::margin(0,0,0,0),
        legend.box.margin=ggplot2::margin(-10,-10,-10,-10))+
  ggtitle("Valeurs du taux de NO2 moyen pour\nle modèle linéaire sans confinement")  

reslinplot11 <- ggplot(data= df_lm11)+
  geom_point(mapping=aes(x=Date, diff_lm),color="blue")+
  ggtitle("Résidus du modèle linéaire en\nfonction du temps") + 
  labs(y="Résidus", x = "Date")

linvarIMP11 <- as.data.frame(varImp(lm_model11))
linvarIMP11 <- add_rownames(linvarIMP11, "variable")
linvarIMP11 <- linvarIMP11[order(linvarIMP11$Overall),]
linvarIMPplot11 <- ggplot(linvarIMP11, aes(y = reorder(linvarIMP11$variable,linvarIMP11$Overall), x= linvarIMP11$Overall)) + geom_bar(stat='identity') +theme(axis.text.y = element_text( hjust = 1,size=5))+ 
  labs(title = "Importance des variables\nmodèle linéaire sans var. conf.", x = "Importance", y = "Variables" )  


grid.arrange(reslinplot11,linvarIMPplot11,ncol=2)

# SVM

# tuned_svm_2 <- tune.svm(mean_no2~.,data=train_df_1 %>% select(-Date),
#                       kernel="linear", 
#                       cost=seq(350,1000,50))
# saveRDS(file="tune_svm2.rds",tuned_svm_2)

tuned_svm <- readRDS("tune_svm1.rds")


#tuned_svm$best.performance**0.5

df_lm$pred_svm <- tuned_svm$best.model$fitted
df_lm$diff_svm <- df_lm$mean_no2-df_lm$pred_svm


p <- ggplot(data= df_lm)+
  geom_point(mapping=aes(x=Date, y=mean_no2, colour="Observation"))+
  geom_point(mapping=aes(x=Date,y=pred_svm,colour="Prédiction"))+
  ggtitle("Taux de NO2 moyen observées et prédites par le modèle SVM")  + 
  labs(y="Taux moyen de NO2", x = "Date") + 
  guides(shape = guide_legend(override.aes = list(size = 0.5)))+
  theme(legend.position = "top",
        legend.justification="left",
        legend.margin=ggplot2::margin(0,0,0,0),
        legend.box.margin=ggplot2::margin(-10,-10,-10,-10),
        axis.ticks.x = element_blank(), axis.title.x = element_blank(),
        axis.text.x = element_blank(),legend.title=element_text(size=5),
        legend.text = element_text(size=5))


p1 <- ggplot(data= df_lm)+
  geom_point(mapping=aes(x=Date, diff_svm),color="blue")+
  ggtitle("Résidus du modèle SVM linéaire en fonction du temps") + 
  labs(y="Résidus", x = "Date")

grid.arrange(p,p1,ncol=1)

# Random Forest

#tune_grid <- expand.grid(mtry = seq(3, 16, by = 1),node_size  = c(3:7))
# Mis en commentaire pour faciliter le Knit
# rf_gridsearch <-list()
# rmse_list <- list()
# rmse_final_list <- list()
# rsq_list <- list()
# for (i in seq(1,nrow(tune_grid))){
#    rf_gridsearch[[i]] <-  randomForest(mean_no2 ~ ., data=train_df_1%>% select(-Date),ntree=300, 
#                                        mtry=tune_grid$mtry[i], 
#                                        node_size=tune_grid$node_size[i],
#                                        importance=TRUE)
#    rmse_list[[i]] <- sqrt(rf_gridsearch[[i]]$mse)
#    rsq_list[[i]] <- tail(rf_gridsearch[[i]]$rsq,1)
#    rmse_final_list[[i]] <- tail(sqrt(rf_gridsearch[[i]]$mse),1)
#    print(paste("Rsqr:",rsq_list[[i]]))
#    print(paste("RMSE:",rmse_final_list[[i]]))
# }
# saveRDS(object = rf_gridsearch,file = "rf_model.rds")
rf_gridsearch <- readRDS("rf_model.rds")


rmse_final_list <- sapply(1:length(rf_gridsearch),FUN = function(x){
  min(rf_gridsearch[[x]]$mse)})


n_min=which(unlist(rmse_final_list)==min(rmse_final_list))
n_max=which(unlist(rmse_final_list)==max(rmse_final_list))

df_lm$pred_rf <- rf_gridsearch[[n_min]]$predicted
df_lm$diff_rf <- df_lm$mean_no2-df_lm$pred_rf

ggplot(data=df_lm)+ 
  geom_point(mapping=aes(x=Date,y=mean_no2, colour="Observation"))+ 
  geom_point(mapping=aes(x=Date,y=pred_rf, colour="Prédiction"))+ 
  labs(y="Taux moyen de NO2", x = "Date")+
  ggtitle("Taux de NO2 moyen observées et prédites par\nle modèle RF sans mobilité") +     theme(legend.title = element_blank(),legend.position = "top",
                                                                                                 legend.justification="left",
                                                                                                 legend.margin=ggplot2::margin(0,0,0,0),
                                                                                                 legend.box.margin=ggplot2::margin(-10,-10,-10,-10)) 
rfIMP <- as.data.frame(rf_gridsearch[[n_min]]$importance[,1])
rfIMP <- add_rownames(rfIMP, "variable")
colnames(rfIMP) <- c("variable","Overall")
rfIMP <- rfIMP[order(rfIMP$Overall),]
rfIMPplot <- ggplot(rfIMP, aes(y = reorder(rfIMP$variable,rfIMP$Overall), x= rfIMP$Overall)) + geom_bar(stat='identity') +theme(axis.text.y = element_text( hjust = 1,size=5))+ labs(title = "Importance des variables modèle\nforêts aléatoires", x = "Importance", y = "Variables" )  

resrf1 <- ggplot(data=df_lm)+ geom_point(mapping=aes(x=Date,y=diff_rf),color='blue') +
  ggtitle("Résidus du modèle forêts \naléatoires 1") + 
  labs(y="Résidus", x = "Date")

grid.arrange(resrf1,rfIMPplot,ncol=2)

densrf1 <- ggplot(data=df_lm)+ geom_density(mapping=aes(x=diff_rf),alpha=0.15,fill='green')+
  ggtitle("Densité des résidus modèle\nforêts aléatoires 1")+ 
  labs(y="Densité", x = "Résidus")

df_rmse= melt(data.frame("best"=rf_gridsearch[[n_min]]$mse**0.5, "worst"= rf_gridsearch[[n_max]]$mse**0.5, "id"=1:length(rf_gridsearch[[n_min]]$mse)),id.vars="id")
RMSEplot<- ggplot(data=df_rmse)+ 
  geom_point(mapping=aes(x=id,y=value,color=variable))+
  ggtitle("Évolution du RMSE pour\ndes modèle selon le\nnombre d'arbres dans la forêt")+
  theme(legend.title=element_blank(),
        legend.position = "top",
        legend.justification="left",
        legend.margin=ggplot2::margin(0,0,0,0),
        legend.box.margin=ggplot2::margin(-10,-10,-10,-10))+
  labs(y="RMSE", x = "Nombres d'arbres dans la forêt")


grid.arrange(densrf1,RMSEplot,ncol=2)

#tune_grid2 <- expand.grid(mtry = seq(3, 16, by = 2),
#                        node_size  = c(3:7))

# Ici c'est mis en commentaire seuelement pour pas que le Knit prenne 10 heures à rouler.
# rf_gridsearch2 <-list()
# rmse_list2 <- list()
# rmse_final_list2 <- list()
# rsq_list2 <- list()
# for (i in seq(1,nrow(tune_grid2))){
#    rf_gridsearch2[[i]] <-  randomForest(mean_no2 ~ ., data=train_df_2%>% select(-Date),ntree=300, 
#                                        mtry=tune_grid2$mtry[i], 
#                                        node_size=tune_grid2$node_size[i],
#                                        importance=TRUE)
#    rmse_list2[[i]] <- sqrt(rf_gridsearch2[[i]]$mse)
#    rsq_list2[[i]] <- tail(rf_gridsearch2[[i]]$rsq,1)
#    rmse_final_list2[[i]] <- tail(sqrt(rf_gridsearch2[[i]]$mse),1)
#    print(rsq_list2[[i]])
#    print(rmse_final_list2[[i]])
# }
# 

# saveRDS(file="rf_2.rds",rf_gridsearch2)
rf_gridsearch2 <- readRDS("rf_2.rds")

rmse_final_list2 <- sapply(1:length(rf_gridsearch2),FUN = function(x){
  min(rf_gridsearch2[[x]]$mse)})


n_min2=which(unlist(rmse_final_list2)==min(unlist(rmse_final_list2)))
n_max2=which(unlist(rmse_final_list2)==max(unlist(rmse_final_list2)))


train_df_2$pred_rf_2 <- rf_gridsearch2[[n_min2]]$predicted
train_df_2$diff <- train_df_2$mean_no2-train_df_2$pred_rf_2



ggplot(data=train_df_2)+
  geom_point(mapping=aes(x=Date,y=mean_no2, colour="Observation"))+
  geom_point(mapping=aes(x=Date,y=pred_rf_2, colour="Prédiction"))+ 
  labs(y="Taux moyen de NO2", x = "Date")+
  ggtitle("Valeurs du taux de NO2 moyen observées et prédites par\nle modèle forêt aléatoire avec mobilité") +   theme(legend.title = element_blank(),legend.position = "top",
                                                                                                                       legend.justification="left",
                                                                                                                       legend.margin=ggplot2::margin(0,0,0,0),
                                                                                                                       legend.box.margin=ggplot2::margin(-10,-10,-10,-10))

rfIMP2 <- as.data.frame(rf_gridsearch2[[n_min2]]$importance[,1])
rfIMP2 <- add_rownames(rfIMP2, "variable")
colnames(rfIMP2) <- c("variable","Overall")
rfIMP2 <- rfIMP2[order(rfIMP2$Overall),]
ggplot(rfIMP2, aes(y = reorder(rfIMP2$variable,rfIMP2$Overall), x= rfIMP2$Overall),colour='blue') + geom_bar(stat='identity') +theme(axis.text.y = element_text( hjust = 1,size=5))+ labs(title = "Importance des variables modèle forêt aléatoire\navec mobilité", x = "Importance", y = "Variables" )  

# Comparaison des modèles

#modèle linéaire avec variable confinement

predict_test_lm = predict(lm_model, newdata = test_df_1 )
RMSE_test_lm = sqrt(mean((test_df_1$mean_no2 - predict_test_lm)^2))

#modèle linéaire sans variable confinement
predict_test_lm11 = predict(lm_model11, newdata= test_df_11)
RMSE_test_lm11 = sqrt(mean((test_df_11$mean_no2 - predict_test_lm11)^2))

#modèle svm 
predict_test_svm = predict(tuned_svm$best.model, newdata= test_df_1)
RMSE_test_svm = sqrt(mean((test_df_1$mean_no2 - predict_test_svm)^2))

#modèle forêt aléatoire 1
modelrf1 = randomForest(mean_no2 ~ ., data=train_df_1%>% select(-Date),ntree=300, 
                        mtry=11, 
                        node_size=5,
                        importance=TRUE)
predict_test_rf1 = predict(modelrf1, newdata = test_df_1 %>% select(-Date))
RMSE_test_rf1 = sqrt(mean((test_df_1$mean_no2 - predict_test_rf1)^2))

#modèle forêt aléatoire 2
modelrf2 = randomForest(mean_no2 ~ ., data=train_df_2%>% select(-c(Date,pred_rf_2, diff)),ntree=300, 
                        mtry=13, 
                        node_size=4,
                        importance=TRUE)

predict_test_rf2 = predict(modelrf2, newdata = test_df_2 %>% select(-Date))
RMSE_test_rf2 = sqrt(mean((test_df_2$mean_no2 - predict_test_rf2)^2))

#compilation RMSE train et test
RMSE_train = c(8.31,8.97,8.61,9.32, 7.23)
RMSE_test = c(RMSE_test_lm,RMSE_test_lm11,RMSE_test_svm,RMSE_test_rf1,RMSE_test_rf2)
var_import = c("vitesse moyenne du vent et valeur de la veille du taux de NO2", "vitesse moyenne du vent et valeur de la veille du taux de NO2",
               "-", "valeur de la veille du taux de NO2", "température minimale et valeur de la veille du taux de NO2")
mat_resultat = as.data.frame(rbind(RMSE_train, RMSE_test) )
mat_resultat <- mat_resultat %>% rename(lineaire_avec_confinement = V1 , lineaire_sans_confinement = V2 , SVM = V3 , RF_sans_mobilite = V4, RF_avec_mobilite = V5)
mat_resultat <- round(mat_resultat, 2)
mat_resultat <- rbind(mat_resultat, var_import)
rownames(mat_resultat) <- c("RMSE_train","RMSE_test","variables_importantes")

mat_resultat%>%
  kbl() %>%
  kable_styling(font_size = 12)


# Annexes

alimentationplot <- ggplot(data=data)+geom_point(mapping=aes(x=data$grocery_and_pharmacy_percent_change_from_baseline, y=data$mean_no2),colour="green")+
  labs(y="Taux NO2 moyen", x = "Variation déplacements vers alim. et pharmacie") +ylim(0,80) +
  ggtitle("Taux moyen de NO2 selon la variation\ndéplacements vers alim. ou phar.")

travailplot <- ggplot(data=data)+geom_point(mapping=aes(x=data$workplaces_percent_change_from_baseline, y=data$mean_no2),colour="green")+labs(y="Taux NO2 moyen", x = "Variation déplacements vers travail")+ylim(0,80)+ggtitle("Taux moyen de NO2 selon la variation\ndéplacements vers travail")+theme(strip.text = element_text(size = 5))

grid.arrange(alimentationplot,travailplot,ncol=2)

ggplot(data=data) + 
  geom_point(mapping=aes(x=data$Moy_vit_vent, y=data$mean_no2)) + labs(x="Vitesse moyenne du vent", y = "Concentration moyenne de NO2") + 
  ggtitle("Variation du taux moyen de NO2 en fonction de la vitesse moy. du vent")+
  theme(strip.text = element_text(size = 5))+ guides(shape = guide_legend(override.aes = list(size = 0.5)))


ggplot(aq_keep )+geom_point(mapping = aes(x=Date,y=value,color=hour))+ 
  labs(y="Taux de NO2 (ppm)", x = "Date")+
  ggtitle("Taux de NO2 horaire au point FR04071 en fonction du temps")+ 
  theme(legend.position = 'none')

ggplot(aq_keep)+geom_density(mapping = aes(x=value,fill=hour),alpha=0.15) + labs(y="Densité", x = "Taux de NO2")+ggtitle("Densité pour les valeurs de NO2\nen fonction de l'heure de la mesure au point FR04071")+theme(legend.position="none")

summary(lm_model)
summary(lm_model11)
summary(tuned_svm$best.model)
rf_gridsearch[[n_min]]
rf_gridsearch2[[n_min2]]









