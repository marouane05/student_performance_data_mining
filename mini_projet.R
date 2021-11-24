library(ggplot2)
library(GGally)
library(scales)
library(dplyr)
library(rpart)
library(rpart.plot)
library(lattice)
library(DMwR)
library(randomForest)
library(e1071)


#importation des données

read.csv("C:/Users/DS/Desktop/Les études/MASTER IBN TOFAIL/S2/data mining/micro projet/xAPI-Edu-Data.csv")

Student_data <- read.csv("C:/Users/DS/Desktop/Les études/MASTER IBN TOFAIL/S2/data mining/micro projet/xAPI-Edu-Data.csv")

#structure des données
str(Student_data) 

summary(Student_data)

#conversion des données de type caractère à des facteurs

Student_data <- as.data.frame(unclass(Student_data),stringsAsFactors = TRUE)
summary(Student_data)


#Visualisation des données sous forme PLOT

#Création d'un bar plot pour montrer le nombre de la population concerné selon le sex  

ggplot(Student_data, aes(x=gender)) +
geom_bar(fill="blue")

ggsave(filename="C:/Users/DS/Desktop/Les études/MASTER IBN TOFAIL/S2/data mining/micro projet/plot1.png",plot=last_plot())

#Création d'un bar plot pour montrer le nombre de la population concernée selon les pays 

ggplot(Student_data, aes(x =NationalITy))+
  geom_bar(fill="green")

ggsave(filename="C:/Users/DS/Desktop/Les études/MASTER IBN TOFAIL/S2/data mining/micro projet/plot2_population_country.png",plot=last_plot())


#Histograms 

#raise hand
ggplot(Student_data, aes(x = raisedhands)) + geom_histogram(color = "black",bins="20")+
  scale_x_continuous(breaks = seq(0,100,5))
  labs(x = "Raised Hands", y = "Student Count")
  
  ggsave(filename="C:/Users/DS/Desktop/Les études/MASTER IBN TOFAIL/S2/data mining/micro projet/plot3_raise_hand.png",plot=last_plot())
  
  
#Visited Resources
  
  ggplot(Student_data, aes(x = VisITedResources)) + geom_histogram(color = "black",bins="20") +
    scale_x_continuous(breaks = seq(0,100,5)) + 
    labs(x = "Visited Resources", y = "Student Count")
  
  ggsave(filename="C:/Users/DS/Desktop/Les études/MASTER IBN TOFAIL/S2/data mining/micro projet/plot4_visited_resource.png",plot=last_plot())
  
#Discussion
  
  ggplot(Student_data, aes(x = Discussion)) + geom_histogram(color = "black",bins="20") +
    scale_x_continuous(breaks = seq(0,100,5)) + 
    labs(x = "Discussion Participation", y = "Student Count")
  
  ggsave(filename="C:/Users/DS/Desktop/Les études/MASTER IBN TOFAIL/S2/data mining/micro projet/plot5_discussion.png",plot=last_plot())
  
  
  
  #analyse 
  
  
  #comparaison des étudiants par domaine selon le sex

  
  ggplot(Student_data, aes(x = Topic, fill = gender, by=gender ,y = after_stat(prop),
                           label = scales::percent(after_stat(prop), accuracy = 1) )) + geom_bar(stat = "prop", position = position_dodge(.9)) +
    geom_text(
      aes(y = after_stat(prop) - .01), stat = "prop", 
      position = position_dodge(.9), vjust = "top"
    ) +
    scale_y_continuous(labels = percent)
   
  
  ggsave(filename="C:/Users/DS/Desktop/Les études/MASTER IBN TOFAIL/S2/data mining/micro projet/plot6_Orientation_par_Gender.png",plot=last_plot())
  
  
  
  
  
  
  
  
  # comparaison des domaines des études selon chaque pays (on voit au Maroc il y a une grande orientation au Biologie, Kuwait IT)
  ggplot(Student_data, aes(x = NationalITy, fill =Topic )) + geom_bar() +
    labs(x = "Topic", y = "Nationality") 
    scale_y_continuous(breaks = seq(0,100,4))
    
    ggsave(filename="C:/Users/DS/Desktop/Les études/MASTER IBN TOFAIL/S2/data mining/micro projet/plot6_Orientation_par_pays.png",plot=last_plot())
    
    
    
    
    
    
    
    #Analyse des absences selon le sex femme , homme
  
    ggplot(Student_data, aes(x = gender, fill = StudentAbsenceDays, by=StudentAbsenceDays , y = after_stat(prop),
                             label = scales::percent(after_stat(prop), accuracy = 1), )) + 
      geom_bar(stat = "prop", position = position_dodge(.9)) +
      geom_text(
        aes(y = after_stat(prop) - .01), stat = "prop", 
        position = position_dodge(.9), vjust = "top"
      ) +
      scale_y_continuous(labels = percent)
    
    ggsave(filename="C:/Users/DS/Desktop/Les études/MASTER IBN TOFAIL/S2/data mining/micro projet/plot6_Absense_by_Gender.png",plot=last_plot())
    
    
    
    
    
    
    
    #Analyse des absences selon le pays
    
    ggplot(Student_data, aes(x = NationalITy, fill = StudentAbsenceDays)) + geom_bar() +
      labs(x = "Nationality", y = "Student Count")
    
    ggsave(filename="C:/Users/DS/Desktop/Les études/MASTER IBN TOFAIL/S2/data mining/micro projet/plot6_Absense_by_Nationality.png",plot=last_plot())
    
    
    
    
    
    #Semestre qui a plus de participation "raise hand" dans les cours
    
    ggplot(Student_data, aes(x = Semester, y = raisedhands)) + geom_boxplot()
    ggsave(filename="C:/Users/DS/Desktop/Les études/MASTER IBN TOFAIL/S2/data mining/micro projet/plot7_Raise_Hand_By_Semester.png",plot=last_plot())
    
    
    
    #Relation entre participation dans les cours "raise hand" et la visite des resources
    
    ggplot(Student_data, aes( x = raisedhands, y = VisITedResources)) + geom_point() +
      geom_smooth(method = "lm")
   
    ggsave(filename="C:/Users/DS/Desktop/Les études/MASTER IBN TOFAIL/S2/data mining/micro projet/plot8_Relation_between_raiseHand_visit_Resources.png",plot=last_plot())
    
    
    
    #Densité plot
    
    ggplot(data = Student_data, aes(x = raisedhands, color = gender)) + geom_density(alpha = 0.4)+
   
      scale_color_manual(values = c("#868686FF", "#EFC000FF"))+
      scale_fill_manual(values = c("#868686FF", "#EFC000FF"))
    ggsave(filename="C:/Users/DS/Desktop/Les études/MASTER IBN TOFAIL/S2/data mining/micro projet/plot9_Density_Fem_Male.png",plot=last_plot())
    
    
    ggplot(data = Student_data, aes(x = raisedhands, color = Semester)) + geom_density()
    ggsave(filename="C:/Users/DS/Desktop/Les études/MASTER IBN TOFAIL/S2/data mining/micro projet/plot10_Density_Hand_Semester.png",plot=last_plot())
    
    
    
    #Analyse pour trouver le nombre Des étudiants by catégorie sexuel et nationale
    
    tile.map <- Student_data %>% group_by(gender, NationalITy) %>%
      summarise(Count = n()) %>% arrange(desc(Count))
    
    tile.map
    
  ggplot(data= tile.map, aes(x = gender, NationalITy, fill = Count)) + geom_tile()
    
    
    
    
    
    
#Modèle prédective
  
  #Linéaire
  
  #on va créer un modèle lineaire 
  
  linear_model <- lm(Discussion~raisedhands,data=Student_data)
  linear_model 
 
  
  
  new_raisedhands <- data.frame(raisedhands=c(110,150,200,250))
  new_raisedhands
  
  #prédictions pour les futures valeurs
  
  linear_prediction <- predict(linear_model,newdata = new_raisedhands)
  
  
  #Tree
  
  


 
  rt.a1 <-rpart(raisedhandsDiscussion, data =Student_data)
  rt.a2 <-rpart(raisedhands~VisITedResources, data =Student_data)
  
  
  
   prp(rt.a1,extra=101,box.col="orange",split.box.col="grey")
  
  ggsave(filename="C:/Users/DS/Desktop/Les études/MASTER IBN TOFAIL/S2/data mining/micro projet/tree_prediction.png",plot=last_plot())
  
  prp(rt.a2,extra=101,box.col="orange",split.box.col="grey")
  
  rt.a3 <-rpart(raisedhands~Discussion, data =Student_data)
  
  prp(rt.a3,extra=101,box.col="orange",split.box.col="grey")
  
  tree_prediction<-predict(rt.a3, Student_data)
  
  
  #comparaison entre Tree et Linéaire prédiction
  
  dg<- data.frame(linear=linear_prediction,
                  tree=tree_prediction,y_val=Student_data$raisedhands)
  
  dg
  
  ggplot(dg,aes(x=linear, y=y_val)) + geom_point() +
    geom_abline(slope=1,intercept=0,color="red") + ggtitle("Linear
Model")
  ggsave(filename="C:/Users/DS/Desktop/Les études/MASTER IBN TOFAIL/S2/data mining/micro projet/gplot11_Lin_prediction.png",plot=last_plot())
  
  
  ggplot(dg,aes(x=tree_prediction,y=y_val)) + geom_point() +
    geom_abline(slope=1,intercept=0,color="red") + ggtitle("Regression
Tree")
  
  ggsave(filename="C:/Users/DS/Desktop/Les études/MASTER IBN TOFAIL/S2/data mining/micro projet/gplot12_tree_prediction.png",plot=last_plot())
  
  
  
  
  str(Student_data)
  
  
  
  ##########################################################################################################
  
  ## Decision trees
  
  # Predictive modelling
  
  
  ## Splitting data into train and cross-validation sets.
  library(caTools)
 
  #spliting
  
  set.seed(55)
  split <- sample.split(Student_data$Class, SplitRatio = 0.75)
  

  
  train <- subset(Student_data, split == T)
  nrow(train)
  cv <- subset(Student_data, split == F)
  cv
  nrow(cv)
  #élaboration de notre modèle
  tree.model <- rpart(Class ~ ., data = train, method = "class", minbucket = 1)
  prp(tree.model) 
  
  #faire notre prédiction
  tree.predict <- predict(tree.model, cv, type = "class")
  table(cv$Class, tree.predict)
  
  confMat <- table(cv$Class, tree.predict)
  
  accuracy <- sum(diag(confMat))/sum(confMat)
  
  
  #calcule d'accuracy 
  
  accuracy
  
  
  #Random forest ************************************
  
  
  set.seed(10)
  
  rf.model <- randomForest(Class ~ .- SectionID , data = train, importance = TRUE,
                           ntree = 2000, nodesize = 20)
  
  rf.predict <- predict(rf.model, cv)
  confMat_Random_Forest <- table(cv$Class, rf.predict)
  varImpPlot(rf.model)
  
  accuracy_random_forest <- sum(diag(confMat_Random_Forest))/sum(confMat_Random_Forest)
  accuracy_random_forest
  
  #
  
  str(Student_data)
  
  
  
  
  ## Suppost Vector Machines


  
  svm.model <- svm(Class ~ ., data = train, kernel = "radial", cost = 10, gamma = 0.15)
  svm.predict <- predict(svm.model, cv)
 
  

  
  confMat_vector <- table(cv$Class, svm.predict)
  
  accuracy_vector <- sum(diag(  confMat_vector))/sum(  confMat_vector)
  
  
  #calcule d'accuracy 
  
  accuracy_vector
  
  
  
  
  #combinaison**
  
  results <- data.frame(tree = tree.predict,  rf = rf.predict,
                        svm = svm.predict,
                        actual.class = cv$Class, final.prediction = rep("-",nrow(cv)))
  
  results
  
  
  getmode <- function(x) {
    unique.x <- unique(x)
    unique.x[which.max(tabulate(match(x, unique.x)))]
  }
  
  results$final.prediction <- apply(results, 1, getmode)
  
  confMat_final <- table(results$actual.class, results$final.prediction)
  
  accuracy_final <- sum(diag(  confMat_final))/sum(  confMat_final)
  
  accuracy_final
  
 