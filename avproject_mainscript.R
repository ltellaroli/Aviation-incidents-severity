#installs required paackages if not already available
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggridges)) install.packages("ggridges", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(earth)) install.packages("earth", repos = "http://cran.us.r-project.org")
if(!require(mda)) install.packages("mda", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")


#Data import and preliminary Wrangling
dl <- tempfile()  #creates temporary file
download.file("https://raw.githubusercontent.com/ltellaroli/Aviation-incidents-severity/master/accident_severity_data.csv", dl)  #downloads data from my github account
raw_data<-read.csv(dl , header=TRUE, stringsAsFactors = FALSE  ) #import downloaded data
#convert severity and accident type as factors
accidents<-raw_data%>%mutate(Severity=factor(Severity), Accident_Type_Code=factor(Accident_Type_Code))
#get the severity levels and converts sverity into an ordered factor
severity_levels<-levels(accidents$Severity)
accidents<-accidents%>%mutate(Severity=ordered(as.character(Severity), levels=c(severity_levels[1], severity_levels[3], severity_levels[4], severity_levels[2]), labels=c("Catastrophic", "Dangerous", "Major", "Minor")))
#prints to console the numer of NAs in the data
print(c("Numbers of NAs in the data equal to", sum(is.na(accidents))))
#set the seed for the next command involving a random sampling
set.seed(1, sample.kind="Rounding")
#split the data in train and test sets, with 70% of data into the train set and 30% into the test set
index<-createDataPartition(y=accidents$Safety_Score, times=1, p=0.3, list=FALSE )
test_set<-accidents[index,]
train_set<-accidents[-index,]

rm(raw_data) #removes original non splitted dataframes, which are no more needed

#Data Exploration

#computes the proportions of the seveirty levels in the training data and store them in a named vector
severity_levels<-levels(accidents$Severity)
train_set_proportions<-c(mean(train_set$Severity==severity_levels[1]), mean(train_set$Severity==severity_levels[2]), mean(train_set$Severity==severity_levels[3]), mean(train_set$Severity==severity_levels[4]))
names(train_set_proportions)<-severity_levels
#creates a barplot to visualize the number of reports for each sverity level
proportions_plot<-train_set%>%ggplot(aes(x=Severity, fill=Severity))+geom_bar(stat= "count")+theme(legend.position = "none")


#plots histogram showing the distribution of safety score in training data
score_distr<-train_set%>%ggplot(aes(Safety_Score))+geom_histogram(binwidth = (range(train_set$Safety_Score)[2]-range(train_set$Safety_Score)[1])/10)+ylab("Number of reports")
#creates boxplot of safety scores grouped by Severity of the accident
score_box<-train_set%>%ggplot(aes(x=Severity, y=Safety_Score, fill=Severity))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90)) + scale_fill_brewer(palette="Set2")+theme(legend.position = "none")
#plots density functions of safety scores grouped by Severity 
score_multiple_distrib<-train_set%>%ggplot(aes(x=Safety_Score, y=fct_rev(Severity) , fill=Severity))+geom_density_ridges()+theme(legend.position = "none")+ylab("Severity")
#generates qqplots of scaled safety cores distributions vs theoretical normal distributions, faceting by severity
score_multiple_qq<-train_set%>%ggplot(aes(sample=scale(Safety_Score)))+geom_qq()+geom_abline()+expand_limits(x=c(-5, 5), y=c(-5, 5))+facet_grid(rows=vars(Severity))


#creates barplot showing number of accidents groupig by accident_type and faceting by Severity
Type_distr<-train_set%>%ggplot(aes(x=Accident_Type_Code, fill=Accident_Type_Code))+geom_bar(stat = "count")+theme(legend.position = "none")+facet_grid(rows=vars(Severity))
#because previous plot showed significant type to type variability in the severity of outcomes we plot safety scores grouped by Severity of the accident faceted by accident type 
score_box_faceted<-train_set%>%ggplot(aes(x=Severity, y=Safety_Score, fill=Severity))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90)) + scale_fill_brewer(palette="Set2") + facet_grid(cols=vars(Accident_Type_Code))+theme(legend.position = "none")+ggtitle("Faceting by Accident type")

#plots histogram showing the distribution of days_since_inspection in training data
days_since_insp_distr<-train_set%>%ggplot(aes(Days_Since_Inspection))+geom_histogram(binwidth = (range(train_set$Days_Since_Inspection)[2]-range(train_set$Days_Since_Inspection)[1])/10)+ylab("Number of reports")
#plots density functions of days since inpection grouped by severity
days_since_insp_multiple_distrib<-train_set%>%ggplot(aes(x=Days_Since_Inspection, y=fct_rev(Severity) , fill=Severity))+geom_density_ridges()+theme(legend.position = "none")+ylab("Severity")
#generates qqplots of scaled days since inspection distributions vs theoretical normal distributions, faceting by severity
days_since_insp_qq<-train_set%>%ggplot(aes(sample=scale(Days_Since_Inspection)))+geom_qq()+geom_abline()+expand_limits(x=c(-5, 5), y=c(-5, 5))+facet_grid(rows=vars(Severity))  
days_since_insp_qq_byType<-train_set%>%ggplot(aes(sample=scale(Days_Since_Inspection)))+geom_qq()+geom_abline()+expand_limits(x=c(-5, 5), y=c(-5, 5))+facet_grid(rows=vars(Severity), cols=vars(Accident_Type_Code))+ggtitle("Faceted by accident type and severity of outcome")  # even when using facetig by sevrity AND type situation does not change
#boxplots of days sinnce last inspection grouped by severity
days_since_insp_box<-train_set%>%ggplot(aes(x=Severity, y=Days_Since_Inspection, fill=Severity))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90)) + scale_fill_brewer(palette="Set1")+facet_grid(cols=vars(Accident_Type_Code))+theme(legend.position = "none")+ggtitle("Faceting by Accident type")


#distribution of complaints received by personnel data
complaints_distr<-train_set%>%ggplot(aes(Total_Safety_Complaints))+geom_histogram(binwidth = (range(train_set$Days_Since_Inspection)[2]-range(train_set$Days_Since_Inspection)[1])/10)+ylab("Number of reports")
#plots density functions of complaints grouped by severity
complaints_multiple_distrib<-train_set%>%ggplot(aes(x=Total_Safety_Complaints, y=fct_rev(Severity) , fill=Severity))+geom_density_ridges()+theme(legend.position = "none")+ylab("Severity")
#generates qqplots of scaled days since inspection distributions vs theoretical normal distributions, faceting by severity, to detect differences in the various ditributions shapes
complaints_qq<-train_set%>%ggplot(aes(sample=scale(Total_Safety_Complaints)))+geom_qq()+geom_abline()+expand_limits(x=c(-5, 5), y=c(-5, 5))+facet_grid(rows=vars(Severity))
#jittered points plot showing Complaints grouped by Severity and faceted by accident type
complaints_point<-train_set%>%ggplot(aes(x=Severity, y=Total_Safety_Complaints,  color=Severity))+geom_jitter(alpha=0.7)+theme(axis.text.x = element_text(angle = 90)) + scale_fill_brewer(palette="Set1")+facet_grid(cols=vars(Accident_Type_Code))+theme(legend.position = "none")+ggtitle("Faceting by Accident type")

#histogram of control metric distribution in the training data
control_distr<-train_set%>%ggplot(aes(Control_Metric))+geom_histogram(binwidth = (range(train_set$Control_Metric)[2]-range(train_set$Control_Metric)[1])/10)+ylab("Number of reports")
#plots density functions of control metric grouped by severity
control_multiple_distrib<-train_set%>%ggplot(aes(x=Control_Metric, y=fct_rev(Severity) , fill=Severity))+geom_density_ridges()+theme(legend.position = "none")+ylab("Severity")
#plots boxplots of control metric grouped by severity and faceted by accident type
control_box<-train_set%>%ggplot(aes(x=Severity, y=Control_Metric, fill=Severity))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90)) + scale_fill_brewer(palette="Set1")+facet_grid(cols=vars(Accident_Type_Code))+ggtitle("Faceting by Accident type")


#histogram of turbulence induced Gs distribution in the training data
turb_distr<-train_set%>%ggplot(aes(Turbulence_In_gforces))+geom_histogram(binwidth = (range(train_set$Turbulence_In_gforces)[2]-range(train_set$Turbulence_In_gforces)[1])/10)+ylab("Number of reports")
#plots density functions of turbulence induced Gs grouped by severity
turb_multiple_distrib<-train_set%>%ggplot(aes(x=Turbulence_In_gforces, y=fct_rev(Severity) , fill=Severity))+geom_density_ridges()+theme(legend.position = "none")+ylab("Severity")

turb_box<-train_set%>%ggplot(aes(x=Severity, y=Turbulence_In_gforces,  fill=Severity))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90)) + scale_fill_brewer(palette="Set2")+facet_grid(cols=vars(Accident_Type_Code))+ggtitle("Faceting by Accident type")

#histogram of cabin temperature distribution in the training data
CabTemp_distr<-train_set%>%ggplot(aes(Cabin_Temperature))+geom_histogram(binwidth = (range(train_set$Cabin_Temperature)[2]-range(train_set$Cabin_Temperature)[1])/10)+ylab("Number of reports")
#plots density functions of cabin temperature grouped by severity
CabTemp_multiple_distrib<-train_set%>%ggplot(aes(x=Cabin_Temperature, y=fct_rev(Severity) , fill=Severity))+geom_density_ridges()+theme(legend.position = "none")+ylab("Severity")
#plots boxplots of control metric grouped by severity and faceted by accident type
CabTemp_box<-train_set%>%ggplot(aes(x=Severity, y=Cabin_Temperature, fill=Severity))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90)) + scale_fill_brewer(palette="Set1")+facet_grid(cols=vars(Accident_Type_Code))+ggtitle("Faceting by Accident type")

#histogram of Max Altitude Reached distribution in the training data
Alt_distr<-train_set%>%ggplot(aes(Max_Elevation))+geom_histogram(binwidth = (range(train_set$Max_Elevation)[2]-range(train_set$Max_Elevation)[1])/10)+ylab("Number of reports")
#plots density functions of maximum altitude reached grouped by severity
Alt_multile_distr<-train_set%>%ggplot(aes(x=Max_Elevation, y=fct_rev(Severity) , fill=Severity))+geom_density_ridges()+theme(legend.position = "none")+ylab("Severity")
#plots boxplots of max altitude reached grouped by severity and faceted by accident type
Alt_box<-train_set%>%ggplot(aes(x=Severity, y=Max_Elevation, fill=Severity))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90)) + scale_fill_brewer(palette="Set1")+facet_grid(cols=vars(Accident_Type_Code))+ggtitle("Faceting by Accident type")


#histogram of number of violations distribution in the training data
Violations_distr<-train_set%>%ggplot(aes(Violations))+geom_histogram(bins=6)+ylab("Number of reports")
#boxplot of number of vioolations grouped by Severity and faceted by accident type
Violations_box<-train_set%>%ggplot(aes(x=Severity, y=Violations, fill=Severity))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90)) + scale_fill_brewer(palette="Set1")+facet_grid(cols=vars(Accident_Type_Code))+ggtitle("Faceting by Accident type")

#initialize list to store following plots
plots_list<-vector(mode="list")
#for cycle to create scatterplots for each variable to variable combination, setting point's color based on the Severity, to search for clusters of data 
for (i in seq(1 : length(colnames(train_set)))) {  
  plots_list2<-vector(mode="list")
  for (j in seq(1 : length(colnames(train_set)))) {
    xvalue=train_set[,i]
    yvalue=train_set[,j]
    plotdata<-data.frame(xvalue, yvalue, Severity=train_set$Severity, stringsAsFactors=FALSE)
   plots_list2[[j]]<-plotdata%>%ggplot(aes(x=xvalue, y=yvalue, color=Severity))+geom_point()+xlab(colnames(test_set)[i])+ylab(colnames(test_set)[j])
  }
  names(plots_list2)<-colnames(train_set)
  plots_list[[i]]<-plots_list2
 }
names(plots_list)<-colnames(train_set)

#the control metric higlights a negative correlation between turbulence an control metric, which is expected, but gives no info to predict the severity
control_metric_vs_turbulence<-plots_list[["Control_Metric"]][["Turbulence_In_gforces"]]  
#the Days since inspection vs safety core plots, with jittered points, let us see clusters of data with ostly the same severity at various Safety_score-Days_Since_Inspection values
safetyscore_vs_days_inspection<-plots_list[["Safety_Score"]][["Days_Since_Inspection"]]+geom_jitter(alpha=0.5)+ggtitle("Jittered Scatterplot of Days since inspection vs Safety score")
 
#we reproduce the previous plot creating method adding faceting by accident type
plots_list_faceted<-vector(mode="list")

for (i in seq(1 : length(colnames(train_set)))) {  
  plots_list2_faceted<-vector(mode="list")
  for (j in seq(1 : length(colnames(train_set)))) {
    xvalue=train_set[,i]
    yvalue=train_set[,j]
    plotdata<-data.frame(xvalue, yvalue, Severity=train_set$Severity, code=train_set$Accident_Type_Code, stringsAsFactors=FALSE)
    plots_list2_faceted[[j]]<-plotdata%>%ggplot(aes(x=xvalue, y=yvalue, color=Severity))+geom_point()+xlab(colnames(test_set)[i])+ylab(colnames(test_set)[j])+facet_grid(rows=vars(code))
  }
  names(plots_list2_faceted)<-colnames(train_set)
  plots_list_faceted[[i]]<-plots_list2_faceted
}
names(plots_list_faceted)<-colnames(train_set)
#In this control metric vs safety score plot we see that for accident types 1, 7, 5, 4, 2 we can see clusters of data with the same Severity
score_vs_control_faceted<-plots_list_faceted[["Safety_Score"]][["Control_Metric"]]
#we see some clusters also in this safety score - turbulence plot for accidents type 2, 4 and 7
score_vs_turb_faceted<-plots_list_faceted[["Safety_Score"]][["Turbulence_In_gforces"]]
#we see some clusters also in this control metric - turbulence plot for accidents type 1, 5 and 4
control_vs_turb<-faceted<-plots_list_faceted[["Control_Metric"]][["Turbulence_In_gforces"]]


#applying PCA once for every accident type to see if Severity can be predicted with principal components

#creating a matrix containing only numeric predictor as columns, not including Severity, accidentID, Accident type and cabin temperature (we have seen that cabin temperature boxplots are almost equal for differents levels of severity)
predictors_matrix<-as.matrix(train_set%>%select(-Severity, -Accident_ID, -Accident_Type_Code, -Cabin_Temperature))  
#computing pca
pca<-prcomp(predictors_matrix)

#creating boxplots for every principal components, grouped by severity and faceted by accident type
PC2_plot<-data.frame(pca$x[,1:8], Severity=train_set%>%.$Severity , type=train_set%>%.$Accident_Type_Code)%>%ggplot(aes(Severity, PC2, color=Severity))+geom_boxplot()+facet_grid(cols=vars(type))+theme(axis.text.x = element_text(angle = 90))
PC5_plot<-data.frame(pca$x[,1:8], Severity=train_set%>%.$Severity , type=train_set%>%.$Accident_Type_Code)%>%ggplot(aes(Severity, PC5, color=Severity))+geom_boxplot()+facet_grid(cols=vars(type))+theme(axis.text.x = element_text(angle = 90))
PC1_plot<-data.frame(pca$x[,1:8], Severity=train_set%>%.$Severity , type=train_set%>%.$Accident_Type_Code)%>%ggplot(aes(Severity, PC1, color=Severity))+geom_boxplot()+facet_grid(cols=vars(type))+theme(axis.text.x = element_text(angle = 90))
PC3_plot<-data.frame(pca$x[,1:8], Severity=train_set%>%.$Severity , type=train_set%>%.$Accident_Type_Code)%>%ggplot(aes(Severity, PC3, color=Severity))+geom_boxplot()+facet_grid(cols=vars(type))+theme(axis.text.x = element_text(angle = 90))
PC4_plot<-data.frame(pca$x[,1:8], Severity=train_set%>%.$Severity , type=train_set%>%.$Accident_Type_Code)%>%ggplot(aes(Severity, PC4, color=Severity))+geom_boxplot()+facet_grid(cols=vars(type))+theme(axis.text.x = element_text(angle = 90))
PC6_plot<-data.frame(pca$x[,1:8], Severity=train_set%>%.$Severity , type=train_set%>%.$Accident_Type_Code)%>%ggplot(aes(Severity, PC6, color=Severity))+geom_boxplot()+facet_grid(cols=vars(type))+theme(axis.text.x = element_text(angle = 90))
PC7_plot<-data.frame(pca$x[,1:8], Severity=train_set%>%.$Severity , type=train_set%>%.$Accident_Type_Code)%>%ggplot(aes(Severity, PC7, color=Severity))+geom_boxplot()+facet_grid(cols=vars(type))+theme(axis.text.x = element_text(angle = 90))
PC8_plot<-data.frame(pca$x[,1:8], Severity=train_set%>%.$Severity , type=train_set%>%.$Accident_Type_Code)%>%ggplot(aes(Severity, PC8, color=Severity))+geom_boxplot()+facet_grid(cols=vars(type))+theme(axis.text.x = element_text(angle = 90))
#as we see Principal Components here are not very useful, only PC2 PC5 and PC3 show some variability between different severities
#some other PCs, like PC8 do a good job in predicting accident type but are not useful for severity predictions
#PC1 instead even if it accounts for almost all the variance in the data cannot help in preictions both for severity and for type

rm(pca, predictors_matrix) #removes objects only needed to create previous plots

#Model development

#To benchmark the performance of our model we start by computing the accuracy of randomly selecting a severity

set.seed(1, sample.kind = "Rounding")

random_severity<-ordered(sample(severity_levels, nrow(test_set) ,replace=TRUE)) #samples severity predictions randomly
#we then compute the confusion matrix of the random prediction
confMatrix_random<-confusionMatrix(random_severity, reference = test_set$Severity) 

#Then we try a model that randomly selects severity levels using the proportions of severity level in the training set as samling probabilities
proportionsOnly_severity<-ordered(sample(severity_levels, nrow(test_set) ,replace=TRUE, prob=train_set_proportions))
#And we compute the confusion matrix of this model
confMatrix_proportionsOnly<-confusionMatrix(proportionsOnly_severity, reference=test_set$Severity)

#Now we implement a model that fids the most frequent severity class for each accident type in the training set and predicts severity in the test from the accident type of each report
#plots the number of reports per seveirty level faceting by accident type 
code_vs_severity_plot<-train_set%>%ggplot(aes(x=Severity, fill=Severity))+geom_bar(stat = "count")+theme(legend.position = "none")+facet_grid(rows=vars(Accident_Type_Code))

#creating  function that computes the mode for factor variables
factor_mode <- function(x) {
  uniqx <- unique((x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
#grouping by accident type and predicting severity as the most frequent severity category (the mode) for eah accident type 
#and storing the results in a dataframe with one column for accident type and one column with most frequent severity
AccTypeSeverities<-train_set%>%group_by(Accident_Type_Code)%>%dplyr::summarize(PredSeverity=as.character(factor_mode(Severity)))
#initializing  empty vector of predictions for this model, it will be populated in the for cycle 
prediction_by_acctype=vector(length=nrow(test_set), mode="character")
#including the empty vector created above in the test set
test_set<-test_set%>%mutate(predictionByAccType=prediction_by_acctype)
#creating a for cycle over the 7 accident types predicting the severity of rows in the test set with the accident type-severity link 
#stored in the AccTypeSeverities dataframe
for (i in c(1:7)) {
test_set$predictionByAccType[test_set$Accident_Type_Code==AccTypeSeverities$Accident_Type_Code[i]]<- AccTypeSeverities$PredSeverity[i]
}
#converts predictions as an ordered factor to compare them with actual severities
test_set<-test_set%>%mutate(predictionByAccType=ordered(predictionByAccType, levels=levels(test_set$Severity)))

#computes confusion matrix for the model above
confMatrix_byType<-confusionMatrix(test_set$predictionByAccType, reference=test_set$Severity)
#removes objects that are no more needed
rm(prediction_by_acctype, AccTypeSeverities, random_severity)

#in preparation  for more computationally heavy alghoritms we remove Cabin temerature and Accident ID columns, which  are not useful for prediction
train_set<-train_set%>%select(-Cabin_Temperature, -Accident_ID)

#one-hot encoding factor variable Accident_Type_Code 
#we split the data contained in accident type code in 7 columns with only 1 or 0 values to prepare the data for the following predictions alghoritms
dummy_test<-data.frame(predict(dummyVars("~ Accident_Type_Code", data=test_set), newdata=test_set))
#and we bind the newly created column to our test set, deleting the Accidet_Tye_Code column
test_set_dummy<-cbind(test_set%>%select(-Accident_Type_Code), dummy_test)
#We repeat the same process for the training set
dummy_train<-data.frame(predict(dummyVars("~ Accident_Type_Code", data=train_set), newdata=train_set))
train_set_dummy<-cbind(train_set%>%select(-Accident_Type_Code), dummy_train)



#fitting a classification tree model
fit_CART<-train(Severity ~ . , data=train_set_dummy, method="rpart", tuneGrid = data.frame(cp = seq(0.00, 0.05, len = 25)))
#plotting accuracy vs complexity parameter
plot_accur_CART<-plot(fit_CART)
#plotting the best classsification tree found by the train function
plot_tree_CART<-rpart.plot(prune(fit_CART$finalModel, cp=0.01),  fallen.leaves = FALSE, tweak=1.4 ) #, margin = 0.1)
#making prediction with the best classification tree
prediction_CART<-predict(fit_CART, test_set_dummy)
#computing alghoritm performances
confMatrix_CART<-confusionMatrix(prediction_CART, reference = test_set_dummy$Severity)

set.seed(1, sample.kind = "Rounding")
#fitting a randomforest model
fit_randomforest<-train(Severity ~ . , data=train_set_dummy, method='Rborist')
#plotting accuracy vs number of trees
plot(fit_randomforest)
#making predictions
prediction_randomforest<-predict(fit_randomforest, test_set_dummy)
#compting alghoritm performance
confMatrix_randomforest<-confusionMatrix(prediction_randomforest, reference=test_set_dummy$Severity)


#fitting knn model
fit_knn<-train(Severity ~ . , data=train_set_dummy, method='knn', tuneGrid=data.frame(k=seq(1, 3, 1)))
#plotting accuracy vs number of k
plot(fit_knn)
prediction_knn<-predict(fit_knn, test_set_dummy)
confMatrix_knn<-confusionMatrix(prediction_knn, reference = test_set_dummy$Severity)


#fitting flexible discriminant analysis model
fit_fda<-train(Severity ~ . , mtehod="fda", data=train_set_dummy)
prediction_fda<-predict(fit_fda, test_set_dummy)
confMatrix_fda<-confusionMatrix(prediction_fda, reference=test_set_dummy$Severity )