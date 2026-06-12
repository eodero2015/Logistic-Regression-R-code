# Import dataset
Malaria<-read.table("E:/MMUST/STATISTICS NOTES/STA 347  Statistical Computing/Malaria data.csv",header=TRUE, sep=",",na.strings="NA", dec=".", strip.white=TRUE)
View(Malaria)
summary(Malaria) 
#Convert Dependent Variable to Binary
#Suppose the outcome variable is status where:
# malaria = 1
# no malaria= 0
Malaria$Status1 <- factor(Malaria$Status1,
                        levels = c(0,1),
                        labels = c("no malaria","malaria"))
#Fit Multiple Logistic Regression Model
model <- glm(Status1 ~ Fever + Rigors + Convulsion +
               sweating + Vomiting + Diarrhea + Pallor + Cough + Prostration,
             data = Malaria,
             family = binomial(link = "logit"))

summary(model)
#Obtain Odds Ratios

#The coefficients from logistic regression are in log-odds. Convert them to Odds Ratios:
exp(coef(model))
#Odds Ratios with 95% Confidence Intervals
exp(cbind(Odds_Ratio = coef(model),
          confint(model)))
#Model Fit Statistics
install.packages("pscl")
library(pscl)
pR2(model)
#Hosmer-Lemeshow Test
install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(as.numeric(Malaria$Status1) - 1, fitted(model), g = 5)
# Check Multicollinearity
install.packages("car")
library(car)
vif(model)
#Predicted Probabilities
Malaria$predicted_prob <- predict(model,
                               type = "response")

head(Malaria$predicted_prob)
#classification Table
predicted_class <- ifelse(Malaria$predicted_prob > 0.5, 1, 0)

table(Predicted = predicted_class,
      Actual = as.numeric(Malaria$Status1)-1)
#ROC curve and AUC
install.packages("pROC")
library(pROC)
roc_curve <- roc(as.numeric(Malaria$Status1)-1,
                 Malaria$predicted_prob)
plot(roc_curve)
auc(roc_curve)