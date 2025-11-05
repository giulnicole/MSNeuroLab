
library(pROC)
library(plotROC)

plot(MS1_stepwise_model2) #OVerall model quality check
#Residuals vs. fitted: no clear and severe patterns
# Q-Q residuals: Quite nicely on the line apart from a few values
# REsiduals vs leverahe: No values over the dashed gray 0+.5 line -> ok

# Extract model results
model.data <- broom::augment(MS1_stepwise_model2) %>% mutate(index = 1:n()) #MAke sure you have package broom installed!
model.data %>% top_n(3, .cooksd) #If cooksd values are over 1, you should be alarmed, and if 0.5 you should be cautious. Here this looks fine

# You can categorize model given probabilities onto "Predicted progression" and "Predicted stable"
fitted1 <- fitted(MS1_stepwise_model2) > 0.5 # Here probability 0.5 is used, but it could theoretically be e.g. according to the actuald proportion of progressed in the data
fitted1

#Check the table: dow does it look:
table(MS1$EDSSprogression, fitted1)

#For each 1-unit increase in a continuous variable, the odds of EDSS increase is...
exp(coef(MS1_stepwise_model2))

#But if you need, for each 0.1-unit increase in a continuous variable, the odds of EDSS increase for each varaible would be...
exp(0.1*coef(MS1_stepwise_model2)) #Equivalently: (exp(coef(MS1_stepwise_model2))^0.1)
#This works well at least for DVRs



#Leave one out cross validation (LOOCV) - predict each value based on the other values using the obtained model above
fitted0 <- 0
for (i in 1:nrow(MS1)) {
  CV1 <- glm(MS1_stepwise_model2$formula, family = "binomial", data = MS1[-i, ])
  fitted0[i] <- predict(CV1, MS1[i,], type = "response") #gives probability of progression in the cross-validation
}


#Create roc object based on the LOOCV
roc_obj <- roc(MS1$EDSSprogression, fitted0, ci = TRUE) #roc(response, predictor...
auc1 <- auc(roc_obj) #calculate AUC (area under curve) base on the roc curve

p1 <- pROC::ggroc(roc_obj, legacy.axes = TRUE, size = 1.5) #from pROC package
pROCa_AllMS <- p1 + #style_roc() +
  style_roc(xlab = "1 - Specificity", ylab = "Sensitivity") +
  theme(axis.title = element_text(size = 9, face = "bold"),
        axis.text = element_text(size = 9, face = "bold")) +
  theme(strip.text.x = element_text(size = 9)) +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold")) +
  annotate(geom = "text", y = 0.375, x = 0.625, label = paste0("AUC = ", round(auc1, 2)), size = 4)
plot(pROCa_AllMS)
pROCa_AllMS <- annotate_figure(pROCa_AllMS, top = text_grob("All MS", face = "bold", size = 9))
pROCa_AllMS