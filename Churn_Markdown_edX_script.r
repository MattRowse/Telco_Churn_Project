
# Load packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(Amelia)) install.packages("Amelia", repos = "http://cran.us.r-project.org")
if(!require(mltools)) install.packages("mltools", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(i2dash)) install.packages("i2dash", repos = "http://cran.us.r-project.org")
if(!require(Boruta)) install.packages("Boruta", repos = "http://cran.us.r-project.org")
if(!require(h2o)) install.packages("h2o", repos = "http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")
if(!require(prettydoc)) install.packages("prettydoc", repos = "http://cran.us.r-project.org")

# Import data
path <- "C://Users/bmr057/Desktop/edx/CIY_Project/datasets_13996_18858_WA_Fn-UseC_-Telco-Customer-Churn.csv"
data <- read.csv(path)

# Perform initial checks of the data
str(data)
missmap(data)

## Wrangling
# Replace the na total charges with the mean
data <- data  %>% 
  mutate(TotalCharges = if_else(is.na(TotalCharges) == TRUE,
  mean(TotalCharges, na.rm = TRUE), TotalCharges))
missmap(data)

# View the unique SeniorCitizen values and convert to factor
unique(data$SeniorCitizen)
# Coerce 
data <- data %>% 
  mutate(SeniorCitizen = as.factor(if_else(SeniorCitizen == 1, "Yes", "No")))
data <- data %>% 
  mutate(tenure = as.integer(tenure))

## Expolration 
# Churn vs internet service
data %>% ggplot(aes(InternetService, TotalCharges, fill = Churn))+
  geom_col()+
  ggtitle("Total Charges by Internet Service")+
  labs(x="Internet Service", y="Total Charges",
  subtitle = "Higher churn in fibre optic internet services")+
  scale_y_continuous(labels = dollar)

# How do the costs stack up?
data %>% ggplot(aes(tenure, TotalCharges, colour=InternetService))+
  geom_point()+
  ggtitle("Total Charges by Plan & Tenure")+
  labs(x="Tenure", y="Total Charges",
  subtitle = "Note the absence of longer term DSL customers")+
  scale_y_continuous(labels = dollar)

# Lets look more closely at this
data %>% filter(InternetService=="DSL") %>%
  ggplot(aes(tenure, MonthlyCharges, colour=Churn))+
  geom_point()+
  geom_smooth(se=FALSE)+
  ggtitle("Tenure vs Monthly Charges vs Churn")+
  labs(x="Tenure", y="Monthly Charges",
  subtitle = "DSL churn occurs early in tenure without charges correlation")

# Is there a connection with dependents? 
data %>% filter(InternetService=="DSL" & Churn == "Yes") %>%
  ggplot(aes(tenure, MonthlyCharges, colour=Dependents))+
  geom_point()+
  geom_smooth(se=FALSE)+
  ggtitle("Churned DSL customers vs Monthly Charges vs Dependents")+
  labs(x="Tenure", y="Monthly Charges",
  subtitle="It appears if you do have dependents, monthly charges are higher")

# Perhaps this churn is related to contract?
data %>% filter(InternetService=="DSL" & Churn == "Yes") %>%
  ggplot(aes(tenure, MonthlyCharges, colour=Contract))+
  geom_point()+
  geom_smooth(se=FALSE)+
  ggtitle("Churned DSL customers vs Tenure vs Plan")+
  labs(x="Tenure", y="Monthly Charges",
  subtitle="The business needs to convert monthly plans to longer terms")

# Customer value
data %>% group_by(Contract, TotalCharges) %>%
  ggplot(aes(Contract, TotalCharges, fill = Contract))+
  geom_col()+
  ggtitle("Total Revenue by Plan")+
  labs(x="Plan", y="Total Revenue",
       subtitle = "Opportunity to Extend Customer Value From Monthly Plan")+
  scale_y_continuous(labels = dollar)

# Facet wrap plot
data %>% ggplot(aes(SeniorCitizen,TotalCharges, fill=Churn))+
  facet_wrap(~PaymentMethod)+
  geom_col()+
  ggtitle("Payment Method vs Senior Citizen")+
  labs(x="Senior Citizen", y="Total Charges",
       subtitle = "Surprised to see customers using mailed payments not senior citizens")+
  scale_y_continuous(labels = dollar)

# Tenure length wrangling
data <- mutate(data, tenure_bin = tenure)
data$tenure_bin[data$tenure_bin >=0 & data$tenure_bin <= 12] <- '0-1 year'
data$tenure_bin[data$tenure_bin > 12 & data$tenure_bin <= 24] <- '1-2 years'
data$tenure_bin[data$tenure_bin > 24 & data$tenure_bin <= 36] <- '2-3 years'
data$tenure_bin[data$tenure_bin > 36 & data$tenure_bin <= 48] <- '3-4 years'
data$tenure_bin[data$tenure_bin > 48 & data$tenure_bin <= 60] <- '4-5 years'
data$tenure_bin[data$tenure_bin > 60 & data$tenure_bin <= 72] <- '5-6 years'
data$tenure_bin <- as.factor(data$tenure_bin)

# Plot tenure length
data %>% ggplot(aes(tenure_bin, fill = tenure_bin)) + 
  geom_bar()+
  ggtitle("Tenure Length by Year")+
  labs(x="Tenure Length in Years",
       y="Number of Customers",
       subtitle = "Again we see the short term monthly plans as an opportunity")+
  scale_fill_discrete(guide=FALSE)

# Plotly object
plota <- data %>% ggplot(aes(tenure, MonthlyCharges, colour = InternetService))+
  geom_point()+
  geom_smooth()+
  ggtitle("Monthly Charges vs Tenure vs Service")+
  labs(x="Tenure", y="Monthly Charges")
ggplotly(plota)

## Feature Importance
set.seed(1)
# For ease lets remove the customer identification
boruta_data <- data[complete.cases(data[]),] %>%
  select(-customerID)

# Create and print Boruta output 
boruta_output <- Boruta(Churn~., data = boruta_data)
print(boruta_output)

# Tidy and plot the output
plot(boruta_output, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta_output$ImpHistory),function(i)
boruta_output$ImpHistory[is.finite(boruta_output$ImpHistory[,i]),i])
names(lz) <- colnames(boruta_output$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
at = 1:ncol(boruta_output$ImpHistory), cex.axis = 0.7)

# Get the important attributes withough tentative
getSelectedAttributes(boruta_output, withTentative = F)

## Modelling
# Remove unimportant features
data <- data %>% select(-gender, -customerID)

# Split the data into training and validation sets
test_index <- createDataPartition(data$Churn, p = .10, list = FALSE)
training <- data[-test_index,]
validation <- data[test_index,]

# Train a random forest model
rf_fit <- train(Churn ~.,
                data = training,
                method = "ranger")

# Test the model
rf_pred <- predict(rf_fit, newdata = validation, na.action = na.pass)

# Table and view the result
rf_result <- confusionMatrix(table(rf_pred,validation$Churn))
rf_result

# Store the accuracy for comparison later
rf.accuracy <- rf_result$overall['Accuracy']

## Automated Machine Learning (AML)
set.seed(1234)
# Use the h2o package to create a best fit stacked ensemble
h2o.init()

# Convert data to h2o arrays
h2o_training <- as.h2o(training)
h2o_test <- as.h2o(validation)

# Use the power of automated machine learning
aml <- h2o.automl(y="Churn", training_frame = h2o_training,
                  max_runtime_secs = 300)
aml_pred <- h2o.predict(aml@leader, h2o_test)

# Store accuracy and create confusion matrix
perf <- h2o.performance(aml@leader,h2o_test)
perf_cf <- h2o.confusionMatrix(perf)
h2o_acc <- max(h2o.accuracy(perf))
perf_cf

# Compare best performing automated algorithm vs randomforest
overall_results <- data.frame(Method="randomforest", 
                          Accuracy = rf.accuracy)
h2o_results <- data.frame(Method="aml",
                          Accuracy = h2o_acc)
overall_results <- overall_results %>% 
  rbind(h2o_results) %>%  knitr::kable(row.names = FALSE)
overall_results