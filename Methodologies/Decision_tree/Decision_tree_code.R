



# Set parameters for the tree
cp1 <- 0.01 #default 0.01 (complexity parameter) : 0.045
minsplit1 <- 20 #default 20 (minimum group size that can stil be split)
maxdepth1 <- 5 #default 30 (maximum number of levels in the tree)

# cp: Complexity Parameter
# The complexity parameter (cp) in rpart is the minimum improvement in the model needed at each node. It’s based on the cost complexity of the model defined as...
# 
# sum of terminal nodes' missclassification + lambda*number of splits
# 
# For the given tree, add up the misclassification at every terminal node.
# Then multiply the number of splits time a penalty term (lambda) and add it to the total misclassification.
# The lambda is determined through cross-validation and not reported in R.
# The cp we see using printcp() is the scaled version of lambda over the misclassifcation rate of the overall data.
# The cp value is a stopping parameter. It helps speed up the search for splits because it can identify splits that don’t meet this criteria and prune them before going too far.
# 
# If you take the approach of building really deep trees, the default value of 0.01 might be too restrictive.


# Create a decision tree model specification
class_tree_spec <- decision_tree(
  min_n = minsplit1,
  tree_depth = maxdepth1,
  cost_complexity = cp1,
) %>%
  set_engine("rpart") %>%
  set_mode("classification")


# Now let´s introduce the partition of the dataset
set.seed(655)
trainIndex <- createDataPartition(data$sex, p=0.7, list=F)
trainData <- data[trainIndex, -2]
testData <- data[-trainIndex, -2]

train_y <- data$sex[trainIndex]
test_y <- data$sex[-trainIndex]


class_tree_fit <- class_tree_spec %>%
  fit(as.factor(train_y) ~ ., data = trainData) 

# Build the decision tree
mytree <- rpart(train_y ~ ., data = trainData, method = "class", minsplit = minsplit1, cp = cp1, maxdepth = maxdepth1, model = TRUE)
# Plot the decision tree
rpart.plot(mytree, type = 2, extra = 1, under = TRUE, cex = NULL, box.palette = "auto", digits = 3)


testData2$test_y <- as.factor(test_y)

#...for testing set
augment(class_tree_fit, new_data = testData2) %>%
  accuracy(truth = test_y, estimate = .pred_class)


class_tree_fit <- class_tree_spec %>%
  fit(as.factor(test_y) ~ ., data = testData) 

class_tree_fit


# Build the decision tree
mytree <- rpart(test_y ~ ., data = testData, method = "class", minsplit = minsplit1, cp = cp1, maxdepth = maxdepth1, model = TRUE)

# Plot the decision tree
rpart.plot(mytree, type = 2, extra = 1, under = TRUE, cex = NULL, box.palette = "auto", digits = 3)
