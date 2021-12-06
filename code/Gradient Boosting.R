centers = read.csv("./data/fulfilment_center_info.csv")
meals = read.csv("./data/meal_info.csv")
sales = read.csv("./data/train.csv")

library (gbm)

X = merge(sales, centers, by = "center_id")
X = merge(X, meals,by = "meal_id")
head(X)
colnames(X)

X$meal_id = as.factor(X$meal_id)
X$center_id = as.factor(X$center_id)
X$emailer_for_promotion = as.factor(X$emailer_for_promotion)
X$homepage_featured = as.factor(X$homepage_featured)
X$city_code = as.factor(X$city_code)
X$region_code = as.factor(X$region_code)
X$center_type = as.factor(X$center_type)
X$category = as.factor(X$category)
X$cuisine = as.factor(X$cuisine)

#### parte presa da Massimiliano
Design_matrix = Matrix::sparse.model.matrix( ~ X$week + X$meal_id*X$center_id + X$week*X$meal_id*X$center_id)
lin_model = glmnet::glmnet(Design_matrix, X$num_orders)
predicted = glmnet::predict.glmnet(lin_model, newx = Design_matrix, type = "response", s = 0)
####

val_idx = which(X$week > 130)
X = X[,-c(3,4,5)] # "id", "checkout_price", "week"
residuals = X$num_orders - predicted
X$residuals = residuals # add residuals

X_train = X[-val_idx,]
X_val = X[val_idx,]






# Hyperparameters
n_trees = 100
depth_tree = 10

# Training
m1 = gbm(num_orders ~ ., data=X_train, distribution="gaussian",
         n.trees=n_trees, interaction.depth=depth_tree)

# Prediction for validation set
predicted_GB = predict(m1, newdata = X_val, n.trees=1:n_trees)

# Compute MSE on prediction
err = apply(predicted_GB, 2, function(pred) mean((X_val$num_orders - pred)^2))

# Plot train vs validation MSE
plot(m1$train.error, type="l", ylim = c(0,150000), xlab='Number of trees', ylab='MSE')
lines(err, type="l", col=2)




# Relative influence plot
mai.old <- par()$mai
mai.new <- mai.old
mai.new[2] <- 2.1 
par(mai=mai.new)
summary(m1, las=1, cBar=10)
par(mai=mai.old)





# Marginal effects
plot(m1, i.var=1, n.trees = 50)
plot(m1, i.var=2, n.trees = 50)
plot(m1, i.var=3, n.trees = 50) # base_price
plot(m1, i.var=4, n.trees = 50)
plot(m1, i.var=5, n.trees = 50) # homepage_feature
plot(m1, i.var=6, n.trees = 50)
plot(m1, i.var=7, n.trees = 50)
plot(m1, i.var=8, n.trees = 50)
plot(m1, i.var=9, n.trees = 50) # op_area
plot(m1, i.var=10, n.trees = 50)
plot(m1, i.var=11, n.trees = 50)
plot(m1, i.var=12, n.trees = 50) # residuals




# MAE
err2 = apply(yhat.boost, 2, function(pred) mean(abs(X_val$num_orders - pred)))
err2
