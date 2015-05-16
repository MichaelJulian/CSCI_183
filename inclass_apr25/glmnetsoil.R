library(glmnet);
# Load soil data and switch to matrix. glmnet only takes matrices
m = read.csv('../highdimensional/soil.csv')
mat = as.matrix(m)


# not looking for probabilities, so guassian
fit = glmnet(x=mat[,1:3578] , y=mat[,3579], family='gaussian',nlambda=5)
cv = cv.glmnet(x=mat[,1:3578], y=mat[,3579], family='gaussian')
plot(cv) # plot cross-validated lambdas


lam = cv$lambda.min # minimum error lambda

# Using this new lambda, create a new glmnet
fit2 <- glmnet(x=mat[,1:3578], y=mat[,3579], family="gaussian", lambda=lam)


coefs = coef(fit2)
coefdf = data.frame(variable = rownames(coefs)[2:3579], beta=as.numeric(abs(fit2$beta)))

coefdf = coefdf[order(-abs(coefdf$beta)),]
bigB = head(coefdf,n=10)
bigB
