# SPSS import

install.packages(c('foreign','ggplot2'))
library(ggplot2)
library(foreign)
library(memisc)


## Getting and Cleaning Data
df = read.spss('http://tiny.cc/employeedata', to.data.frame=T)
df$BDATE <- as.POSIXct(df$BDATE, origin='1582-10-14', format='%Y-%m-%d')
#options(scipen=9)


dim(df)
head(df)
qplot(y=SALARY, x=EDUC)
qplot(y=SALARY, x=EDUC, data=df)
qplot(y=SALARY, x=EDUC, data=df, col=JOBCAT)
qplot(x=BDATE, y=SALARY, data=df)
qplot(SALARY, colour=MINORITY, 
      data=df, geom='density')

qplot(JOBCAT, data=df, fill=GENDER)
qplot(paste(df$GENDER, df$JOBCAT), data=df, fill=EDUC) + scale_fill_brewer()

df$EDUC <- as.numeric(df$EDUC)
fit_linearAge = lm(SALARY ~ EDUC + GENDER + JOBCAT + MINORITY + AGE, data=df)
fit_nonlinAge = lm(SALARY ~ EDUC + GENDER + JOBCAT + MINORITY + AGE + I(AGE^2), data=df)
fit_Exp = lm(SALARY ~ EDUC + GENDER + JOBCAT + MINORITY, data=df)
fit_GenEduc = lm(SALARY ~ EDUC + GENDER:EDUC + GENDER + JOBCAT + MINORITY, data=df)
mtable(fit_Exp, fit_GenEduc)

hist(df$JOBTIME)
