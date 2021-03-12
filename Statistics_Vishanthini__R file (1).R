
## Load Packages and Data

# Required packages
library(psych)
library(tidyverse)
library(qqplotr)
library(coda)
library(readr)

# Import data from local disk
x<-read_csv("C:/Users/visha/Desktop/Satistics Assignment/Assignment/X.csv",col_names=FALSE)
y<-read_csv("C:/Users/visha/Desktop/Satistics Assignment/Assignment/Y.csv",col_names=FALSE)
time<-read_csv("C:/Users/visha/Desktop/Satistics Assignment/Assignment/Time.csv",col_names=FALSE)
colnames(x)<-paste0(rep("x",ncol(x)),1:ncol(x))
colnames(y)<-"y"
colnames(time)<-"time"
df<-cbind(time,y,x)
head(df)

#_____________    Task 1: Preliminary data analysis  _________________________

# Time series plots (input and output EEG signals)

# Output Signal
ggplot(df, aes(x=time, y=y)) + geom_line() + 
            xlab("Time")+ ylab("Signal")+ ggtitle( "Output EEG signal")

# Input Signal
df_x <- gather(df[,-2],key = "input", value = "value", -time)
plot_x <-ggplot(df_x, aes(x = time, y = value)) + 
            geom_line(aes(color = input)) + 
             facet_wrap(~input, scales = "free_x")+
            xlab("Time")+ ggtitle( "Input EEG signals")
plot_x

# Distribution for each EEG signal

ggplot(gather(y), aes(value, fill = key))+ geom_density(alpha = 0.2)
ggplot(gather(x), aes(value, fill = key))+ geom_density(alpha = 0.2)


# Correlation and scatter plots (between different input EEG signals and the output EEG) 

pairs.panels(df[,-1], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE, ellipses = TRUE )

#_______ Task 2: Regression- modelling the relationship between EEG signals ____

mod_1<- lm(y ~ x4 + I(x1^2)+I(x1^3)+I(x3^4)+1         , data=df)
mod_2<- lm(y ~ I(x3^3)+I(x3^4)+1                      , data=df)
mod_3<- lm(y ~ x2 +I(x1^3)+I(x3^4)+1                  , data=df)
mod_4<- lm(y ~ x4 +I(x1^3)+I(x3^4)+1                  , data=df)
mod_5<- lm(y ~ x4 + I(x1^2)+I(x1^3)+I(x3^4)+I(x1^4)+1 , data=df)

# Task 2.1

mod_1$coefficients
mod_2$coefficients
mod_3$coefficients
mod_4$coefficients
mod_5$coefficients

# Task 2.2

rss1<-anova(mod_1)["Residuals", "Sum Sq"]
rss2<-anova(mod_2)["Residuals", "Sum Sq"]
rss3<-anova(mod_3)["Residuals", "Sum Sq"]
rss4<-anova(mod_4)["Residuals", "Sum Sq"]
rss5<-anova(mod_5)["Residuals", "Sum Sq"]
rss<-data.frame(Model=paste("Model",1:5, sep=" "),
                    RSS=c(rss1,rss2,rss3,rss4,rss5))
rss

# Task 2.3

logLike<-function(model)
       {
        n<-nobs(model)
        rss<-anova(model)["Residuals", "Sum Sq"]
        sigma2<-rss/(n-1)
        logL<- -(n/2)*log(2*pi) - (n/2)*log(sigma2) -(1/2*sigma2)*rss
        return(logL)}
likelihood<-data.frame(Model=paste("Model",1:5, sep=" "),
       log_likelihood=c(logLike(mod_1),logLike(mod_2),logLike(mod_3),logLike(mod_4),logLike(mod_5)))
likelihood

# Task 2.4 

aic<-function(model)
         {
          logL<-logLike(model)
          k<-length(coef(model))
          AIC<- 2*k - 2*logL
          return(AIC)}

bic<-function(model)
         {
          n<-nobs(model)
          logL<-logLike(model)
          k<-length(coef(model))
          BIC<- k*log(n) - 2*logL
          return(BIC)}

aic_bic<-data.frame(Model=paste("Model",1:5, sep=" "),
             AIC=c(aic(mod_1),aic(mod_2),aic(mod_3),aic(mod_4),aic(mod_5)),
             BIC=c(bic(mod_1),bic(mod_2),bic(mod_3),bic(mod_4),bic(mod_5)))
aic_bic

# Task 2.5

# Error distribution
error<-data.frame(model_1=mod_1$residual,model_2=mod_2$residual,model_3=mod_3$residual,model_4=mod_4$residual,model_5=mod_5$residual)
ggplot(gather(error), aes(value, fill = key))+ geom_density(alpha = 0.2)+ 
             facet_wrap(~key, scales = "free_x")
# QQ-plot
ggplot(gather(error),mapping = aes(sample = value, color = key, fill = key)) +
           stat_qq_line() +
           stat_qq_point() +
           facet_wrap(~ key) +
           labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

# Task 2.6
The smallest AIC and BIC values was found at `Model 3` and the residual of this model are more normal comparative to others model. 

# Task 2.7

splt <-sort(sample(1:dim(df)[1], 0.7*dim(df)[1]))
tr_df<-df[splt ,]
ts_df<-df[splt ,]

mod_bst<- lm(y ~ x2 +I(x1^3)+I(x3^4)+1 , data=tr_df)
ts_res <- predict(mod_bst, newdata=ts_df,interval="confidence",level = 0.95, se.fit=TRUE)
pred_df<- data.frame(ts_df,ts_res$fit,sd=ts_res$se.fit)
head(pred_df)

# Plot for prediction
plot(pred_df$time ,pred_df$fit, type="l",col="red",xlab="Time",ylab="Output signal")
arrows(x0=pred_df$time, y0=pred_df$fit-pred_df$sd, x1=pred_df$time, y1=pred_df$fit+pred_df$sd, code=3, angle=90, length=0.05)
points(pred_df$time,pred_df$y, pch=20)
lines(pred_df$time,pred_df$upr, type="l", col="gray",lty=5)
lines(pred_df$time,pred_df$lwr, type="l", col="gray",lty=5)

# Task 3

# (1)

# Parameters with largest absolute values 
 p_select<-order(abs(coef(mod_3)),decreasing =TRUE)[1:2]
 coef(mod_3)[ p_select]

# (2 & 3)

abc_df<-function(df){
        p_bias<-runif(1, -1, 1)
        p_1   <-runif(1, -0.5, 0.5)
        model<-lm(y ~ x2 +I(x1^3)+I(x3^4)+1, data=df)
        theta<-coef(model)
        p_select<-order(abs(theta),decreasing =TRUE)[1:2]
        theta[p_select]<-c(p_bias,p_1)
        y_sim<- as.matrix(df[,-(1:2)]) %*% as.matrix(theta)
        distance <- abs(mean(df$y) - mean(y_sim))
        return(c(p_bias,p_1,distance))}

iteration<-10000
pos_df<-as.data.frame(t(replicate(iteration,abc_df(df))))
names(pos_df)<-c("theta_bias","theta_1","distance")
Epsilon<-c()
Epsilon[which(pos_df$distance>=0.1 & pos_df$distance<0.5)]<-"<0.5"
Epsilon[which(pos_df$distance>=0.09 & pos_df$distance<0.1)]<-"<0.1"
Epsilon[which(pos_df$distance<0.09)]<-"<0.09"
dddf<-data.frame(Epsilon,pos_df)
ggplot(dddf, aes(x = theta_bias, y =theta_1)) + 
            geom_point(aes(color = Epsilon))     +
            ggtitle( "Accepted parameters for different values of epsilon") 

mcmc_mod <- mcmc(pos_df[which(pos_df$distance<0.09),c(1,2)])
summary(mcmc_mod)

# (4)

# Plotting the posterior distributions
plot(mcmc_mod)  
