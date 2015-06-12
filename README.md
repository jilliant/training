# training


#######################################################################
# Data Analysis for Fraud and Anomaly Detection in Forensics Security #
# Alces Ronin Presciient Pty Ltd 2013                                 #
# Examples:                                                           #
# - Predictive Methods - Residual Analysis                            #
# - Predictive Methods - Overfitting                                  #
# - Outlier Detection - Distributions                                 #
# - Outlier Detection - K Means                                       #
# - Outlier Detection - Random Forest                                 #
# - Social Network Analysis                                           #
#######################################################################


# EXAMPLE
# Predictive Methods - Residual Analysis
expenses <- read.csv('Expenses.csv')
summary(expenses)

# To run a linear model use
exp.model <- lm(formula=Expenses ~ Department + Level + Location + Hours_Worked, data=expenses)
summary(exp.model)

RF <- randomForest(expenses[,2:5], expenses$Expenses,do.trace=T,importance=T)

summary(exp.model)

# To get rid of the intercept term include -1 in the formula
exp.model.no.inter <- lm(formula=Expenses ~ Department + Level + Location + Hours_Worked - 1, data=expenses)
summary(exp.model.no.inter)
anova(exp.model.no.inter)

# Attach the fitted values and the residuals to the main data set
expenses.pred <- cbind(expenses,exp.model.no.inter$fitted.values, exp.model.no.inter$residuals)

expenses.RF <-cbind(expenses,exp.model.no.inter$fitted.values, exp.model.no.inter$residuals)

a<-expenses.RF[order(expenses.RF[,8],decreasing=T),]






# Order the data set by the residuals (column 8) in descending order . 
expenses.pred[order(-abs(expenses.pred[,8])),]

#######################################################################

# EXAMPLE
# Predictive Methods - Overfitting
install.packages(ggplot2)
library(ggplot2)
bonus_ofit <- read.csv('bonus_overfit.csv')
x <- bonus_ofit[,2]
y <- bonus_ofit[,3]

# specify the maximum polynomial degree that will be explored
max.poly <- 20

# cretaing data.frame which will store model predictions
# that will be used for the smooth curves in Fig. 1
x.new <- seq(min(x), max(x), length.out=100)
degree <- rep(1:max.poly, each=length(x.new))
predicted <- numeric(length(x.new)*max.poly)
new.dat <- data.frame(x=rep(x.new, times=max.poly),
                      degree,
                      predicted)

# fitting lm() polynomials of increasing complexity
# (up to max.degree) and storing their predictions
# in the new.dat data.frame
for(i in 1:max.poly)
{
  sub.dat <- new.dat[new.dat$degree==i,]
  new.dat[new.dat$degree==i,3] <- predict(lm(y~poly(x, i)),
                                          newdata=data.frame(x=x.new))
}

# plotting the data and the fitted models
p <- ggplot()
p + geom_point(aes(bonus_ofit$x, bonus_ofit$y), bonus_ofit, colour="darkgrey") +
  xlab('Income') + ylab('Bonus') +
 geom_line(aes(x, predicted,
                  colour=as.character(degree)),
              new.dat[new.dat$degree %in% c(20),]) +
 scale_colour_discrete(name = "Degree")

# function that will perform the "leave one out"
# crossvalidation for a y~poly(x, degree) polynomial
crossvalidate <- function(x, y, degree)
{
  preds <- numeric(length(x))
  for(i in 1:length(x))
  {
    x.in <- x[-i]
    x.out <- x[i]
    y.in <- y[-i]
    y.out <- x[i]
    m <- lm(y.in ~ poly(x.in, degree=degree) )
    new <- data.frame(x.in = seq(-3, 3, by=0.1))
    preds[i]<- predict(m, newdata=data.frame(x.in=x.out))
  }
  # the squared error:
  return(sum((y-preds)^2))
}

# crossvalidating all of the polynomial models
# and storing their squared errors in
# the "a" object
a <- data.frame(cross=numeric(max.poly))
for(i in 1:max.poly)
{
  a[i,1] <- crossvalidate(bonus_ofit$x, bonus_ofit$y, degree=i)
}

# plotting crossvalidated squared errors agains
# model complexity
cross.plot <- qplot(1:max.poly,cross, data=a, geom=c("line"))+
  xlab("Degree") +
  ylab("Squared error") +
  labs(title="Crossvalidation")
cross.plot

#######################################################################

# EXAMPLE
# Outlier Detection - Distributions

income.distributions <- read.csv(file='Distributions.csv') 

# Then plot the data using

boxplot(income.distributions$Income_norm)
hist(income.distributions$Income_norm)
plot(density(income.distributions$Income_norm))

# Plot the other data using

boxplot(income.distributions$Income_tail)
hist(income.distributions$Income_tail)
plot(density(income.distributions$Income_tail))

boxplot(income.distributions$Income_bimodal)
hist(income.distributions$Income_bimodal)
plot(density(income.distributions$Income_bimodal, bw = 2000))


#######################################################################

# EXAMPLE
# Outlier Detection - K Means

# Read in the data
expenses.cluster <- read.csv(file="Expenses_cluster.csv")

# Perform k-means clustering with 4 clusters
exp.km <- kmeans(expenses.cluster, 4)

# Plot the clusters and the centre of the clusters
plot(expenses.cluster, col = exp.km$cluster)
points(exp.km$centers, col = 1:4, pch = 8, cex = 2)

# Calculate the residual
resid <- expenses.cluster - fitted(exp.km)
resid <- scale(resid)

# Calculate the Euclidian distance from the centroids
distance.k <- sqrt((resid[,1])**2 + (resid[,2])**2)

# Join the cluster labellings to our original data and order by the distance
# descending
exp.model <- cbind(expenses.cluster,exp.km$cluster,distance.k)
exp.model[order(-distance.k),]

# Plot the points with the residuals colour coded.
plot(exp.model[,1:2], col=round(distance.k+1))
points(exp.km$centers, col = 1, pch = 8, cex = 2)


#######################################################################

# EXAMPLE
# Outlier Detection - Random Forest

# Load the library and the data
library(randomForest)
audit <- read.csv('audit.csv')

# Drop the id and target variables
audit.drop <- audit[,2:10]

# Run the Random Forest
audit.rf <- randomForest(x=audit.drop, ntree=1000)

# Fix the na values then run the Random Forest
audit.na <- na.roughfix(audit.drop)
audit.rf <- randomForest(x=audit.na, ntree=1000, proximity=TRUE, keep.forest=FALSE)

# Outliers from the Random Forest
outlier.audit <- outlier(audit.rf$proximity)
plot(outlier(audit.rf), type="h")


audit.scored <- cbind(audit,outlier.audit)
audit.scored <- audit.scored[order(-abs(audit.scored[,14])),]

MDSplot(audit.rf, as.factor(audit$TARGET_Adjusted), k=3)

#######################################################################

# EXAMPLE
# Social Network Analysis

# Load the necessary packages
install.packages(c("igraph","ggplot2"))
library(igraph)
library(ggplot2)

# Read in the graph
G<-read.graph("drug_main.txt",format="edgelist")

# Change to undirected graph, this means from 1 to 10 is the same as from
# 10 to 1
G<-as.undirected(G)

# Plot the graph
plot(G,vertex.size=3, vertex.label.dist=0.4, vertex.label.cex=0.5)

# Create a new dataframe with centrality metrics
cent<-data.frame(bet=betweenness(G),eig=evcent(G)$vector)

# We use ggplot2 to make things a it prettier
p<-ggplot(cent,aes(x=bet,y=eig,label=rownames(cent),colour=res,
                   size=abs(res)))+xlab("Betweenness Centrality")+ylab("Eigenvector Centrality")
p+geom_text()+labs(title="Key Actor Analysis for Hartford Drug Users")
# We use the residuals to color and shape the points of our plot,
# making it easier to spot outliers.
pdf('key_actor_analysis.pdf')
p+geom_text()+opts(title="Key Actor Analysis for Hartford Drug Users")
# We use the geom_text function to plot the actors' ID's rather than points
# so we know who is who
dev.off()

# Plot the graph highlighting 44
ident.col <- rep(1,194)
ident.col[44] <- 4
ident.size <- rep(1,194)
ident.size[44] <- 4
plot(G,vertex.size=3, vertex.label.dist=0.4, vertex.label.cex=ident.size*0.5, vertex.label.color=ident.col)


# Plot the graph highlighting 67,79 and 102
ident.col <- rep(1,194)
ident.col[c(67,79,102)] <- 4
ident.size <- rep(1,194)
ident.size[c(67,79,102)] <- 4
plot(G,vertex.size=3, vertex.label.dist=0.4, vertex.label.cex=ident.size*0.5, vertex.label.color=ident.col)
