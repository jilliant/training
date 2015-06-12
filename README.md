# t Analysis
expenses <- read.csv('Expenses.csv')
summary(expenses)

# To run a linear model use
exp.model <- lm(formula=Expenses ~ Department + Level + Location + Hours_Worked, data=expenses)
summary(exp.-i]
    x.out <distributions$Income_norm)
plot(density(income.distributions$Income_norm))

# Plot the other #################################################

# EXAMPLE
# Outlier Detection - K Means

# Read in the data
expenses.cluster <- read.csv(file="Expenses_cluster.csv")

# Perform k-means clustering with 4 clusters
exp.km <- kmeans(expenses.cluster, 4)

# Plot the clusters and the centre of the clusters
plot(expenses.cluster, col = exp.km$cluster)
points(exp.km$centers, col =

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
ident.size[c(67,79,102=0.4, vertex.label.cex=ident.size*0.5, vertex.label.color=ident.col)
