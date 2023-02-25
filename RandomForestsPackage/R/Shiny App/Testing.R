# Testing
library(tree)

ir.tr <- tree(Species ~., iris)
ir.tr
ir.tr1 <- snip.tree(ir.tr, nodes = c(12, 7))
summary(ir.tr1)
par(pty = "s")
plot(iris[, 3],iris[, 4], type="n",
     xlab="petal length", ylab="petal width")
text(iris[, 3], iris[, 4], c("s", "c", "v")[iris[, 5]])
partition.tree(ir.tr1, add = TRUE, cex = 1.5)

# https://www.statology.org/plot-decision-tree-in-r/

library(rpart)
library(rpart.plot)
library(ISLR)
tree <- rpart(Salary ~ Years + HmRun, data=Hitters, control=rpart.control(cp=.0001))
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree <- prune(tree, cp=best)
prp(pruned_tree)

#plot decision tree using custom arguments
prp(pruned_tree,
    faclen=0, #use full names for factor labels
    extra=1, #display number of observations for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output
