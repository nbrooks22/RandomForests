library(RandomForestsPackage)

test <- RandomForestsPackage::greedy_cart_regression(create_random_sample_data_reg(1, 100), depth = 5)
printGreedyCartRegression(test)
plotTree(test)
