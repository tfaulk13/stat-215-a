library(foreach)
library(doParallel)

nCores <- 12
registerDoParallel(nCores)



# Wrong way of splitting
# Ignore this!
RandomSpiltData <- function(img1, img2, img3) {

	imgMutate1 <- mutate(img1, Image = 1)
	imgMutate2 <- mutate(img1, Image = 2)
	imgMutate3 <- mutate(img1, Image = 3)
	img <- rbind(imgMutate1, imgMutate2, imgMutate3)

	sampleSize <- floor(0.75 * nrow(img))

	trainInd <- sample(seq_len(nrow()))

	train <- img[trainInd, ]
	test <- img[-trainInd, ]

}

# Wrong cross-validation
# Ignore this!
RandomCrossValidationLR <- function(img) {
	sumTrainAcc <- 0
	sumTestAcc <- 0

	numFold <- 10

	img$Num <- sample(1:numFold, nrow(img), replace = FALSE)
	for (i in 1:numFold) {
		train <- filter(img, num != i)
		test <- filter(img, num == i)
		
		tmpTrain <- LogisticRegression(train, test)$trainAcc
		tmpTest <- LogisticRegression(train, test)$testAcc

		sumTrainAcc <- sumTrainAcc + tmpTrain
		sumTestAcc <- sumTestAcc + tmpTest
	}

	sumTrainAcc <- sumTrainAcc / numFold
	sumTrainAcc <- sumTestAcc / numFold

	return (list("trainAcc" = trainAcc, "testAcc" = testAcc))
}


# Add variable block to the data 
# A preprocessing step for cross-validation
BlockData <- function(img1, img2, img3) {
	count <- 1

	meanX <- mean(img1$x)
	meanY <- mean(img1$y)
	img1$block <- count + 2 * 1*(img1$x > meanX) + 1*(img1$y > meanY)
	count <- count + 4

	meanX <- mean(img2$x)
	meanY <- mean(img2$y)
	img2$block <- count + 2 * 1*(img2$x > meanX) + 1*(img2$y > meanY)
	count <- count + 4

	meanX <- mean(img3$x)
	meanY <- mean(img3$y)
	img3$block <- count + 2 * 1*(img3$x > meanX) + 1*(img3$y > meanY)
	count <- count + 4
	
	imgMutate1 <- mutate(img1, Image = 1)
	imgMutate2 <- mutate(img2, Image = 2)
	imgMutate3 <- mutate(img3, Image = 3)
	img <- rbind(imgMutate1, imgMutate2, imgMutate3)

	return (img)
}


# A parallel implementation of CV
# img is the merged data frame
# FUN is a function parameter
BlockCrossValidation <- function(img, FUN) {
	
	sumTrainAcc <- 0
	sumTestAcc <- 0 
	accuracy <- matrix(0, nrow = 2, ncol = 12)

	s <- foreach(i = 1:12, .combine = cbind) %dopar% {
		train <- filter(img, block != i)
		test  <- filter(img, block == i)
		
		result <- FUN(train, test)
		tmpTrain <- result$trainAcc
		tmpTest <- result$testAcc

		return (c(tmpTrain, tmpTest))
	}

	sumTrainAcc <- mean(s[1,])
	sumTestAcc <- mean(s[2,])

	return (list("trainAcc" = sumTrainAcc, "testAcc" = sumTestAcc, "accTable" = s))
}


# type should be "probs" or "label", depending on whether the binaryTest has column pred or not
ErrorVisualization <- function(binaryTest, type, i) {
	if (type == "probs") {
		binaryTest$pred <- 1*(binaryTest$prob > 0.5) + 0
	}
	p <- ggplot(binaryTest) + geom_point(aes(x = x, y = y, color = factor(pred))) +
  			scale_color_discrete(name = "Predicted label")
	#ggsave(filename = paste0("Pred", i, ".png"), plot = p, device = png)
	p
	ggsave(paste0("Pred", i, ".png"))

	p <- ggplot(binaryTest) + geom_point(aes(x = x, y = y, color = factor(label))) +
  			scale_color_discrete(name = "Expert label")
  	p
  	ggsave(paste0("Expert", i, ".png"))
	#ggsave(filename = paste0("Expert", i, ".png"), plot = p, device = png)
}


# not able to make this parallel ...
# should set #cores = 1 when using this feature
BlockCVAndVisualization <- function(img, FUN, type) {
	
	sumTrainAcc <- 0
	sumTestAcc <- 0 
	accuracy <- matrix(0, nrow = 2, ncol = 12)

	s <- foreach(i = 1:12, .combine = cbind) %dopar% {
		train <- filter(img, block != i)
		test  <- filter(img, block == i)
		
		result <- FUN(train, test)
		tmpTrain <- result$trainAcc
		tmpTest <- result$testAcc
		binaryTest <- result$binaryTest

		ErrorVisualization(binaryTest, type, i)
		#ggsave(filename = paste0("Visual", i, "png"), plot = p, device = png)

		return (c(tmpTrain, tmpTest))
	}

	sumTrainAcc <- mean(s[1,])
	sumTestAcc <- mean(s[2,])

	return (list("trainAcc" = sumTrainAcc, "testAcc" = sumTestAcc, "accTable" = s))
}


# type is probs
LogisticRegression <- function(train, test) {
	# need to filter zero label
	binaryTrain <- filter(train, label != 0)
	binaryTest  <- filter(test, label != 0)

	# glm function does not allow negative response...
	binaryTrain[,"label"] <- (binaryTrain[, "label"] + 1) / 2
	binaryTest[,"label"] <- (binaryTest[, "label"] + 1) / 2
	#logit <- glm(label ~ NDAI + CORR + SD + DF, data = binaryTrain, family = "binomial")
	logit <- glm(label ~ NDAI + CORR + SD + DF + CF + BF + AF + AN, data = binaryTrain, family = "binomial")

	binaryTrain$prob <- predict(logit, type = "response")

	#Training accuracy

	binaryTrain <- binaryTrain %>% mutate(pred = 1*(prob > 0.5) + 0)
	binaryTrain <- binaryTrain %>% mutate(acc = 1*(pred == label))
	trainAcc <- sum(binaryTrain$acc) /nrow(binaryTrain)

	#Test accuracy
	binaryTest$prob <- predict(logit, binaryTest, type = "response")
	binaryTest <- binaryTest %>% mutate(pred = 1*(prob > 0.5) + 0)
	binaryTest <- binaryTest %>% mutate(acc = 1*(pred == label))
	testAcc <- sum(binaryTest$acc) /nrow(binaryTest)

	return (list("trainAcc" = trainAcc, "testAcc" = testAcc, "binaryTest" = binaryTest))

}

# type is label
RandomForest <- function(train, test) {
	# need to filter zero label
	binaryTrain <- filter(train, label != 0)
	binaryTest  <- filter(test, label != 0)

	rf <- randomForest(factor(label) ~ NDAI + CORR + SD + DF + CF + BF + AF + AN, binaryTrain, mtry = 3, ntree = 50)
	
	#Training accuracy
	binaryTrain$pred <- predict(rf, binaryTrain, type = "response")
	binaryTrain <- binaryTrain %>% mutate(acc = 1*(pred == label))
	trainAcc <- sum(binaryTrain$acc) /nrow(binaryTrain)

	#Test accuracy
	binaryTest$pred <- predict(rf, binaryTest, type = "response")
	binaryTest <- binaryTest %>% mutate(acc = 1*(pred == label))
	testAcc <- sum(binaryTest$acc) /nrow(binaryTest)

	return (list("trainAcc" = trainAcc, "testAcc" = testAcc, "binaryTest" = binaryTest))
}


# # type is label
AdaBoost <- function(train, test) {
	# need to filter zero label
	binaryTrain <- filter(train, label != 0)
	binaryTest  <- filter(test, label != 0)

	#ad <- ada(factor(label) ~ NDAI + CORR + SD + DF + CF + BF + AF + AN, data = binaryTrain, iter = 50, nu = 1, type = "real")
	ad <- ada(factor(label) ~ NDAI + CORR + SD + DF, data = binaryTrain, iter = 50, nu = 0.8, type = "real")

	#Train accuracy
	binaryTrain$pred <- predict(ad, binaryTrain, type = "vector")
	binaryTrain <- binaryTrain %>% mutate(acc = 1*(pred == label))
	trainAcc <- sum(binaryTrain$acc) /nrow(binaryTrain)

	#Test accuracy
	binaryTest$pred <- predict(ad, binaryTest, type = "vector")
	binaryTest <- binaryTest %>% mutate(acc = 1*(pred == label))
	testAcc <- sum(binaryTest$acc) /nrow(binaryTest)

	return (list("trainAcc" = trainAcc, "testAcc" = testAcc, "binaryTest" = binaryTest))

}



# probably would give up this function due to time constraints
AddNeighbors <- function(img) {
	# This is too see whether including the neighbors will help
	for (i in 1:8) {
		img[, paste0("N", i)] <- rep(0, nrow(img))
	}

	for (r in 1:nrow(img)) {
		x <- img[r, "x"]
		y <- img[r, "y"]

		count <- 0

		for (i in -1:1) {
			for (j in -1:1) {
				if (i == 0 && j == 0) next
				count <- count + 1
				tmpX <- x + i
				tmpY <- y + i
				tmpRow <- filter(img, x == tmpX, y == tmpY)
				img[i, paste0("N", count)] <- tmpRow[1, "NDAI"]
			}
		}
	}
}
