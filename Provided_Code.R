# install.packages('e1071') # <-- You need to do this in order to use SVM
library(e1071) # Load the package with SVM implementation

make_one_random_spell = function() {
  rbinom(26,1,0.1) #0.3
}

# Generates a random dataset with n spells
make_n_random_spells = function(n) {
  r = c()
  for (i in 1:n) { r = rbind(r, make_one_random_spell()); }
  r
}

# Assigns true class to given spell(s)
true_spell_class = function(spell) {
  w = c(1,-2,3,-4,5,-6,7,-8,9,-10,11,-12,13,-14,15,
        -16,17,-18,19,-20,21,-22,23,-24,25,-26)
  
  sign(spell %*% w - 27.5)
}

#BEGIN
sauron_test = function() {
  # Sauron generates a dataset:
  spells = make_n_random_spells(20)
  c = true_spell_class(spells)
  
  # First student finds the majority class
  majority_class = sign(sum(c) + 0.5)
  
  
  # Second student trains an SVM:
  svm_model = svm(spells, c, type='C', kernel='linear')
  
  
  #Second student uses a Decision Tree
  library(rpart)
  library("party") #install.packages("party")
  
  test = data.frame(make_n_random_spells(10))
  test$Sum <- rowSums(test)
  test$ID <- seq.int(nrow(test))
  class(test)
  
  train = data.frame(make_n_random_spells(50))  #class(train) remove(train) ?plot
  train$Sum <- rowSums(train)
  train$Good <- 0
  train$Good[ train$Sum >= 2 ]  <- 1 
  train$ID <- seq.int(nrow(train))
  
  dt_model1 <- ctree(Good~Sum, data=train)
  print(dt_model1)
  summary(dt_model1)
  plot(dt_model1)
  
  #which(train$Good == 1)
  my_prediction1 <- predict(dt_model1, test)
  my_sol1 <- data.frame(ID = test$ID, Good = my_prediction1)
  
  dt_model <- rpart(Good~Sum, data=train)
  plot(dt_model)
  my_prediction <- predict(dt_model, test)
  my_sol <- data.frame(ID = test$ID, Good = my_prediction)
  

  
  # Sauron generates a new example:
  test_example = make_one_random_spell()
  
  # .. and tests each student's predictions:
  student1_correct = (majority_class == true_spell_class(test_example))
  student2_correct = (predict(svm_model, t(test_example)) == true_spell_class(test_example))
  
  c(student1_correct, student2_correct)
}

# Runs the sauron test many times
many_sauron_tests = function(n) {
  r = c()
  for (i in 1:n) {
    # Sometimes svm training fails for some weird reason. Just ignore these cases.
    x = tryCatch(sauron_test(), error=function(err) { c(); })
    r = rbind(r, x)
  }
  r
}

main = function() {
  results = many_sauron_tests(1000)
  
  # Analyze student's performance (expected generalization error):
  print("First student:")
  print(table(results[,1]))
  
  print("Second student:")
  print(table(results[,2]))
  
  # These are the cases where student's predictions disagree:
  disagreements = results[which(results[,1] != results[,2]),]
  
  #agreements = results[which(results[,1] == results[,2]),]
  
  # How often is the first student correct?
  print("If students disagree, the first one will be correct with probability:")
  sum(disagreements[,1])/length(disagreements[,1])
}

main()

#First point: How often would the first student guess correctly?
#I will do a prop.table

#class(results)  remove(disagreements) remove(results) remove(my_results)
#results = many_sauron_tests(1000)
my_results <- data.frame( many_sauron_tests(1500) )

#
prop.table(table(my_results$X1))
prop.table(table(my_results$X2))








