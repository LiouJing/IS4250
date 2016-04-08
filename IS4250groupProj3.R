mydata <- group3.dataset
myscenario <- group3.scenario

# randomly allocate a choice-set for each respondent
for(i in 1:25){
  cs <- sample(c(1:10), 1);
  row <- cs*2;
  mydata$Treatment[2*i-1] <- myscenario$Treatment[row];
  mydata$Treatment[2*i] <- myscenario$Treatment[row+1];
  mydata$Involvement[2*i-1] <- myscenario$Involvement[row];
  mydata$Involvement[2*i] <- myscenario$Involvement[row+1];
  mydata$Care.prog[2*i-1] <- myscenario$Care.prog[row];
  mydata$Care.prog[2*i] <- myscenario$Care.prog[row+1];
  mydata$Outcome.measurement[2*i-1] <- myscenario$Outcome.measurement[row];
  mydata$Outcome.measurement[2*i] <- myscenario$Outcome.measurement[row+1];
  mydata$Pharmacotherapy[2*i-1] <- myscenario$Pharmacotherapy[row];
  mydata$Pharmacotherapy[2*i] <- myscenario$Pharmacotherapy[row+1];
  mydata$Governance[2*i-1] <- myscenario$Governance[row];
  mydata$Governance[2*i] <- myscenario$Governance[row+1];
}

# randomly generate respondents' choice
for(i in 1:50){
  if(i%%2==0){
    y <- mydata[(i-1), 'Choice'];
    x <- 1-y;
  } else {
    x <- sample(c(0,1), 1);
  }
  mydata$Choice[i] <- x  
}

# use logit model to determine which factors are significanlt important
mylogit <- glm(Choice ~ Treatment + Involvement + Care.prog + Outcome.measurement + Pharmacotherapy + Governance, data = mydata, family = "binomial")
summary(mylogit)
