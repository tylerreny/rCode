library(broom)
library(tidyverse)

# function to create fake datasets
createData <- function(){
  n <- 1000
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  x4 <- rnorm(n)
  betas <- sample(seq(0,0.5,length.out=n), 5, replace=T)
  e <- rnorm(n)
  z <- betas[1] + betas[2]*x1 + betas[3]*x2 + betas[4] * x3 + betas[5]* x4 + e
  pr <- 1/(1 + exp(-z))
  y <- rbinom(n,1,pr)
  dat <- data.frame(y,x1,x2,x3,x4)
  return(dat)
}

# create fake datasets
df1 <- createData()
df2 <- createData()
df3 <- createData()
df4 <- createData()

# run models
out1 <- glm(y ~ x1 + x2 + x3 + x4, data=df1, family = binomial (link='logit'))
out2 <- glm(y ~ x1 + x2 + x3 + x4, data=df2, family = binomial (link='logit'))
out3 <- glm(y ~ x1 + x2 + x3 + x4, data=df3, family = binomial (link='logit'))
out4 <- glm(y ~ x1 + x2 + x3 + x4, data=df4, family = binomial (link='logit'))

# tidy models

tidyModel <- function(modelObject,modelName){
  out <- tidy(modelObject) %>%
    mutate(lower = estimate - 1.96 * std.error,
           upper = estimate + 1.96 * std.error,
           Model=modelName) %>%
    filter(term != '(Intercept)')
  return(out)
}

m1 <- tidyModel(out1,'Model 1')
m2 <- tidyModel(out2,'Model 2')
m3 <- tidyModel(out3,'Model 3')
m4 <- tidyModel(out4,'Model 4')

bind_rows(m1, m2, m3, m4) %>%
  ggplot(aes(reorder(term,estimate), estimate, color=Model)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.25, position=position_dodge(width=0.3)) + 
  geom_point(position=position_dodge(width=0.3),shape=21, fill='white',size=3) +
  scale_color_brewer(palette='Set2') +
  coord_flip() + 
  geom_hline(yintercept=0,linetype=2,color='red') +
  labs(x='',y='Coefficient')
