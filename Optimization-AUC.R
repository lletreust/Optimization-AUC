library(tidyverse)
library(pROC)

bw <- MASS::birthwt %>% 
  select(low, smoke, ui, ht)



aire <- function(coef) {
  beta_0 <- coef[1]
  beta_smoke <- coef[2]
  beta_ht <- coef[3]
  beta_ui <- coef[4]
  bw %>% 
    mutate(
      fitted_values = plogis(beta_0 + beta_smoke * smoke + beta_ht * ht + beta_ui * ui),
    ) %>%
    summarise(
      c.index = auc(roc(bw$low ~ fitted_values))
    ) %>%
    pull(c.index)
}



opt <- optim(
  par = c(b_0 = 0, b_smoke = 0, b_ht = 0, b_ui = 0),
  fn = aire, 
  control = list(fnscale = -1)
)

opt
names(opt)
opt$par
aire(c(opt$par[1], opt$par[2], opt$par[3], opt$par[4]))

