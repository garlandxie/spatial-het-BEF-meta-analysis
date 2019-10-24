# libraries ----
library(metafor)
library(here)
library(readr)

# import ----
df <- read_csv(here("data/final", "escalc_df.csv"))

# calculate effect sizes ----

esc_df <- escalc(measure = "CVR",
       
       # coarse-scale het group: mean, sd, n
       m1i = cs_mean, sd1i = cs_sd, n1i = cs_n,
       
       # fine-scale het group: mean, sd, n 
       m2i = fs_mean, sd2i = fs_sd, n2i = fs_n,
       
       data = df)

# meta-regression: inference ----

# without moderator variables 
rma1 <- rma.uni(yi = yi, vi = vi, data = esc_df)
summary(rma1)
forest(rma1)
funnel(rma1)


