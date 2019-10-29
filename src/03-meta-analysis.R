# libraries --------------------------------------------------------------------
library(metafor)
library(here)
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)

# import -----------------------------------------------------------------------
df <- read_csv(here("data/final", "escalc_df.csv"))

# calculate effect sizes -------------------------------------------------------

esc_df <- escalc(measure = "CVR",
       
       # coarse-scale het group: mean, sd, n
       m1i = cs_mean, sd1i = cs_sd, n1i = cs_n,
       
       # fine-scale het group: mean, sd, n 
       m2i = fs_mean, sd2i = fs_sd, n2i = fs_n,
       
       data = df)

esc_df <- esc_df %>%
        rename(lnCV_ES  = yi, # effect size estimate
               samp_var = vi) # corresponding sampling variance

# meta-regression: inference ---------------------------------------------------

# without moderator variables 
# REML for tau-squared estimation: recommended by Viechtbauer (2005)
# Use weighted least squares: weighted = TRUE
rma1 <- rma(yi = lnCV_ES,
            vi = samp_var,
            method = "REML", 
            weighted = TRUE, 
            test = "knha",
            level = 95,
            data = esc_df)

# get tau-squared, I-squared, H-squared, overall effect size + CI's
summary(rma1)

# 95% Wald confidence intervals 
confint(rma1)

# funnel plot variations 
# applies only to rma models without mods 
funnel(rma1, main = "Standard Error")
funnel(rma1, yaxis = "vi", main = "Sampling Variance")
funnel(rma1, yaxis = "seinv", main = "Inverse Standard Error")
funnel(rma1, yaxis = "vinv", main = "Inverse Sampling Variance")

# plot of the (restricted) log-likelhood as a function of tau-squared
profile.rma.uni(rma1)

# funnal plot assymmetry
trimfill.rma.uni(rma1)

# forest plot ------------------------------------------------------------------

# build df for a ggplot2-based forest plot
forest_df <- rbind(
        
        # effect size and SE for each study
        data.frame(ES    = rma1$yi,
                   SE    = sqrt(rma1$vi),
                   Type  = "Study", 
                   Study = paste(esc_df$source, 
                                 esc_df$location, 
                                 sep = " ")
                   ),
        
        # overall effect size estimate across all studies
        data.frame(ES    = rma1$b,
                   SE    = rma1$se,
                   Type  = "Summary",
                   Study = "Summary")
        )

# convert to factor for plotting purposes
forest_df <- forest_df %>%
        mutate(Study2 = factor(Study, rev(levels(Study))))

# create forest plot
custom_forest <- forest_df %>%
        ggplot(aes(
                x = Study2,
                y = ES,
                ymax = ES + (1.96 * SE),
                ymin = ES - (1.96 * SE),
                colour = factor(Type)
        )) +
        geom_pointrange() +
        coord_flip() +
        geom_hline(
                yintercept = 0,
                lty = 2,
                size = 1,
                colour = "grey"
        ) +
        xlab("Study") +
        ylab("Log CV Ratio") +
        scale_colour_manual(values = c("grey", "black")) +
        theme(
                legend.position = "none",
                panel.border = element_rect(fill = NA),
                panel.background = element_blank(),
        )

# save the plot! ---------------------------------------------------------------

# ggplot2-based forest plot
ggsave(here("output/figures", "forest-plot.png"),
       plot = custom_forest,
       height = 7,
       width = 5,
       device = "png")


