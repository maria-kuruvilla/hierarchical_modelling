# try nls method to fit beverton hold model with viner data

library(tidyverse)
library(here)
library(stats)


chum_data <- read_csv(here("data", "chum_SR_20_hat_yr_w_ocean_covariates.csv")) 

chum_data$River_n <- as.numeric(factor(chum_data$River_GFE_ID))

viner <- chum_data %>% 
  filter(River == "VINER SOUND CREEK") %>% 
  mutate(logR = log(Recruits),
         logS = log(Spawners)) %>%
  select(BroodYear, Spawners, Recruits, ln_RS, logR, logS, River_n)

nls_bh <- nls(ln_RS ~ alpha - log(1 + exp(alpha)*Spawners/Rk),
              data = viner,
              start = list(alpha = 2.1, Rk = max(viner$Recruits)),
              algorithm = "port", 
              lower = c(1,0)
              )

#predict using nls_bh

predict_nls_bh = data.frame(
  Spawners = seq(0, max(viner$Spawners), length.out = 100),
  ln_RS = predict(nls_bh, 
                      newdata = data.frame(Spawners = seq(0, max(viner$Spawners), 
                                                          length.out = 100)), 
                      se.fit = TRUE)
)

# colnames(predict_nls_bh) <- c("Spawners", "ln_RS")


# plot data from viner$ln_RS vs Spawners using ggplot and then add 
# results of nls_bh

viner %>% 
  ggplot(aes(x = Spawners, y = ln_RS)) +
  geom_point(alpha = 0.5) +
  geom_line(data = predict_nls_bh, aes(x = Spawners, y = ln_RS), 
            color = "salmon", size = 2, alpha = 0.5) +
  labs(title = "Beverton-Holt Model Fit to Viner Data using NLS",
       x = "Spawners",
       y = "ln(R/S)") +
  theme(plot.title = element_text(size = 2),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.position = "none") + 
  theme_classic()


ggsave(here("figures", "bh_viner_nls.png"),
       width = 7, height = 5, dpi = 300)

