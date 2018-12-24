rm(list = ls())
setwd('~/Downloads/ChenZiyue/CCM/vis/')

# reference url: https://rpubs.com/Koundy/71792
theme_publication <- function(base_size = 14, base_family = "Helvetica") {
  library(grid)
  (theme_foundation(base_size = base_size, base_family = base_family) 
    + theme(plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle = 90, vjust = 2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour = "black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour = "#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size = unit(0.2, "cm"),
            legend.spacing = unit(0, "cm"),
            legend.title = element_text(face = "italic"),
            plot.margin = unit(c(10, 5, 5, 5), "mm"),
            strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
            strip.text = element_text(face = "bold")
    ))
}


library(xlsx)
library(tidyverse)
dat0 <- data.frame()
dat <- data.frame()
season <- c("Spring", "Summer", "Autumn", "Winter")
# s <- "Summer"
for (s in season) {
  # sheet <- read.xlsx("data/190ccm8class.xlsx", sheetName = s)
  # # 20171012
  # sheet <- read.xlsx("data/189ccm8class.xlsx", sheetName = s)
  # # 20171017
  # sheet <- read.xlsx("data/ccm8classSum.xlsx", sheetName = s)
  # 20171028
  # sheet <- read.xlsx("data/ccm8classMean.xlsx", sheetName = s)
  # 20181224
  sheet <- read.xlsx("data/CCMO31h8classMean.xlsx", sheetName = s)
  # sheet <- read.xlsx("data/CCMO38h8classMean.xlsx", sheetName = s)
  sheet <- sheet %>% 
    dplyr::rename(city = 城市)
  dat0 <- rbind(dat0, data.frame(sheet, season = s))
  min <- sheet %>% 
    summarise_at(vars(TEM, SSD, PRE, EVP, PRS, RHU, WIN, Dir_WIN), funs(min))
  max <- sheet %>% 
    summarise_at(vars(TEM, SSD, PRE, EVP, PRS, RHU, WIN, Dir_WIN), funs(max))
  mean <- sheet %>% 
    summarise_at(vars(TEM, SSD, PRE, EVP, PRS, RHU, WIN, Dir_WIN), funs(mean))
  sd <- sheet %>% 
    summarise_at(vars(TEM, SSD, PRE, EVP, PRS, RHU, WIN, Dir_WIN), funs(sd))
  se <- sd / sqrt(nrow(sheet))
  variable <- names(sheet)[-1]
  df <- data.frame(season = s, variable = variable, mean = t(mean), sd = t(sd), 
                   se = t(se), min = t(min), max = t(max))
  x <- apply(sheet[, -1], 1, which.max)
  df$number <- sapply(seq_along(variable), function(i) {
    sum(x == i)
  })
  dat <- rbind(dat, df)
}

x <- dat %>% 
  mutate(season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")), 
         variable = factor(variable, levels = c("EVP", "PRE", "PRS", "RHU", 
                                          "SSD", "TEM", "WIN", "Dir_WIN")))

dat0.long <- reshape2::melt(dat0, id.vars = c("season", "city"))
dat0.long <- merge(dat0.long, dat, by = c("season", "variable"))
dat0.long <- dat0.long %>%
  mutate(season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")), 
         variable = factor(variable, levels = c("EVP", "PRE", "PRS", "RHU", 
                                                "SSD", "TEM", "WIN", "Dir_WIN")))

library(ggplot2)
library(ggthemes)
library(drlib)

p <- ggplot(dat0.long, aes(x = reorder_within(variable, value, season, median), 
                           y = value, fill = number)) + 
  geom_violin(draw_quantiles = .5) + 
  labs(x = "Variable", y = expression(rho), fill = "No. of cities") + 
  scale_x_reordered() + 
  facet_wrap(~ season, scales = "free_x") + 
  scale_fill_continuous_tableau(palette = "Classic Area Red") +
  theme_classic() + 
  theme(legend.position = "right", 
        legend.direction = "vertical", 
        legend.key.size = unit(1, "cm"),
        legend.spacing = unit(1, "cm"), 
        legend.title = element_text(face = "plain"), 
        strip.background = element_rect(colour = "gray80", fill = "gray80"), 
        strip.text = element_text(face = "bold"), 
        axis.title = element_text(face = "bold",size = rel(1)))

pdf(file = "figs/violin1_of_p_o3_1h.pdf", width = 10, height = 6)
print(p)
dev.off()
