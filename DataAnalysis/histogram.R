# Libraries
library(tidyverse)
library(hrbrthemes)
library(RColorBrewer)
library(viridis)
library(hrbrthemes)
library(forcats)

# Load dataset from github

RICS <- RICS %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0))
#discs40 point5 radius
# plot
p <- data %>%
  #mutate(text = fct_reorder(text, value)) %>%
  ggplot(aes(x=`Steps to Hit`, fill=after_stat(count))) +
  geom_histogram(alpha=0.9, binwidth = 10) +
  scale_fill_viridis(discrete=FALSE) +
  theme_minimal() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()   # Remove minor gridlines
  ) +
  xlab("Steps to Hit") +
  ylab("Count") +
  facet_wrap(~'Steps to Hit') +  #displays data corresponding to variable level
  xlim(0,11000)
p

