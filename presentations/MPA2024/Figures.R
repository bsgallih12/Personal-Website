library(tidyverse)
library(ggplot2)
library(ggsignif)
library(ggdist)
library(RColorBrewer)
library(ggtext)

data <- read.csv("Cleaned_Data.csv")

data_tt_sym <- read.csv("Cleaned_Data.csv") %>% 
  pivot_longer(cols = c("AthSyT","AgnSyT"),
               values_to = "Symbol_Threat",
               names_to = "Condition") %>% 
  mutate(Condition = as.factor(case_when(
    Condition == "AthSyT" ~ "Atheist",
    Condition == "AgnSyT" ~ "Agnostic"
  )))

data_tt_real <- read.csv("Cleaned_Data.csv") %>% 
  pivot_longer(cols = c("AthReT","AgnReT"),
               values_to = "Real_Threat",
               names_to = "Condition") %>% 
  mutate(Condition = as.factor(case_when(
    Condition == "AthReT" ~ "Atheist",
    Condition == "AgnReT" ~ "Agnostic"
  )))

data_tt_convert <- read.csv("Cleaned_Data.csv") %>% 
  pivot_longer(cols = c("AthConv","AgnConv"),
               values_to = "Conversion_Perception",
               names_to = "Condition") %>% 
  mutate(Condition = as.factor(case_when(
    Condition == "AthConv" ~ "Atheist",
    Condition == "AgnConv" ~ "Agnostic"
  )))

data_tt_accept <- read.csv("Cleaned_Data.csv") %>% 
  pivot_longer(cols = c("AthAcc","AgnAcc"),
               values_to = "Accept_Perception",
               names_to = "Condition") %>% 
  mutate(Condition = as.factor(case_when(
    Condition == "AthAcc" ~ "Atheist",
    Condition == "AgnAcc" ~ "Agnostic"
  )))

## Symbolic Threat Visualization

## Cohen = -.32**

agn_sym <- data_tt_sym %>% 
  filter(Condition == "Agnostic") %>% 
  select(Condition,Symbol_Threat)

ath_sym <- data_tt_sym %>% 
  filter(Condition == "Atheist") %>% 
  select(Condition,Symbol_Threat)

sig_sym_df <- rbind(ath_sym,agn_sym)

data_tt_sym %>% 
  group_by(Condition) %>% 
  summarize(sym_threat = mean(Symbol_Threat),
            label=sprintf("%.2f",round(sym_threat,2)),
            label2=n()) %>%
  ggplot(.,aes(x=Condition,y=sym_threat,fill=Condition)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(x = Condition,
                 y = sym_threat + .25,
                 label=glue::glue("{label} (N = {label2})")),size = 14/.pt,fontface="bold") +
  geom_signif(data=sig_sym_df,
              aes(x=Condition,y=Symbol_Threat),
              map_signif_level = TRUE,
              y_position = c(4.00,4.50),
              comparisons = list(c("Agnostic","Atheist")),
              test = "t.test",
              test.args = list(paired=TRUE)) +
  labs(title="Figure 2. Symbolic Threat by Target",
       x="Target",
       y="Symbolic Threat Rating",
       caption = "Note: ** indicates statistical significance at p < .01") +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0),
        plot.title = ggtext::element_markdown(),
        panel.background = element_rect(fill = "#F0F1EC"),
        plot.background = element_rect(fill = "#F0F1EC",colour = "black",size = 1.5),
        axis.text.x = element_text(size = 14,face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"),
        title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold")) +
  scale_fill_manual(values = c("#005AB5","#DC3220"))

ggsave(filename = "symbol_threat.png",height = 4*1.2,width = 6*1.2,units = c("in"))

## Realistic Threat Visualization

## Cohen d = -.07

agn_real <- data_tt_real %>% 
  filter(Condition == "Agnostic") %>% 
  select(Condition,Real_Threat)

ath_real <- data_tt_real %>% 
  filter(Condition == "Atheist") %>% 
  select(Condition,Real_Threat)

sig_real_df <- rbind(ath_real,agn_real)

data_tt_real %>% 
  group_by(Condition) %>% 
  summarize(real_threat = mean(Real_Threat),
            label=sprintf("%.2f",round(real_threat,2)),
            label2=n()) %>%
  ggplot(.,aes(x=Condition,y=real_threat,fill=Condition)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(x = Condition,
                y = real_threat + .25,
                label=glue::glue("{label} (N = {label2})")),size = 14/.pt,fontface="bold") +
  geom_signif(data=sig_real_df,
              aes(x=Condition,y=Real_Threat),
              map_signif_level = TRUE,
              y_position = c(3.2,3.7),
              comparisons = list(c("Agnostic","Atheist")),
              test = "t.test",
              test.args = list(paired=TRUE)) +
  labs(title="Figure 1. Realistic Threat by Target",
       x="Target",
       y="Realistic Threat Rating",
       caption = "Note: NS indicates non significance") +
  scale_y_continuous(limits = c(0,3.5)) +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_markdown(),
        panel.background = element_rect(fill = "#F0F1EC"),
        plot.background = element_rect(fill = "#F0F1EC",colour = "black",size = 1.5),
        axis.text.x = element_text(size = 14,face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"),
        title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold")) +
  scale_fill_manual(values = c("#005AB5","#DC3220"))

ggsave(filename = "real_threat.png",height = 4*1.2,width = 6*1.2,units = c("in"))

## Acceptance Visualization

## Cohen d = .34

agn_accept <- data_tt_accept %>% 
  filter(Condition == "Agnostic") %>% 
  select(Condition,Accept_Perception)

ath_accept <- data_tt_accept %>% 
  filter(Condition == "Atheist") %>% 
  select(Condition,Accept_Perception)

sig_accept_df <- rbind(ath_accept,agn_accept)

data_tt_accept %>% 
  group_by(Condition) %>% 
  summarize(accept_rating = mean(Accept_Perception),
            label=sprintf("%.2f",round(accept_rating,2)),
            label2=n()) %>%
  ggplot(.,aes(x=Condition,y=accept_rating,fill=Condition)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(x = Condition,
                y = accept_rating + .25,
                label=glue::glue("{label} (N = {label2})")),size = 14/.pt,fontface="bold") +
  geom_signif(data=sig_accept_df,
              aes(x=Condition,y=Accept_Perception),
              map_signif_level = TRUE,
              y_position = c(3.1,3.6),
              comparisons = list(c("Agnostic","Atheist")),
              test = "t.test",
              test.args = list(paired=TRUE)) +
  labs(title="Figure 4. Acceptance Perception by Target",
       x="Target",
       y="Acceptance Perception Rating",
       caption = "Note: ** indicates statistical significance at p < .01") +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_markdown(),
        panel.background = element_rect(fill = "#F0F1EC"),
        plot.background = element_rect(fill = "#F0F1EC",colour = "black",size = 1.5),
        axis.text.x = element_text(size = 14,face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"),
        title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold")) +
  scale_fill_manual(values = c("#005AB5","#DC3220"))

ggsave(filename = "acceptance.png",height = 4*1.2,width = 6*1.2,units = c("in"))

## Conversion Visualization

## Cohen d = .63

agn_convert <- data_tt_convert %>% 
  filter(Condition == "Agnostic") %>% 
  select(Condition,Conversion_Perception)

ath_convert <- data_tt_convert %>% 
  filter(Condition == "Atheist") %>% 
  select(Condition,Conversion_Perception)

sig_convert_df <- rbind(ath_convert,agn_convert)

data_tt_convert %>% 
  group_by(Condition) %>% 
  summarize(convert_rating = mean(Conversion_Perception),
            label=sprintf("%.2f",round(convert_rating,2)),
            label2=n()) %>%
  ggplot(.,aes(x=Condition,y=convert_rating,fill=Condition)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(x = Condition,
                y = convert_rating + .25,
                label=glue::glue("{label} (N = {label2})")),size = 14/.pt,fontface="bold") +
  geom_signif(data=sig_convert_df,
              aes(x=Condition,y=Conversion_Perception),
              map_signif_level = c("***"=.001,"**"=.01,"*"<.05),
              y_position = c(2.75,3.25),
              comparisons = list(c("Agnostic","Atheist")),
              test = "t.test",
              test.args = list(paired=TRUE)) +
  labs(title="Figure 3. Conversion Perception by Target",
       x="Target",
       y="Conversion Perception Rating",
       caption = "Note: *** indicates statistical significance at p < .001") +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0),
        plot.title = element_markdown(),
        panel.background = element_rect(fill = "#F0F1EC"),
        plot.background = element_rect(fill = "#F0F1EC",colour = "black",size = 1.5),
        axis.text.x = element_text(size = 14,face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"),
        title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold")) +
  scale_fill_manual(values = c("#005AB5","#DC3220"))

ggsave(filename = "conversion.png",height = 4*1.2,width = 6*1.2,units = c("in"))

sig_convert_df %>% ggplot(.,aes(x=Conversion_Perception,group=Condition,fill=Condition)) +
  geom_density(alpha=.5) +
  geom_vline(aes(xintercept = 2.37,color="#1E4A76"),linetype="longdash",show.legend = FALSE) +
  geom_vline(aes(xintercept = 1.91,color="#5FA3BF"),linetype="longdash",show.legend = FALSE) +
  labs(title="Conversion Perception Density by Target",
       x="Target",
       y="Conversion Perception Density",
       caption = "Note: The vertical dashed lines represent group level means") +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0)) +
  scale_fill_manual(values = c("#005AB5","#DC3220")) +
  scale_color_manual(values = c("darkred","darkred"))


sig_convert_df %>% ggplot(., aes(x = Condition, y = Conversion_Perception,fill=Condition)) + 
  ggdist::stat_halfeye(adjust = .4,width = .4,
                       .width = c(.5, .95),
                       show.legend = FALSE) + 
  ggdist::stat_dots(side = "left",dotsize = .1,
                    justification = 1.01,binwidth = .5,
                    overflow="compress",
                    show.legend = FALSE) +
  labs(title="Distribution of Scores by Group",
       x="Condition",
       y="Conversion Perception") +
  theme_classic() +
  scale_fill_manual(values = c("#005AB5","#DC3220"))

sig_convert_df %>% ggplot(., aes(x = Condition, y = Conversion_Perception,fill=Condition)) +
  ggdist::stat_slabinterval(side="top",show.legend = FALSE) +
  labs(title="Distribution of Scores by Group",
       x="Condition",
       y="Conversion Perception") +
  coord_flip() +
  theme_classic() +
  scale_fill_manual(values = c("#005AB5","#DC3220"))
