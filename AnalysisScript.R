# Script for analyzing TreeAge output in COVID OB ARDS project

# colours
c_light <- c("#DCBCBC")
c_light_highlight <- c("#C79999")
c_mid <- c("#B97C7C")
c_mid_highlight <- c("#A25050")
c_dark <- c("#8F2727")
c_dark_highlight <- c("#7C0000")

c_blue = "cornflower blue"
c_lightblue = "cadetblue2"

nMom  <- 1000
nIter <- 1000
names <- list("rr1.4",
              "cs0.005",
              "ga28",
              "fl0.01",
              "ards0.25",
              "nnrr2",
              "nnlts1.5",
               "pmcs90",
             "ga30",
              "base",
              "ga34",
              "ards1",
              "pmcs50",
              "fl0.1",
              "muno0.5",
              "rr0.7"
)

library(tidyverse)
  # adjust for your appropriate working drive
setwd("C:/Users/chris/Dropbox/Coursework/Decision Analysis/HAD5304 Project")

filenames <- sapply(names, paste0, ".txt")
df_list <- lapply(filenames, read.table, header=T)
names(df_list) <- names
bigdf <- bind_rows(df_list,.id = "Case")

keep <- grepl("ATTR", names(bigdf)) | names(bigdf) %in% c("Case","ITERATION")

bigdf <- bigdf[,keep]

# fix names

names(bigdf) <- sub("STRATEGY_1", "ElectiveDelivery", names(bigdf))
names(bigdf) <- sub("STRATEGY_2", "ExpectantManagement", names(bigdf))

names(bigdf) <- sub("ATTR_1_", "Mat_QALYs_", names(bigdf))
names(bigdf) <- sub("ATTR_2", "Mat_LYs", names(bigdf))
names(bigdf) <- sub("ATTR_3", "Mat_HospSurv", names(bigdf))

names(bigdf) <- sub("ATTR_4", "NN_QALYs", names(bigdf))
names(bigdf) <- sub("ATTR_5", "NN_LYs", names(bigdf))
names(bigdf) <- sub("ATTR_6", "NN_HospSurv", names(bigdf))

names(bigdf) <- sub("ATTR_7", "MatSurviveNNDeath", names(bigdf))
names(bigdf) <- sub("ATTR_8", "BothSurvive", names(bigdf))
names(bigdf) <- sub("ATTR_9", "BothSurviveTermBirth", names(bigdf))

names(bigdf) <- sub("ATTR_10", "Mat_HospitalLOS", names(bigdf))
names(bigdf) <- sub("ATTR_11", "Mat_VentDuration", names(bigdf))
names(bigdf) <- sub("ATTR_12", "Mat_LongTermComp", names(bigdf))

names(bigdf) <- sub("ATTR_13", "FetalLoss", names(bigdf))
names(bigdf) <- sub("ATTR_14", "NICU_Admit", names(bigdf))
names(bigdf) <- sub("ATTR_15", "NICU_LOS", names(bigdf))

names(bigdf) <- sub("ATTR_16", "NN_LongTermComp", names(bigdf))
names(bigdf) <- sub("ATTR_17", "NN_COVID", names(bigdf))
names(bigdf) <- sub("ATTR_18", "GA", names(bigdf))


# pivot_long

bigdf <- bigdf %>%
  pivot_longer(
    cols = c(3:ncol(bigdf)),
    names_to = c("Variable", "Strategy"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  mutate(Case = ordered(Case,
                        levels = names
                        , labels = c(
                                  "Delivery worsens maternal outcome (RR 1.4)",
                                     "Higher mortality rate of cesarean delivery (0.5% vs 0.1%)",
                                   "GA 28 weeks",
                                   "Fetal loss rate lower (1% vs 5%)",
                                   "Lower ARDS mortality (7% vs 13%)",
                                   "Preterm mortality risk higher (RR 2)",
                                   "Long-term mortality risk of prematurity higher (HR 2)",
                                   "Perimortem delivery survival higher (90% vs 75%)",
                                    "GA 30 weeks",
                                   "Base case*",
                                   "GA 34 weeks",
                                   "Fetal loss rate higher (10% vs 5%)",
                                   "Perimortem delivery survival lower (50% vs 75%)",
                                   "Higher ARDS mortality (24% vs 13%)",
                                   "Lower maternal utility of neonatal outcomes (0.5 vs 0.95)",
                                   "Delivery improves maternal outcome (RR 0.7)")))

diff <- bigdf %>%
  group_by(Case, ITERATION, Variable) %>%
  summarise(Difference = -diff(value)) %>%
  group_by(Case, Variable) %>%
  mutate(MeanDifference = mean(Difference))

df_mom_violin <- diff %>%
  filter(grepl("QALYs", Variable)) %>%
  filter(grepl("Delivery improves maternal", Case)|
           grepl("Delivery worsens maternal",Case) |
           grepl("Lower maternal utility", Case) |
           grepl("weeks", Case)|
           grepl("cesarean", Case)|
           grepl("Base", Case))

case_order <- filter(df_mom_violin, Variable == "Mat_QALYs") %>%
  group_by(Case) %>%
  summarise(MeanDifference = first(MeanDifference)) %>%
  mutate(Rank = rank(MeanDifference)) %>%
  arrange(Rank) %>%
  select(-MeanDifference)



df_mom_violin <- df_mom_violin %>%
  left_join(case_order, by = "Case")

plot_mom_violin <- ggplot(df_mom_violin,
       aes(x = reorder(Case,Rank), 
           y = Difference,
           fill = MeanDifference)) + 
  geom_violin() + 
  facet_grid(.~factor(Variable,
                      levels = c("Mat_QALYs",
                                 "NN_QALYs"),
                      labels = c("",
                                 " ")),
             scales = "free_y") +
  theme_minimal() +
  scale_fill_gradient2(low = c_dark, mid = "white", high = c_blue,
                       breaks = 0,labels = "",
                       name = "") +
  labs(y = "",
       x = "") +
  scale_y_continuous(breaks = c(-2,0,2),
                     limits = c(-4,4)) +
  coord_flip() +
  theme(legend.position = "right",
        plot.caption = element_text(hjust=0))


ggsave(filename = "MatQALYsDiffByScenario.svg",
       plot_mom_violin,
       height = 6,
       width = 8)

df_baby_violin <- diff %>%
  filter(grepl("QALYs", Variable)) %>%
  filter(grepl("Fetal", Case)|
           grepl("ARDS", Case) |
           grepl("Long-term", Case)|
           grepl("Preterm mortality risk higher", Case)|
           grepl("Base", Case) |
           grepl("delivery survival lower", Case))

case_order <- filter(df_baby_violin, Variable == "NN_QALYs") %>%
  group_by(Case) %>%
  summarise(MeanDifference = first(MeanDifference)) %>%
  mutate(Rank = rank(MeanDifference)) %>%
  arrange(Rank) %>%
  select(-MeanDifference)

df_baby_violin <- df_baby_violin %>%
  left_join(case_order, by = "Case")

plot_baby_violin <- ggplot(df_baby_violin,
                          aes(x = reorder(Case,Rank), 
                              y = Difference,
                              fill = MeanDifference)) + 
  geom_violin() + 
  facet_grid(.~factor(Variable,
                      levels = c("Mat_QALYs",
                                 "NN_QALYs"),
                      labels = c("",
                                 " ")),
             scales = "free_y") +
  theme_minimal() +
  scale_fill_gradient2(low = c_dark, mid = "white", high = c_blue,
                       breaks = 0,labels = "",
                       name = "") +
  labs(y = "",
       x = "") +
  scale_y_continuous(breaks = c(-2,0,2),
                     limits = c(-4,4)) +
  coord_flip() +
  theme(legend.position = "right",
        plot.caption = element_text(hjust=0))

ggsave(filename = "NNQALYsDiffByScenario.svg",
       plot_baby_violin,
       height = 6,
       width = 8)


sumdf <- mutate(diff,
               Strategy = "Difference",
               value = Difference) %>%
  select(Case, ITERATION, Variable, Strategy, value) %>%
  bind_rows(bigdf) %>%
  group_by(Case, Variable, Strategy) %>%
  summarise(mean_raw = mean(value),
            CI95lb = quantile(value, 0.025, na.rm=T),
            CI95ub = quantile(value, 0.975, na.rm=T)) %>%
  mutate(mean_raw = ifelse(grepl("LYs", Variable) | 
                             grepl("Vent", Variable),round(mean_raw, 1),
                           round(mean_raw,3)),
         CI95lb = ifelse(grepl("LYs", Variable) | 
                           grepl("Vent", Variable), round(CI95lb, 1),
                           round(CI95lb,3)),
         CI95ub = ifelse(grepl("LYs", Variable) | 
                           grepl("Vent", Variable),round(CI95ub, 1),
                         round(CI95ub,3))) %>%
  mutate(Mean = paste0(mean_raw, " (",
                         CI95lb, " to ",
                         CI95ub, ")"),
         Strategy = ordered(Strategy,
                            levels = c("ElectiveDelivery",
                                       "ExpectantManagement",
                                       "Difference"),
                            labels = c("Elective Delivery",
                                       "Expectant Management",
                                       "Difference")
                            )) %>%
  select(Case, Variable, Strategy, Mean) %>%
  pivot_wider(names_from = Strategy,
              values_from = Mean) %>%
  select(Case, Variable, "Elective Delivery", "Expectant Management", Difference)

tbl2 <- filter(sumdf, 
               Case == "Base case*") %>%
  write_csv(file = "Tbl2.csv")

tbl3 <- filter(sumdf,
               grepl("LongTerm",Variable) |
               grepl("LYs", Variable) |
               grepl("COVID",Variable) |
               grepl("Surv", Variable)|
               grepl("Both", Variable)) %>%
  select(Case, Variable, Difference) %>%
  pivot_wider(names_from = Case,
              values_from = Difference) %>%
  write_csv(file = "Tbl3.csv")


