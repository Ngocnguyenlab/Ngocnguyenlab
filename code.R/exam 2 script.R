library(tidyverse)
head(esoph)
nrow(esoph)
all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)
esoph %>%
  filter(alcgp == "120+") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)
esoph %>%
  filter(alcgp == "0-39g/day") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)
tob_cases <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncases) %>%
  sum()
tob_controls <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncontrols) %>%
  sum()
esoph %>%
  filter(tobgp == "30+") %>%
  summarize(ncontrol = sum(ncontrols)) %>%
  mutate(p_case = ncontrol / all_controls) %>%
  pull(p_case)
esoph %>%
  filter((tobgp == "30+") | (alcgp == "120+")) %>%
  summarize(ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncontrols / all_controls) %>%
  pull(p_case)
0.145+0.225
