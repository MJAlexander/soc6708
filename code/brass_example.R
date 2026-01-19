library(tidyverse)
library(here)

dm <- read_table("https://www.prdh.umontreal.ca/BDLC/data/ont/bltper_1x1.txt", skip = 2)
dc <- read_table("https://www.prdh.umontreal.ca/BDLC/data/can/bltper_1x1.txt", skip = 2)

brass_std <- dc |> filter(Year==1970) |> 
  select(Age, lx) |> 
  mutate(lx = lx/100000) |> 
  rowwise() |> 
  mutate(logit_lx = log(lx/(1-lx)))  |> 
  select(Age, logit_lx)

on_logit <- dm |> 
  select(Age, Year, lx) |> 
  mutate(lx = lx/100000) |> 
  rowwise() |> 
  mutate(logit_lx_on = log(lx/(1-lx))) |> 
  select(Age, Year, logit_lx_on)

df <- on_logit |> 
  left_join(brass_std) |> 
  filter(Age!=0) |> 
  mutate(Age = as.numeric(Age)) |> 
  drop_na()

df <- df[!is.infinite(rowSums(df)),]

coef <- df |> 
  group_by(Year) |> 
  nest() |> 
  mutate(model = map(data, ~lm(logit_lx_on~logit_lx, data = .x))) |> 
  mutate(tidy = map(model, broom::tidy)) |> 
  unnest(tidy)

coef |> 
  mutate(term = ifelse(term == "logit_lx", "beta", "alpha")) |> 
  select(Year, term, estimate) |> 
  ggplot(aes(Year, estimate, color = term)) + 
  geom_line()+
  facet_wrap(~term, scales = "free_y")



