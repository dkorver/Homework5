
library(tidyverse)

d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students


d3=inner_join(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

#G1, G2, G3, paid, absences
d1_selected <- d1 %>% select(-G1, -G2, -G3, -paid, -absences)
d2_selected <- d2 %>% select(-G1, -G2, -G3, -paid, -absences)
d3_selected=inner_join(d1_selected,d2_selected,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

class(d1)
typeof(d1)

sum_na <- function(column){
  sum(is.na(column))
}

na_counts <- d3_selected |>
  summarize(across(everything(), sum_na))
na_counts

#Medu Fedu sex famsize
d3_selected <- d3_selected |>
  mutate(SexF = factor(sex, levels = c("F", "M"), labels = c("Female", "Male")),
         MeduF = as.factor(Medu),
         FeduF = as.factor(Fedu),
         famsizeF = as.factor(famsize))

d3_selected |> count(Medu)
d3_selected |> count(Fedu)
d3_selected |> count(sex,SexF)
d3_selected |> count(famrel.x)
d3_selected |> count(traveltime.x)

#age,absences,G3 G1 G2 G3
d1 <- d1 |>
  mutate(SexF = factor(sex, levels = c("F", "M"), labels = c("Female", "Male")),
         MeduF = as.factor(Medu),
         FeduF = as.factor(Fedu),
         addressF = as.factor(address),
         famsizeF = as.factor(famsize))

d1 |>
  select(SexF,age,absences,G3) |>
  group_by(SexF) |>
  drop_na(SexF) |>
  summarize(across(where(is.numeric), 
                   list("mean" = ~ mean(.x, na.rm = TRUE), 
                        "median" = ~ median(.x, na.rm = TRUE),
                        "sd" = ~sd(.x, na.rm = TRUE),
                        "iqr" = ~IQR(.x, na.rm = TRUE)), 
                   .names = "{.fn}_{.col}"))

d1 |>
  select(addressF,famsizeF,SexF,age,absences,G3) |>
  group_by(addressF,famsizeF) |>
  drop_na(SexF) |>
  summarize(across(where(is.numeric), 
                   list("mean" = ~ mean(.x, na.rm = TRUE), 
                        "median" = ~ median(.x, na.rm = TRUE),
                        "sd" = ~sd(.x, na.rm = TRUE),
                        "iqr" = ~IQR(.x, na.rm = TRUE)), 
                   .names = "{.fn}_{.col}"))




