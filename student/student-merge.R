
library(tidyverse)
library(ellipse)

d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students

d3_all=inner_join(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

#G1, G2, G3, paid, absences
d1_selected <- d1 %>% select(-G1, -G2, -G3, -paid, -absences)
d2_selected <- d2 %>% select(-G1, -G2, -G3, -paid, -absences)
d3_selected=inner_join(d1_selected,d2_selected,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

class(d3_selected)
typeof(d3_selected)

sum_na <- function(column){
  sum(is.na(column))
}

na_counts <- d3_selected |>
  summarize(across(everything(), sum_na))
na_counts

math <- read.table("student-mat.csv",sep=";",header=TRUE)
portuguese <- read.table("student-por.csv",sep=";",header=TRUE)

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

math_stats <- math |>
  select(age,absences,G1,G2,G3) |>
  summarize(across(where(is.numeric), 
                   list("mean" = ~ mean(.x, na.rm = TRUE), 
                        "median" = ~ median(.x, na.rm = TRUE),
                        "sd" = ~sd(.x, na.rm = TRUE),
                        "iqr" = ~IQR(.x, na.rm = TRUE)), 
                   .names = "{.fn}_{.col}"))

math_stats_by_sex <- math |>
  select(sex,age,absences,G1,G2,G3) |>
  group_by(sex) |>
  summarize(across(where(is.numeric), 
                   list("mean" = ~ mean(.x, na.rm = TRUE), 
                        "median" = ~ median(.x, na.rm = TRUE),
                        "sd" = ~sd(.x, na.rm = TRUE),
                        "iqr" = ~IQR(.x, na.rm = TRUE)), 
                   .names = "{.fn}_{.col}"))

math_stats_by_sex_study <- math |>
  select(sex,age,absences,G1,G2,G3,studytime) |>
  group_by(sex,studytime) |>
  summarize(across(where(is.numeric), 
                   list("mean" = ~ mean(.x, na.rm = TRUE), 
                        "median" = ~ median(.x, na.rm = TRUE),
                        "sd" = ~sd(.x, na.rm = TRUE),
                        "iqr" = ~IQR(.x, na.rm = TRUE)), 
                   .names = "{.fn}_{.col}"))


portuguese_stats <- portuguese |>
  select(age,absences,G1,G2,G3) |>
  summarize(across(where(is.numeric), 
                   list("mean" = ~ mean(.x, na.rm = TRUE), 
                        "median" = ~ median(.x, na.rm = TRUE),
                        "sd" = ~sd(.x, na.rm = TRUE),
                        "iqr" = ~IQR(.x, na.rm = TRUE)), 
                   .names = "{.fn}_{.col}"))


ctab_math <- math |>
  select(age,absences,G1,G2,G3) |>
  cor()
as_tibble(ctab_math)

ggplot(math, aes(x=age, fill=sex)) +
  geom_histogram(binwidth=.5, alpha=.5, position="dodge")

ggplot(math, aes(x=age, colour=sex)) + geom_density()

ggplot(math, aes(x=G3, y=absences, fill=sex)) + geom_boxplot()

ggplot(math, aes(x=absences, y=G3)) +
  geom_point(shape=1,position=position_jitter(width=1,height=.5))

ggplot(math, aes(x=absences, y=G3, colour = sex)) +
  geom_point(shape=1,position=position_jitter(width=1,height=.5))


ggplot(portuguese, aes(x=age, fill=sex)) +
  geom_histogram(binwidth=.5, alpha=.5, position="dodge")

ggplot(portuguese, aes(x=age, colour=sex)) + geom_density()

ggplot(portuguese, aes(x=G3, y=absences, fill=sex)) + geom_boxplot()

ggplot(portuguese, aes(x=absences, y=G3, colour = sex)) +
  geom_point(shape=1,position=position_jitter(width=1,height=.5))


ggplot(math, aes(x=rating)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")





