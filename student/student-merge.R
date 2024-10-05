
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