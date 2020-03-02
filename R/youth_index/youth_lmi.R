library(tidyverse)

lmi_data <- read_excel("~/polybox/Youth Employment/1b Index/youth-lmi-master/last_available_year_YLILI.xlsx")

rescale <- function(x, na.rm = FALSE) (100-x)

indicators <- lmi_data %>% 
  select(country_name, country_abbreviation, SOYN, RUR, YSMR, YWPR_EPMP, YTRUR, SYIE, YVER, SYEO, SYSAF, SYNSE, YIR, YTRUR) %>% 
  mutate_at(c("SOYN", "YSMR", "YWPR_EPMP", "YTRUR", "SYIE", "YVER", "SYEO", "SYSAF", "SYNSE", "YIR", "YTRUR"), rescale)



