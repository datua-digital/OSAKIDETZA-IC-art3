# explorando Cox
library(JMbayes)
source("utils/jm_utils.R")
source("utils/table_utils.R")




coxph(surv_object ~ sexo, cluster = id, cox_df)
coxph(surv_object ~ edad_ing1, cluster = id, cox_df)
coxph(surv_object ~ charlson, cluster = id, cox_df)
coxph(surv_object ~ fe.reducida.severa, cluster = id, cox_df)

coxph(surv_object ~ sexo + edad_ing1  + charlson + fe.reducida.severa, cluster = id, cox_df)