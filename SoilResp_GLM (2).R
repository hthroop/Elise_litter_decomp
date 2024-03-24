
data=read.csv("SoilResp_150701a.csv",header=TRUE)

data1 <- data[complete.cases(data[,c("GWC1")]),]
# Analysis of soil respiration data using stepwise regression of General Linear Model with all four possible predictor variables

# Model comparison - GLM of response variable CO_machine; possible predictor variables inlcude Patch_type, Fire_block, GWC1, and Tsoil
full_glm <- glm(CO_machine ~ Patch_type + Fire_block + GWC1 + Tsoil, family = gaussian, data=data1)
step(full_glm)

# split into two datasets to analyze separately by month
feb_resp = data1 [data1$Month == "February",] #take only the data1 from february
april_resp = data1[data1$Month == "April",] #take only the data1 from april

# Model comparison - GLM of response variable CO_machine; possible predictor variables inlcude Patch_type, Fire_block, GWC1, and Tsoil
full_glm_feb <- glm(CO_machine ~ Patch_type + Fire_block + GWC1 + Tsoil, family = gaussian, data1=feb_resp)
step(full_glm_feb)

# Model comparison - GLM of response variable CO_machine; possible predictor variables inlcude Patch_type, Fire_block, GWC1, and Tsoil
full_glm_april <- glm(CO_machine ~ Patch_type + Fire_block + GWC1 + Tsoil, family = gaussian, data1=april_resp)
step(full_glm_april)

