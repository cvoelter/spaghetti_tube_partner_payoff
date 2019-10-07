rm(list = ls())

library (car)
library(lme4)
load("exp2_glmm1_CI.RData")

#Exp 2 - GLMM 1 - DV: tool transfer

contr=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
                 
boot.mm1.2=boot.glmm(model.res=mm01.2, excl.warnings=T, nboots=1000, para=T)

mm1.2.stab=glmm.model.stab(model.res=mm01.2, contr=contr)

#CIs for plots

pred.mm01.2.gesture.ci=boot.glmm.pred(model.res=pred.mm01.2.gesture , resol=100, level=0.95, use="Gesture", n.cores="all-1", para=T)
pred.mm01.2.NS3.ci=boot.glmm.pred(model.res=pred.mm01.2.NS3 , resol=100, level=0.95, use="NonsocialRoom3", n.cores="all-1", para=T)
pred.mm01.2.Room4.ci=boot.glmm.pred(model.res=pred.mm01.2.Room4 , resol=100, level=0.95, use="NonsocialRoom4", n.cores="all-1", para=T)
pred.mm01.2.ST4.ci=boot.glmm.pred(model.res=pred.mm01.2.ST4 , resol=100, level=0.95, use="SpaghettiTube", n.cores="all-1", para=T)
pred.mm01.2.Presence.ci=boot.glmm.pred(model.res=pred.mm01.2.Presence, resol=100, level=0.95, use="Presence", n.cores="all-1", para=T)

save.image("SpaghettitTube_partner_payoffs_exp2_GLMM01_calculatedCIs.Rdata")

####################
rm(list = ls())
library (car)
library(lme4)
load("exp2_glmm2_CI.RData")

#Exp 2 - GLMM 2 - DV: tool in tube

contr=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))

boot.mm2.2=boot.glmm(model.res=mm2.2, excl.warnings=T, nboots=1000, para=T)

mm2.2.stab=glmm.model.stab(model.res=mm2.2, contr=contr)

#CIs for plots

pred.mm2.2.Room4.ci=boot.glmm.pred(model.res=pred.mm2.2.Room4 , resol=100, level=0.95, use="SpaghettiTube", n.cores="all-1", para=T)


save.image("SpaghettitTube_partner_payoffs_exp2_GLMM02_calculatedCIs.Rdata")


####################
rm(list = ls())
library (car)
library(lme4)
load("exp2_GLMM_S02_HighPassing.rData")

#Exp 2 - GLMM S2 - DV: high passing

contr=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))

boot.mm.S2=boot.glmm(model.res=mm.S2, excl.warnings=T, nboots=1000, para=T)

mm.S2.stab=glmm.model.stab(model.res=mm.S2, contr=contr)

#CIs for plots

pred.mm.S2.ci=boot.glmm.pred(model.res=pred.mm.S2 , resol=100, level=0.95, use="Species", n.cores="all-1", para=T)

save.image("SpaghettitTube_partner_payoffs_exp2_GLMM_S02_calculatedCIs.Rdata")


####################
rm(list = ls())
library (car)
library(lme4)
load("exp2_glmm_S3_CI.rData")

#Exp 2 - GLMM 2 - DV: gestures 
contr=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))

boot.mm.S3=boot.glmm(model.res=mm.S3, excl.warnings=T, nboots=1000, para=T)

mm.S3.stab=glmm.model.stab(model.res=mm.S3, contr=contr)

#CIs for plots

pred.mm.S3.ci=boot.glmm.pred(model.res=pred.mm.S3 , resol=100, level=0.95, use="", n.cores="all-1", para=T)

save.image("SpaghettitTube_partner_payoffs_exp2_GLMM_S03_calculatedCIs.Rdata")


##############################
rm(list = ls())
library (car)
library(lme4)
load("exp2_glmm1_CI.RData")

#Exp 2 - GLMM 1 - DV: tool transfer (w/0 gesture)

contr=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))

boot.mm.S1=boot.glmm(model.res=mm.S1, excl.warnings=T, nboots=1000, para=T)

mm.S1.stab=glmm.model.stab(model.res=mm.S1, contr=contr)

save.image("SpaghettitTube_partner_payoffs_exp2_GLMM_S01_calculatedCIs.Rdata")
                   