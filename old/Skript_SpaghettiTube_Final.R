setwd("X:/R/Spaghetti tube_phase7")

getwd()

xdata=read.table(file="Spaghetti_Tube_orang_chimp_data_JahagaTrial.txt", header=T, sep="\t")

str(xdata)
summary(xdata)

#xdata=subset(xdata, Phase!="Pretest")



#z.Session=as.vector(scale(xdata$Session))
#z.Trial=as.vector(scale(xdata$Trial))

#relevel
xdata$Nonsocial.Room.3=relevel(xdata$Nonsocial.Room.3, ref="High")
xdata$Spaghetti.Tube=relevel(xdata$Spaghetti.Tube, ref="High")
xdata$Nonsocial.Room.4=relevel(xdata$Nonsocial.Room.4, ref="High")

xdata$Presence=relevel(xdata$Presence, ref="no")
xdata$BeggingGestures=relevel(xdata$BeggingGestures, ref="yes")
xdata$AttgetObeg=relevel(xdata$AttgetObeg, ref="yes")


xdata$Tool.nach.passing=relevel(xdata$Tool.nach.passing, ref="yes")
xdata$Tool.nach.success=relevel(xdata$Tool.nach.success, ref="yes")


library(lme4)

#setwd("C:/Users/Christoph/R/Stats_course")
#setwd("X:/Statistics/R scripts")
source("W:/Statistics/R scripts/Roger/Diagnostic_fcns.r")



xx.fe.re=fe.re.tab(fe.model="tool_trans ~ Nonsocial.Room.3 * Spaghetti.Tube *Nonsocial.Room.4*Presence+Species +Session+Trial",
                   re="(1|Room.3)+(1|Room.4)+(1|Dyad)",
                   data=data.frame(xdata))

xx.fe.re$summary

zdata=xx.fe.re$data

z.Session=as.vector(scale(zdata$Session))
z.Trial=as.vector(scale(zdata$Trial))


contr=glmerControl(optimizer="bobyqa",
                   optCtrl=list(maxfun=100000000))


###full model with 3way interaction

#contr=glmerControl(optimizer="bobyqa",
#                   optCtrl=list(maxfun=1000000))


#res=glmer(tool_trans~Nonsocial.Room.3 * Spaghetti.Tube*Nonsocial.Room.4 + 
#            Species+ z.Session+
#            
#            (1|Room.3)+(1|Room.4)+ (1|Dyad)+
#            
#            (0+z.Session|Room.3)+ (0+z.Session|Room.4)+(0+z.Session|Dyad)+
#            
#            (0+Nonsocial.Room.3.Low|Room.3)+(0+Spaghetti.Tube.Nothing|Room.3)+(0+Nonsocial.Room.4.Low|Room.3)+
#            (0+Nonsocial.Room.3.Low|Room.4)+(0+Spaghetti.Tube.Nothing|Room.4)+(0+Nonsocial.Room.4.Low|Room.4)+
#            (0+Nonsocial.Room.3.Low|Dyad)+(0+Spaghetti.Tube.Nothing|Dyad)+(0+Nonsocial.Room.4.Low|Dyad)+
#            
#            (0+I(Nonsocial.Room.3.Low * Spaghetti.Tube.Nothing*Nonsocial.Room.4.Low)|Room.3)+
#            (0+I(Nonsocial.Room.3.Low * Spaghetti.Tube.Nothing*Nonsocial.Room.4.Low)|Room.4)+
#            (0+I(Nonsocial.Room.3.Low * Spaghetti.Tube.Nothing*Nonsocial.Room.4.Low)|Dyad), 
#          data=zdata, family=binomial
#          ,control=contr
#)


#null=glmer(tool_trans~1 +
#             (1|Room.3)+(1|Room.4)+ (1|Dyad)+
             
#             (0+z.Session|Room.3)+ (0+z.Session|Room.4)+(0+z.Session|Dyad)+
             
#             (0+Nonsocial.Room.3.Low|Room.3)+(0+Spaghetti.Tube.Nothing|Room.3)+(0+Nonsocial.Room.4.Low|Room.3)+
#             (0+Nonsocial.Room.3.Low|Room.4)+(0+Spaghetti.Tube.Nothing|Room.4)+(0+Nonsocial.Room.4.Low|Room.4)+
#             (0+Nonsocial.Room.3.Low|Dyad)+(0+Spaghetti.Tube.Nothing|Dyad)+(0+Nonsocial.Room.4.Low|Dyad)+
             
#            (0+I(Nonsocial.Room.3.Low * Spaghetti.Tube.Nothing*Nonsocial.Room.4.Low)|Room.3)+
#             (0+I(Nonsocial.Room.3.Low * Spaghetti.Tube.Nothing*Nonsocial.Room.4.Low)|Room.4)+
#             (0+I(Nonsocial.Room.3.Low * Spaghetti.Tube.Nothing*Nonsocial.Room.4.Low)|Dyad), 
#           data=zdata, family=binomial
#           ,control=contr
#)

#summary(res)
#anova(null, res, test="Chisq")
#drop1(res, test="Chisq", control=contr) 	


# > > > > 3 way interaction not significant, Model in drop1 did not converge



#all 2 way interactions	___________________________________________________________________________________________________________________				



#red1=glmer(tool_trans~(Nonsocial.Room.3 + Spaghetti.Tube+Nonsocial.Room.4)^2  + 
#             Species+ z.Session+
#             (1|Room.3)+(1|Room.4)+ (1|Dyad)+
             
#             (0+z.Session|Room.3)+ 
#             (0+z.Session|Room.4)+
#             (0+z.Session|Dyad)+
             
#             (0+Nonsocial.Room.3.Low|Room.3)+ 
#             (0+Nonsocial.Room.3.Low|Room.4)+
#             (0+Nonsocial.Room.3.Low|Dyad)+
             
#             (0+Spaghetti.Tube.Nothing|Room.3)+ 
#             (0+Spaghetti.Tube.Nothing|Room.4)+
#             (0+Spaghetti.Tube.Nothing|Dyad)+
             
#             (0+Nonsocial.Room.4.Low|Room.3)+ 
#             (0+Nonsocial.Room.4.Low|Room.4)+
#             (0+Nonsocial.Room.4.Low|Dyad)+
             
#             (0+I(Nonsocial.Room.3.Low*Nonsocial.Room.4.Low)+I(Spaghetti.Tube.Nothing*Nonsocial.Room.3.Low)+I(Spaghetti.Tube.Nothing*Nonsocial.Room.4.Low)|Room.3)+
#             (0+I(Nonsocial.Room.3.Low*Nonsocial.Room.4.Low)+I(Spaghetti.Tube.Nothing*Nonsocial.Room.3.Low)+I(Spaghetti.Tube.Nothing*Nonsocial.Room.4.Low)|Room.4)+
#             (0+I(Nonsocial.Room.3.Low*Nonsocial.Room.4.Low)+I(Spaghetti.Tube.Nothing*Nonsocial.Room.3.Low)+I(Spaghetti.Tube.Nothing*Nonsocial.Room.4.Low)|Dyad), 
#           data=zdata, family=binomial
#           ,control=contr
#)


#null1=glmer(tool_trans~1 +
#              (1|Room.3)+(1|Room.4)+ (1|Dyad)+
#              
#              (0+z.Session|Room.3)+ 
#              (0+z.Session|Room.4)+
#              (0+z.Session|Dyad)+
#              
#              (0+Nonsocial.Room.3.Low|Room.3)+ 
#              (0+Nonsocial.Room.3.Low|Room.4)+
#             (0+Nonsocial.Room.3.Low|Dyad)+
#              
#              (0+Spaghetti.Tube.Nothing|Room.3)+ 
#              (0+Spaghetti.Tube.Nothing|Room.4)+
#              (0+Spaghetti.Tube.Nothing|Dyad)+
              
#              (0+Nonsocial.Room.4.Low|Room.3)+ 
#              (0+Nonsocial.Room.4.Low|Room.4)+
#              (0+Nonsocial.Room.4.Low|Dyad)+
              
#              (0+I(Nonsocial.Room.3.Low*Nonsocial.Room.4.Low)+I(Spaghetti.Tube.Nothing*Nonsocial.Room.3.Low)+I(Spaghetti.Tube.Nothing*Nonsocial.Room.4.Low)|Room.3)+
#              (0+I(Nonsocial.Room.3.Low*Nonsocial.Room.4.Low)+I(Spaghetti.Tube.Nothing*Nonsocial.Room.3.Low)+I(Spaghetti.Tube.Nothing*Nonsocial.Room.4.Low)|Room.4)+
#              (0+I(Nonsocial.Room.3.Low*Nonsocial.Room.4.Low)+I(Spaghetti.Tube.Nothing*Nonsocial.Room.3.Low)+I(Spaghetti.Tube.Nothing*Nonsocial.Room.4.Low)|Dyad), 
#            data=zdata, family=binomial
#            ,control=contr
#)

#summary(red1)
#anova(null1, red1, test="Chisq")
#drop1(red1, test="Chisq", control=contr) 	





#####only Room 4 interaction_______________________________________________________________________________________________


red2=glmer(tool_trans~Nonsocial.Room.3 + (Spaghetti.Tube*Nonsocial.Room.4)  + 
             Species+ z.Session+
             (1|Room.3)+(1|Room.4)+ (1|Dyad)+
             
             (0+z.Session|Room.3)+ 
             (0+z.Session|Room.4)+
             (0+z.Session|Dyad)+
             
             (0+Nonsocial.Room.3.Low|Room.3)+ 
             (0+Nonsocial.Room.3.Low|Room.4)+
             (0+Nonsocial.Room.3.Low|Dyad)+
             
             (0+Spaghetti.Tube.Nothing|Room.3)+ 
             (0+Spaghetti.Tube.Nothing|Room.4)+
             (0+Spaghetti.Tube.Nothing|Dyad)+
             
             (0+Nonsocial.Room.4.Low|Room.3)+ 
             (0+Nonsocial.Room.4.Low|Room.4)+
             (0+Nonsocial.Room.4.Low|Dyad)+
             
             (0+I(Spaghetti.Tube.Nothing*Nonsocial.Room.4.Low)|Room.3)+
             (0+I(Spaghetti.Tube.Nothing*Nonsocial.Room.4.Low)|Room.4)+
             (0+I(Spaghetti.Tube.Nothing*Nonsocial.Room.4.Low)|Dyad), 
           data=zdata, family=binomial
           ,control=contr
)


null2=glmer(tool_trans~1 +
              (1|Room.3)+(1|Room.4)+ (1|Dyad)+
              
              (0+z.Session|Room.3)+ 
              (0+z.Session|Room.4)+
              (0+z.Session|Dyad)+
              
              (0+Nonsocial.Room.3.Low|Room.3)+ 
              (0+Nonsocial.Room.3.Low|Room.4)+
              (0+Nonsocial.Room.3.Low|Dyad)+
              
              (0+Spaghetti.Tube.Nothing|Room.3)+ 
              (0+Spaghetti.Tube.Nothing|Room.4)+
              (0+Spaghetti.Tube.Nothing|Dyad)+
              
              (0+Nonsocial.Room.4.Low|Room.3)+ 
              (0+Nonsocial.Room.4.Low|Room.4)+
              (0+Nonsocial.Room.4.Low|Dyad)+
              
              (0+I(Spaghetti.Tube.Nothing*Nonsocial.Room.4.Low)|Room.3)+
              (0+I(Spaghetti.Tube.Nothing*Nonsocial.Room.4.Low)|Room.4)+
              (0+I(Spaghetti.Tube.Nothing*Nonsocial.Room.4.Low)|Dyad), 
            data=zdata, family=binomial
            ,control=contr
)


summary(red2)
anova(null2, red2, test="Chisq")
drop1(red2, test="Chisq", control=contr) 	


#only main effects__________________________________________________________________________________________________________

red3=glmer(tool_trans~Nonsocial.Room.3 + Spaghetti.Tube + Nonsocial.Room.4  + Species + z.Session +
             (1|Room.3)+(1|Room.4)+ (1|Dyad)+
             
             (0+z.Session|Room.3)+ 
             (0+z.Session|Room.4)+
             (0+z.Session|Dyad)+
             
             (0+Nonsocial.Room.3.Low|Room.3)+ 
             (0+Nonsocial.Room.3.Low|Room.4)+
             (0+Nonsocial.Room.3.Low|Dyad)+
             
             (0+Spaghetti.Tube.Nothing|Room.3)+ 
             (0+Spaghetti.Tube.Nothing|Room.4)+
             (0+Spaghetti.Tube.Nothing|Dyad)+
             
             (0+Nonsocial.Room.4.Low|Room.3)+ 
             (0+Nonsocial.Room.4.Low|Room.4)+
             (0+Nonsocial.Room.4.Low|Dyad),
           
           data=zdata, family=binomial
           ,control=contr
)


null3=glmer(tool_trans~1 +
              (1|Room.3)+(1|Room.4)+ (1|Dyad)+
              
              (0+z.Session|Room.3)+ 
              (0+z.Session|Room.4)+
              (0+z.Session|Dyad)+
              
              (0+Nonsocial.Room.3.Low|Room.3)+ 
              (0+Nonsocial.Room.3.Low|Room.4)+
              (0+Nonsocial.Room.3.Low|Dyad)+
              
              (0+Spaghetti.Tube.Nothing|Room.3)+ 
              (0+Spaghetti.Tube.Nothing|Room.4)+
              (0+Spaghetti.Tube.Nothing|Dyad)+
              
              (0+Nonsocial.Room.4.Low|Room.3)+ 
              (0+Nonsocial.Room.4.Low|Room.4)+
              (0+Nonsocial.Room.4.Low|Dyad),
            
            data=zdata, family=binomial
            ,control=contr
)


summary(red3)
anova(null3, red3, test="Chisq")
drop1(red3, test="Chisq", control=contr) 	

#play around with reference categories:__________________________________________________________________________
test.data=zdata
z.Session=as.vector(scale(test.data$Session))
z.Trial=as.vector(scale(test.data$Trial))


ns3.levels=levels(test.data$Nonsocial.Room.3)
st.levels=levels(test.data$Spaghetti.Tube)
ns4.levels=levels(test.data$Nonsocial.Room.4)
all.ll=c()
for(ns3 in 1:length(ns3.levels)){
  for(st in 1:length(st.levels)){
    for(ns4 in 1:length(ns4.levels)){
      test.data=zdata
      test.data$Nonsocial.Room.3=relevel(test.data$Nonsocial.Room.3, ref=ns3.levels[ns3])
      test.data$Spaghetti.Tube=relevel(test.data$Spaghetti.Tube, ref=st.levels[st])
      test.data$Nonsocial.Room.4=relevel(test.data$Nonsocial.Room.4, ref=ns4.levels[ns4])
      test.data$ns3.code=as.numeric(test.data$Nonsocial.Room.3==levels(test.data$Nonsocial.Room.3)[2])
      test.data$st.code=as.numeric(test.data$Spaghetti.Tube==levels(test.data$Spaghetti.Tube)[2])
      test.data$ns4.code=as.numeric(test.data$Nonsocial.Room.4==levels(test.data$Nonsocial.Room.4)[2])
      
      
      res=glmer(tool_trans~Nonsocial.Room.3 + Spaghetti.Tube+Nonsocial.Room.4  + 
                  Species+ z.Session+
                  (1|Room.3)+(1|Room.4)+ (1|Dyad)+
                  
                  (0+z.Session|Room.3)+ 
                  (0+z.Session|Room.4)+
                  (0+z.Session|Dyad)+
                  
                  (0+ns3.code|Room.3)+ 
                  (0+ns3.code|Room.4)+
                  (0+ns3.code|Dyad)+
                  
                  (0+st.code|Room.3)+ 
                  (0+st.code|Room.4)+
                  (0+st.code|Dyad)+
                  
                  (0+ns4.code|Room.3)+ 
                  (0+ns4.code|Room.4)+
                  (0+ns4.code|Dyad),
                
                data=test.data, family=binomial
                ,control=contr
      )
      
      all.ll=rbind(all.ll, data.frame(ns3.level=ns3.levels[ns3], st.level=st.levels[st], ns4.level=ns4.levels[ns4], as.vector(logLik(res))))
    }
  }
}


###centering


ns3.levels=levels(test.data$Nonsocial.Room.3)
st.levels=levels(test.data$Spaghetti.Tube)
ns4.levels=levels(test.data$Nonsocial.Room.4)
all.ll.center=c()
for(ns3 in 1:length(ns3.levels)){
  for(st in 1:length(st.levels)){
    for(ns4 in 1:length(ns4.levels)){
      test.data=zdata
      test.data$Nonsocial.Room.3=relevel(test.data$Nonsocial.Room.3, ref=ns3.levels[ns3])
      test.data$Spaghetti.Tube=relevel(test.data$Spaghetti.Tube, ref=st.levels[st])
      test.data$Nonsocial.Room.4=relevel(test.data$Nonsocial.Room.4, ref=ns4.levels[ns4])
      test.data$ns3.code=as.numeric(test.data$Nonsocial.Room.3==levels(test.data$Nonsocial.Room.3)[2])
      test.data$st.code=as.numeric(test.data$Spaghetti.Tube==levels(test.data$Spaghetti.Tube)[2])
      test.data$ns4.code=as.numeric(test.data$Nonsocial.Room.4==levels(test.data$Nonsocial.Room.4)[2])
      
      
      
      test.data$ns3.code=test.data$ns3.code-mean(test.data$ns3.code)
      test.data$st.code=test.data$st.code-mean(test.data$st.code)
      test.data$ns4.code=test.data$ns4.code-mean(test.data$ns4.code)
      
      res=glmer(tool_trans~Nonsocial.Room.3 + Spaghetti.Tube+Nonsocial.Room.4  + 
                  Species+ z.Session+
                  (1|Room.3)+(1|Room.4)+ (1|Dyad)+
                  
                  (0+z.Session|Room.3)+ 
                  (0+z.Session|Room.4)+
                  (0+z.Session|Dyad)+
                  
                  (0+ns3.code|Room.3)+ 
                  (0+ns3.code|Room.4)+
                  (0+ns3.code|Dyad)+
                  
                  (0+st.code|Room.3)+ 
                  (0+st.code|Room.4)+
                  (0+st.code|Dyad)+
                  
                  (0+ns4.code|Room.3)+ 
                  (0+ns4.code|Room.4)+
                  (0+ns4.code|Dyad),
                
                data=test.data, family=binomial
                ,control=contr
      )
      
      all.ll.center=rbind(all.ll.center, data.frame(ns3.level=ns3.levels[ns3], st.level=st.levels[st], ns4.level=ns4.levels[ns4], as.vector(logLik(res))))
    }
  }
}

####Assumptions

source("X:/Statistics/R scripts/Roger/Diagnostic_fcns.r")
overdisp.test(red3)


source("W:/Statistics/R scripts/Roger/glmm_stability.r")
m.stab=glmm.model.stab(model.res=red3, contr=contr)
round(m.stab$summary[,-1],3)

#visualising the variance of each variable
m.stab.plot(m.stab$summary[,-1])




#### Presence, only main effects______________________________________________________________________________

red10=glmer(tool_trans~Nonsocial.Room.3 + Spaghetti.Tube + Nonsocial.Room.4 + Presence + Species+ z.Session +
              (1|Room.3)+(1|Room.4)+ (1|Dyad)+
              
              (0+z.Session|Room.3)+ 
              (0+z.Session|Room.4)+
              (0+z.Session|Dyad)+
              
              (0+Nonsocial.Room.3.Low|Room.3)+ 
              (0+Nonsocial.Room.3.Low|Room.4)+
              (0+Nonsocial.Room.3.Low|Dyad)+
              
              (0+Spaghetti.Tube.Nothing|Room.3)+ 
              (0+Spaghetti.Tube.Nothing|Room.4)+
              (0+Spaghetti.Tube.Nothing|Dyad)+
              
               (0+Presence.yes|Room.3)+ 
               (0+Presence.yes|Room.4)+
               (0+Presence.yes|Dyad)+
              
              
              (0+Nonsocial.Room.4.Low|Room.3)+ 
              (0+Nonsocial.Room.4.Low|Room.4)+
              (0+Nonsocial.Room.4.Low|Dyad),
            
            data=zdata, family=binomial
            ,control=contr
)


null10=glmer(tool_trans~1 +
               (1|Room.3)+(1|Room.4)+ (1|Dyad)+
               
               (0+z.Session|Room.3)+ 
               (0+z.Session|Room.4)+
               (0+z.Session|Dyad)+
               
               (0+Nonsocial.Room.3.Low|Room.3)+ 
               (0+Nonsocial.Room.3.Low|Room.4)+
               (0+Nonsocial.Room.3.Low|Dyad)+
               
               (0+Spaghetti.Tube.Nothing|Room.3)+ 
               (0+Spaghetti.Tube.Nothing|Room.4)+
               (0+Spaghetti.Tube.Nothing|Dyad)+
               
               (0+Presence.yes|Room.3)+ 
               (0+Presence.yes|Room.4)+
               (0+Presence.yes|Dyad)+
               
               
               (0+Nonsocial.Room.4.Low|Room.3)+ 
               (0+Nonsocial.Room.4.Low|Room.4)+
               (0+Nonsocial.Room.4.Low|Dyad),
             
             data=zdata, family=binomial
             ,control=contr
)

summary(red10)
anova(null10, red10, test="Chisq")




drop1(red10, test="Chisq", control=contr) 



### Interaktion Species/ Begging und Attention_____________________________________________________
      
begdata=subset(zdata, BeggingGestures!="?")
z.Session=as.vector(scale(begdata$Session))

red32=glmer(tool_trans~Nonsocial.Room.3 + Spaghetti.Tube + Nonsocial.Room.4 + (BeggingGestures*Species) +(AttgetObeg*Species)+ z.Session+ 
                          (1|Room.3)+(1|Room.4)+ (1|Dyad)+
                          
                          (0+z.Session|Room.3)+ 
                          (0+z.Session|Room.4)+
                          (0+z.Session|Dyad)+
                          
                          (0+Nonsocial.Room.3.Low|Room.3)+ 
                          (0+Nonsocial.Room.3.Low|Room.4)+
                          (0+Nonsocial.Room.3.Low|Dyad)+
                          
                          (0+Spaghetti.Tube.Nothing|Room.3)+ 
                          (0+Spaghetti.Tube.Nothing|Room.4)+
                          (0+Spaghetti.Tube.Nothing|Dyad)+
                          
                          (0+Nonsocial.Room.4.Low|Room.3)+ 
                          (0+Nonsocial.Room.4.Low|Room.4)+
                          (0+Nonsocial.Room.4.Low|Dyad),
                        
                        data=begdata, family=binomial
                        ,control=contr
            )
            
            
            null32=glmer(tool_trans~1 +
                           (1|Room.3)+(1|Room.4)+ (1|Dyad)+
                           
                           
                           (0+z.Session|Room.3)+ 
                           (0+z.Session|Room.4)+
                           (0+z.Session|Dyad)+
                           
                           (0+Nonsocial.Room.3.Low|Room.3)+ 
                           (0+Nonsocial.Room.3.Low|Room.4)+
                           (0+Nonsocial.Room.3.Low|Dyad)+
                           
                           (0+Spaghetti.Tube.Nothing|Room.3)+ 
                           (0+Spaghetti.Tube.Nothing|Room.4)+
                           (0+Spaghetti.Tube.Nothing|Dyad)+
                           
                           
                           (0+Nonsocial.Room.4.Low|Room.3)+ 
                           (0+Nonsocial.Room.4.Low|Room.4)+
                           (0+Nonsocial.Room.4.Low|Dyad),
                         
                         data=begdata, family=binomial
                         ,control=contr
            )
            
            
            summary(red32)
            
            anova(null32, red32, test="Chisq")
            
            drop1(red32, test="Chisq", control=contr) 
            
            
            
###### PostHoc : Chimpanzee and Attention, Begging _________________________________________________________

chimpdata=subset(begdata, Species=="Chimpanzee")
            
z.Session=as.vector(scale(chimpdata$Session))

red15c=glmer(tool_trans~Nonsocial.Room.3 + Spaghetti.Tube + Nonsocial.Room.4 + AttgetObeg + BeggingGestures+ z.Session +
               (1|Room.3)+(1|Room.4)+ (1|Dyad)+
               
               (0+z.Session|Room.3)+ 
               (0+z.Session|Room.4)+
               (0+z.Session|Dyad)+
               
               (0+Nonsocial.Room.3.Low|Room.3)+ 
               (0+Nonsocial.Room.3.Low|Room.4)+
               (0+Nonsocial.Room.3.Low|Dyad)+
               
               (0+Spaghetti.Tube.Nothing|Room.3)+ 
               (0+Spaghetti.Tube.Nothing|Room.4)+
               (0+Spaghetti.Tube.Nothing|Dyad)+
               
               (0+BeggingGestures.no|Room.3)+ 
               (0+BeggingGestures.no|Room.4)+
               (0+BeggingGestures.no|Dyad)+
               
               (0+AttgetObeg.no|Room.3)+ 
               (0+AttgetObeg.no|Room.4)+
               (0+AttgetObeg.no|Dyad)+
               
               (0+Nonsocial.Room.4.Low|Room.3)+ 
               (0+Nonsocial.Room.4.Low|Room.4)+
               (0+Nonsocial.Room.4.Low|Dyad),
             
             data=chimpdata, family=binomial
             ,control=contr
)


null15c=glmer(tool_trans~1 +
                (1|Room.3)+(1|Room.4)+ (1|Dyad)+
                
                
                (0+z.Session|Room.3)+ 
                (0+z.Session|Room.4)+
                (0+z.Session|Dyad)+
                
                (0+Nonsocial.Room.3.Low|Room.3)+ 
                (0+Nonsocial.Room.3.Low|Room.4)+
                (0+Nonsocial.Room.3.Low|Dyad)+
                
                (0+Spaghetti.Tube.Nothing|Room.3)+ 
                (0+Spaghetti.Tube.Nothing|Room.4)+
                (0+Spaghetti.Tube.Nothing|Dyad)+
                
                (0+BeggingGestures.no|Room.3)+ 
                (0+BeggingGestures.no|Room.4)+
                (0+BeggingGestures.no|Dyad)+
                
                (0+AttgetObeg.no|Room.3)+ 
                (0+AttgetObeg.no|Room.4)+
                (0+AttgetObeg.no|Dyad)+
                
                (0+Nonsocial.Room.4.Low|Room.3)+ 
                (0+Nonsocial.Room.4.Low|Room.4)+
                (0+Nonsocial.Room.4.Low|Dyad),
              
              data=chimpdata, family=binomial
              ,control=contr
)


summary(red15c)

anova(null15c, red15c, test="Chisq")

drop1(red15c, test="Chisq", control=contr) 


### Only Orangutans Attention and Begging __________________________________

orangdata=subset(begdata, Species=="Orangutan")

z.Session=as.vector(scale(orangdata$Session))


red15o=glmer(tool_trans~Nonsocial.Room.3 + Spaghetti.Tube + Nonsocial.Room.4 + AttgetObeg + BeggingGestures+ z.Session +
               (1|Room.3)+(1|Room.4)+ (1|Dyad)+
               
               (0+z.Session|Room.3)+ 
               (0+z.Session|Room.4)+
               (0+z.Session|Dyad)+
               
               (0+Nonsocial.Room.3.Low|Room.3)+ 
               (0+Nonsocial.Room.3.Low|Room.4)+
               (0+Nonsocial.Room.3.Low|Dyad)+
               
               (0+Spaghetti.Tube.Nothing|Room.3)+ 
               (0+Spaghetti.Tube.Nothing|Room.4)+
               (0+Spaghetti.Tube.Nothing|Dyad)+
               
               (0+BeggingGestures.no|Room.3)+ 
               (0+BeggingGestures.no|Room.4)+
               (0+BeggingGestures.no|Dyad)+
               
               (0+AttgetObeg.no|Room.3)+ 
               (0+AttgetObeg.no|Room.4)+
               (0+AttgetObeg.no|Dyad)+
               
               (0+Nonsocial.Room.4.Low|Room.3)+ 
               (0+Nonsocial.Room.4.Low|Room.4)+
               (0+Nonsocial.Room.4.Low|Dyad),
             
             data=orangdata, family=binomial
             ,control=contr
)


null15o=glmer(tool_trans~1 +
                (1|Room.3)+(1|Room.4)+ (1|Dyad)+
                
                
                (0+z.Session|Room.3)+ 
                (0+z.Session|Room.4)+
                (0+z.Session|Dyad)+
                
                (0+Nonsocial.Room.3.Low|Room.3)+ 
                (0+Nonsocial.Room.3.Low|Room.4)+
                (0+Nonsocial.Room.3.Low|Dyad)+
                
                (0+Spaghetti.Tube.Nothing|Room.3)+ 
                (0+Spaghetti.Tube.Nothing|Room.4)+
                (0+Spaghetti.Tube.Nothing|Dyad)+
                
                (0+BeggingGestures.no|Room.3)+ 
                (0+BeggingGestures.no|Room.4)+
                (0+BeggingGestures.no|Dyad)+
                
                (0+AttgetObeg.no|Room.3)+ 
                (0+AttgetObeg.no|Room.4)+
                (0+AttgetObeg.no|Dyad)+
                
                (0+Nonsocial.Room.4.Low|Room.3)+ 
                (0+Nonsocial.Room.4.Low|Room.4)+
                (0+Nonsocial.Room.4.Low|Dyad),
              
              data=orangdata, family=binomial
              ,control=contr
)


summary(red15o)

anova(null15o, red15o, test="Chisq")

drop1(red15o, test="Chisq", control=contr) 





####### Tool Transfer after success in prior Trial___________________________________________

xnachpassdata <- xdata[xdata$Tool.nach.passing == "yes",]

xx.fe.re=fe.re.tab(fe.model="tool_trans ~ Nonsocial.Room.3 * Spaghetti.Tube *Nonsocial.Room.4*Presence*Tool.nach.success*AttgetObeg*BeggingGestures+Species +Session+Trial",
                   re="(1|Room.3)+(1|Room.4)+(1|Dyad)",
                   data=data.frame(xnachpassdata))

xx.fe.re$summary

qdata=xx.fe.re$data

z.Session=as.vector(scale(qdata$Session))


red31=glmer(tool_trans~Nonsocial.Room.3 + Spaghetti.Tube + Nonsocial.Room.4 + (Tool.nach.success*Species)+ z.Session +
              (1|Room.3)+(1|Room.4)+ (1|Dyad)+
              
              (0+z.Session|Room.3)+ 
              (0+z.Session|Room.4)+
              (0+z.Session|Dyad)+
              
              (0+Nonsocial.Room.3.Low|Room.3)+ 
              (0+Nonsocial.Room.3.Low|Room.4)+
              (0+Nonsocial.Room.3.Low|Dyad)+
              
              (0+Spaghetti.Tube.Nothing|Room.3)+ 
              (0+Spaghetti.Tube.Nothing|Room.4)+
              (0+Spaghetti.Tube.Nothing|Dyad)+
              
              
              (0+Nonsocial.Room.4.Low|Room.3)+ 
              (0+Nonsocial.Room.4.Low|Room.4)+
              (0+Nonsocial.Room.4.Low|Dyad),
            
          
            data=qdata, family=binomial
            ,control=contr
)


null31=glmer(tool_trans~1 +
               (1|Room.3)+(1|Room.4)+ (1|Dyad)+
               
               (0+z.Session|Room.3)+ 
               (0+z.Session|Room.4)+
               (0+z.Session|Dyad)+
               
               (0+Nonsocial.Room.3.Low|Room.3)+ 
               (0+Nonsocial.Room.3.Low|Room.4)+
               (0+Nonsocial.Room.3.Low|Dyad)+
               
               (0+Spaghetti.Tube.Nothing|Room.3)+ 
               (0+Spaghetti.Tube.Nothing|Room.4)+
               (0+Spaghetti.Tube.Nothing|Dyad)+
               
               
               (0+Nonsocial.Room.4.Low|Room.3)+ 
               (0+Nonsocial.Room.4.Low|Room.4)+
               (0+Nonsocial.Room.4.Low|Dyad),
             
     
             data=qdata, family=binomial
             ,control=contr
)

summary(red31)

anova(null31, red31, test="Chisq")

drop1(red31, test="Chisq", control=contr) 





##### Only chimpanzees_ Transfer after success______________________

chimpdata2=subset(qdata, Species=="Chimpanzee")

z.Session=as.vector(scale(chimpdata2$Session))

red16c=glmer(tool_trans~Nonsocial.Room.3 + Spaghetti.Tube + Nonsocial.Room.4 + Tool.nach.success+ z.Session +
               (1|Room.3)+(1|Room.4)+ (1|Dyad)+
               
               (0+z.Session|Room.3)+ 
               (0+z.Session|Room.4)+
               (0+z.Session|Dyad)+
               
               (0+Nonsocial.Room.3.Low|Room.3)+ 
               (0+Nonsocial.Room.3.Low|Room.4)+
               (0+Nonsocial.Room.3.Low|Dyad)+
               
               (0+Spaghetti.Tube.Nothing|Room.3)+ 
               (0+Spaghetti.Tube.Nothing|Room.4)+
               (0+Spaghetti.Tube.Nothing|Dyad)+
               
               (0+Tool.nach.success.no|Room.3)+ 
               (0+Tool.nach.success.no|Room.4)+
               (0+Tool.nach.success.no|Dyad)+
               
               (0+Nonsocial.Room.4.Low|Room.3)+ 
               (0+Nonsocial.Room.4.Low|Room.4)+
               (0+Nonsocial.Room.4.Low|Dyad),
             
             data=chimpdata2, family=binomial
             ,control=contr
)


null16c=glmer(tool_trans~1 +
                (1|Room.3)+(1|Room.4)+ (1|Dyad)+
                
                
                (0+z.Session|Room.3)+ 
                (0+z.Session|Room.4)+
                (0+z.Session|Dyad)+
                
                (0+Nonsocial.Room.3.Low|Room.3)+ 
                (0+Nonsocial.Room.3.Low|Room.4)+
                (0+Nonsocial.Room.3.Low|Dyad)+
                
                (0+Spaghetti.Tube.Nothing|Room.3)+ 
                (0+Spaghetti.Tube.Nothing|Room.4)+
                (0+Spaghetti.Tube.Nothing|Dyad)+
                
                (0+Tool.nach.success.no|Room.3)+ 
                (0+Tool.nach.success.no|Room.4)+
                (0+Tool.nach.success.no|Dyad)+
                
                (0+Nonsocial.Room.4.Low|Room.3)+ 
                (0+Nonsocial.Room.4.Low|Room.4)+
                (0+Nonsocial.Room.4.Low|Dyad),
              
              data=chimpdata2, family=binomial
              ,control=contr
)



summary(red16c)

anova(null16c, red16c, test="Chisq")

drop1(red16c, test="Chisq", control=contr) 



####Orangutan - Tool passing after success__________________________

orangdata2=subset(qdata, Species=="Orangutan")

z.Session=as.vector(scale(orangdata2$Session))



red16o=glmer(tool_trans~Nonsocial.Room.3 + Spaghetti.Tube + Nonsocial.Room.4 + Tool.nach.success+ z.Session +
               (1|Room.3)+(1|Room.4)+ (1|Dyad)+
               
               (0+z.Session|Room.3)+ 
               (0+z.Session|Room.4)+
               (0+z.Session|Dyad)+
               
               (0+Nonsocial.Room.3.Low|Room.3)+ 
               (0+Nonsocial.Room.3.Low|Room.4)+
               (0+Nonsocial.Room.3.Low|Dyad)+
               
               (0+Spaghetti.Tube.Nothing|Room.3)+ 
               (0+Spaghetti.Tube.Nothing|Room.4)+
               (0+Spaghetti.Tube.Nothing|Dyad)+
               
               (0+Tool.nach.success.no|Room.3)+ 
               (0+Tool.nach.success.no|Room.4)+
               (0+Tool.nach.success.no|Dyad)+
               
               (0+Nonsocial.Room.4.Low|Room.3)+ 
               (0+Nonsocial.Room.4.Low|Room.4)+
               (0+Nonsocial.Room.4.Low|Dyad),
             
             data=orangdata2, family=binomial
             ,control=contr
)


null16o=glmer(tool_trans~1 +
                (1|Room.3)+(1|Room.4)+ (1|Dyad)+
                
                
                (0+z.Session|Room.3)+ 
                (0+z.Session|Room.4)+
                (0+z.Session|Dyad)+
                
                (0+Nonsocial.Room.3.Low|Room.3)+ 
                (0+Nonsocial.Room.3.Low|Room.4)+
                (0+Nonsocial.Room.3.Low|Dyad)+
                
                (0+Spaghetti.Tube.Nothing|Room.3)+ 
                (0+Spaghetti.Tube.Nothing|Room.4)+
                (0+Spaghetti.Tube.Nothing|Dyad)+
                
                (0+Tool.nach.success.no|Room.3)+ 
                (0+Tool.nach.success.no|Room.4)+
                (0+Tool.nach.success.no|Dyad)+
                
                (0+Nonsocial.Room.4.Low|Room.3)+ 
                (0+Nonsocial.Room.4.Low|Room.4)+
                (0+Nonsocial.Room.4.Low|Dyad),
              
              data=orangdata2, family=binomial
              ,control=contr
)


summary(red16o)

anova(null16o, red16o, test="Chisq")

drop1(red16o, test="Chisq", control=contr)
