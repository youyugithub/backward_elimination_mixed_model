# backward_elimination_mixed_model

## mixed model backward elimination based on p values

```
selected.names<-covariate.names
while(length(selected.names)>=2){
  model.selected<-glmer(as.formula(paste("outcome~",paste(selected.names,collapse="+"),"+(1|id)",collapse = "")),
                        family=binomial(),data=longitudinal.data.sub,
                        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
  pvalues<-summary(model.selected)$coefficient[-1,"Pr(>|z|)"]
  if(all(pvalues<=0.05))break
  remove.idx<-which.max(pvalues)
  selected.names<-setdiff(selected.names,selected.names[remove.idx])
  show(selected.names)
}
```

## mixed model backward elimination based on F test

```
selected.names<-covariate.names
show(selected.names)
while(length(selected.names)>=2){
  pvalues<-numeric(length(selected.names))
  for(ii in 1:length(selected.names)){
    remaining.names<-setdiff(selected.names,selected.names[ii])
    model.selected<-lmer(as.formula(paste("DA~",paste(selected.names,collapse="+"),"+(1|ID)",collapse = "")),
                         data=regression.data,REML=FALSE)
    model.reduced<-lmer(as.formula(paste("DA~",paste(remaining.names,collapse="+"),"+(1|ID)",collapse = "")),
                        data=regression.data,REML=FALSE)
    pvalues[ii]<-anova(model.selected,model.reduced)[2,"Pr(>Chisq)"]
  }
  remove.idx<-which.max(pvalues)
  show(maxpvalues)
  selected.names<-setdiff(selected.names,selected.names[remove.idx])
  show(selected.names)
}
```
