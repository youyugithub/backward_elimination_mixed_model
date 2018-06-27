# backward_elimination_mixed_model
mixed model backward elimination based on F test

```
selected.names<-covariate.names
show(selected.names)
while(length(selected.names)>=2){
  pvalues<-numeric(length(selected.names))
  for(ii in 1:length(selected.names)){
    remaining.names<-setdiff(selected.names,selected.names[ii])
    browser()
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
