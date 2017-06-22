################################################################################
# anova_table: Print ANOVA table for categorial time * condition interation
# works for lme4 (lmer) and nlme (lme) objects
# when time is categorical
# Author: koen.vanbrabant@kuleuven.be
# date: 22/06/2017
################################################################################


anova_table = function(fit,package=c('lme4','nlme')){
    
    format_pval <- function(x){
        if (x < .001) return('< .001')
        else paste(round(x, 3))   
    }
    
    if(package=='lme4'){
        names(fit@frame)[3] = 'condition.cat'
        
        general.df = data.frame(matrix('',nrow(anova(fit)),3))
        
        rownames(general.df)[1] = ifelse(label(fit@frame$moment.cat)=='',
            rownames(anova(fit))[1],label(fit@frame$moment))
        
        rownames(general.df)[2] = ifelse(label(fit@frame$condition.cat)=='',
            rownames(anova(fit))[2],label(fit@frame$condition.cat))
        
        rownames(general.df)[3] = ifelse(label(fit@frame$moment)=='' | 
                label(fit@frame$condition.cat)=='',rownames(anova(fit))[3],
            paste(label(fit@frame$moment),'*',label(fit@frame$condition.cat)))
        
        colnames(general.df) = c('df','f.value','p.value')
        general.df[,1] = anova(fit)[,3]
        general.df[,2] = round(anova(fit)[,5],3)
        general.df[,3] = as.character(general.df[,3])
        
        
        for (nrow in 1:nrow(general.df)){
            general.df[nrow,3] = format_pval(anova(fit)[nrow,6])
        }
        return(general.df)

    }


    if(package=='nlme'){
    
        data = fit$data[,attr(fit$terms,"term.labels")[1:2]]
        
        names(data) = c('moment.cat','condition.cat')
        
        
        general.df = data.frame(matrix('',nrow(anova(fit)),3))
        
        rownames(general.df)[1] = ifelse(label(data$moment.cat)=='',
            rownames(anova(fit))[1],label(data$moment.cat))
        
        rownames(general.df)[2] = ifelse(label(data$condition.cat)=='',
            rownames(anova(fit))[2],label(data$condition.cat))
        
        rownames(general.df)[3] = ifelse(label(data$moment.cat)=='' | 
                label(data$condition.cat)=='',rownames(anova(fit))[3],
            paste(label(data$moment),'*',label(data$condition.cat)))
        
        colnames(general.df) = c('df','f.value','p.value')
        general.df[,1] = anova(fit)[,1]
        general.df[,2] = round(anova(fit)[,3],3)
        general.df[,3] = as.character(general.df[,3])
        
        for (nrow in 1:nrow(general.df)){
            general.df[nrow,3] = format_pval(anova(fit)[nrow,4])
        }
        return(general.df)
    }
    
}
    





