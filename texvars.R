library('broom')

initialize_texvar_cache = function(){    
    return(list())
}
update_texvar_cache = function(cache, var, value, digits=2){
    
    if (length(grep('[[:punct:]]', var)) > 0){
        stop('variable name cannot contain punctuation')
    }

    if (typeof(value) == 'double'){
        cache[[var]] = round(value, digits)
        return(cache)    
    } else if (typeof(value) %in% c('character', 'integer')){
        cache[[var]] = value
        return(cache)
    } else {
        stop('texvar system only acccepts ')
    }    
}

writeout_texvar_cache = function(cache, filename, appendDate=T){

    if (appendDate){
        filename = gsub('.tex', paste0('_',gsub(' ',
            '_',Sys.Date()),'.tex'), filename)
    }
    lines = sapply(names(cache), function(var_name){
        paste0("\\def\\",var_name,"{",cache[[var_name]],"}")})     
    
    fileConn = file(filename)    
    writeLines(lines, fileConn)    
    close(fileConn)
}

get_texvar_from_beta = function(lm, targetVar, tvc_varname, tvc) {
    if (class(summary(lm)) == 'brmssummary') {

        coefs = data.frame(fixef(lm))
        coefs$term = rownames(fixef(lm))
        tv = subset(coefs, coefs$term == targetVar)
        
        rstr = paste0("$\\beta$ = ", round(tv$Estimate[1], digits=3),
            ", 95\\% CI = ",round(tv$Q2.5[1], digits=3), " - ",
         round(tv$Q97.5[1], digits=3))
        tvc[[tvc_varname]] =  rstr

    } else if (class(summary(lm)) == "summary.merMod") {
        coefs = broom::tidy(lm, conf.int = TRUE)
        tv = subset(coefs, term == targetVar)
        rstr = paste0("$\\beta$ = ", round(tv$estimate[1], digits=3),
            ", 95\\% CI = ",round(tv$conf.low[1], digits=3), " - ",
         round(tv$conf.high[1], digits=3))
        tvc[[tvc_varname]] =  rstr
    } 
    
return(tvc)
}