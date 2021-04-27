### Library install script

# This script installs all the necessary libraries so that you don't have to 
# install them one by one

package_list <- c('boot', 'rstudioapi','ggpubr','rcompanion','corrplot',
'mice','VIM','reshape','psych','binom','pwr','Rfast','doParallel','blockTools',
'caret','scales','lme4','lmerTest','nbpMatching','binaryLogic','mltools',
'data.table','parallel','ivreg')

install_fun <-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

install_fun(package_list)



