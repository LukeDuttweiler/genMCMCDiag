mcmcDiag <- setClass("mcmcDiag", slots = c(diagnostics="list", transformedDraws="list",
                                           call = 'list'))


methods::setMethod('print',
                   signature(x = 'mcmcDiag'),
                   function(x){
                     #Extract method
                     m <- x@call$arguments$method

                     #Extract diagnostics
                     diags <- x@diagnostics

                     #Special handling for traceplot
                     if('traceplot' %in% names(diags)){
                       print(diags$traceplot)
                       diags <- diags[names(diags) != 'traceplot']
                     }

                     if(length(diags) !=0){
                       catStr <- paste0('----------------------------------------------------\n',
                                        'Generalized MCMC Diagnostics using ', m,
                                        ' Method \n----------------------------------------------------\n\n')

                       #Other diagnostics printed
                       for(d in names(diags)){
                         nm <- row.names(diags[[d]])[1]
                         row.names(diags[[d]]) <- NULL
                         strAdd <- paste0('|',nm, ': \n|---------------------------\n',
                                          paste0(knitr::kable(diags[[d]]), collapse = '\n'),
                                          '\n\n')
                         catStr <- paste0(catStr, strAdd)
                       }
                       cat(catStr)
                     }
                   })


methods::setMethod('show',
                   signature = 'mcmcDiag',
                   definition = function(object){
                     print(object)
                   })
