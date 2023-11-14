#' Format the MCMC outputs into a dataframe with all values per retained iteration and chain.
#'
#' @param jags_output the mcmc.list object output by the run_model() function
#' @param data the preprocessed data list output by the preprocess_data() function
#' 
#' @examples
#' 
#' \donttest{
#' realistic_biotracer_data <- read.csv(system.file("extdata", "realistic_biotracer_data.csv",
#'                                                package = "EcoDiet"))
#' realistic_stomach_data <- read.csv(system.file("extdata", "realistic_stomach_data.csv",
#'                                              package = "EcoDiet"))
#'
#' data <- preprocess_data(biotracer_data = realistic_biotracer_data,
#'                         trophic_discrimination_factor = c(0.8, 3.4),
#'                         literature_configuration = FALSE,
#'                         stomach_data = realistic_stomach_data)
#'                         
#' write_model(literature_configuration = FALSE)
#' 
#' mcmc_output <- run_model("EcoDiet_model.txt", data, run_param="test")
#'                         
#' reshape_mcmc(mcmc_output, data)
#'              
#' }

reshape_mcmc <- function(jags_output, data){

  allit_allchains <- bind_rows(
    
    lapply(1:length(jags_output$samples),
           
           function(x){
             allit_chain <- as.data.frame(jags_output$samples[[x]])
             allit_chain$it <- seq(1, nrow(allit_chain))
             allit_chain$chain <- x
             allit_chain %>%
               pivot_longer(which(!colnames(allit_chain)%in%c("deviance","it","chain")),
                            names_to="var_code") %>%
               filter(grepl("eta|PI", variable)) %>%
               mutate(variable=ifelse(grepl("eta", var_code), "eta", "PI")) %>%
               mutate(varinter=gsub("eta|PI", "", gsub("\\[|\\]", "", var_code))) %>%
               mutate(prey=as.numeric(sub("\\,.*", "", varinter)),
                      pred=as.numeric(sub(".*\\,", "", varinter))) %>%
               group_by(chain, it, var_code) %>%
               mutate(preyname=colnames(data$o)[prey],
                      predname=colnames(data$o)[pred]) %>%
               ungroup() %>%
               dplyr::select(-varinter, -prey, -pred) -> allit_chain
             
             return(allit_chain)
             
           }
    )
  )
  
  return(allit_allchains)


}