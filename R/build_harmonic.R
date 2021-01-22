
#' Build harmonic
#' 
#' Comming soon.
#' 
#' @export


build_harmonic <- function(spectre,base_note){
  freq<-get_freq(baseNote)
  
  freq_max=spectre[[1]][length(spectre[[1]] )]
  
  hamonic_rank <- 1:(freq_max%/%freq)
  
  harmonic_db <- unlist( lapply(hamonic_rank,
                                 function(n) {
                                   max(spectre[spectre[[1]] <(n*freq+0.5*freq) & spectre[[1]]>(n*freq-0.5*freq),][[2]] )
                                 }),
                          use.names = F)
  
  10^((harmonic_db - harmonic_db[1])/20)
  
}