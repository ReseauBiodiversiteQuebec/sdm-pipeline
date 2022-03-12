loadObsAtlas <- function(species, year_range = NULL){
  
  taxa.id <-
    dplyr::pull(get_taxa(scientific_name = species), id_taxa_obs)
  
  taxa.obs <-
    try({
      get_observations(id_taxa = taxa.id, year = year_range) %>%
        filter(
          variable %in% c(
            "occurrence",
            "presence",
            "abundance",
            "Number of initial captures"
          )
        )
    })
  if (!is.data.frame(taxa.obs) |
      (is.data.frame(taxa.obs) &&
       unique(taxa.obs$taxa_valid_scientific_name) != species)) {
    message(sprintf("Error with species %s", species))
    return(NULL)
    
  } else {
    coords <- data.frame(taxa.obs$geom %>% st_coordinates()) %>%
      rename(lon = X, lat = Y)
    
    taxa.obs <- bind_cols(taxa.obs,
                          coords) %>% rename(scientific_name = taxa_valid_scientific_name)
    return(taxa.obs)
  }
}  
