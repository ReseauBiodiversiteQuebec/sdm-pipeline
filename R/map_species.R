# SDM generic functions


#' @param buffer_type Type of buffer
#' @return Table of pseudoabsence points sampled within the selected distance
#' @return A RasterLayer object containing the final buffer around the
#' occurrence points
#' @import ggplot2
#' @import dplyr
#' @export map_species

map_species <- function(df, species,
                        shp_layer,
                        min_year = 1900,
                        max_year = 2022,
                        vernacular = FALSE) {
    df <- df %>%
        filter(scientific_name == species) %>%
        filter(year_obs >= min_year) %>%
        filter(year_obs <= max_year) %>%
        add_coord_to_df()

    title <- ifelse(vernacular == TRUE,
        paste0(
            df$vernacular_fr[1],
            " (", species, ")"
        ), species
    )
    subtitle <- ifelse(min_year == max_year,
        paste0(nrow(df), " observations (", min_year, ")"),
        paste0(nrow(df), " observations (", min(df$year_obs), "-", max(df$year_obs), ")")
    )
    map <- ggplot() +
        geom_polygon(
            data = shp_layer,
            aes(x = long, y = lat, group = group),
            show.legend = FALSE, fill = "grey"
        ) +
        geom_path(
            data = shp_layer,
            aes(x = long, y = lat, group = group),
            show.legend = FALSE,
            colour = "white"
        ) +
        geom_point(
            data = df, aes(x = X, y = Y), col = "#1B9E77", size = 5,
            alpha = 0.7
        ) +
        xlab("Longitude") +
        ylab("Latitude") +
        ggtitle(title, subtitle = subtitle) +
        theme_bw()
    map
}