################################## load data function

library(readr)
#library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


world_population <- read_csv("https://raw.githubusercontent.com/adamclarke6/data/main/world_population.csv")

world_map <- ne_countries(scale = 50, returnclass = 'sf')


world_map$gu_a3[186] <- "ESH"
world_map$gu_a3[189] <- "SSD"


combined_proper <- merge(world_population, world_map[, c("gu_a3", "geometry")], by.x = "CCA3", by.y = "gu_a3")


names(combined_proper)[names(combined_proper) == "2022 Population"] <- "Population_2022"
names(combined_proper)[names(combined_proper) == "2020 Population"] <- "Population_2020"
names(combined_proper)[names(combined_proper) == "2015 Population"] <- "Population_2015"
names(combined_proper)[names(combined_proper) == "2010 Population"] <- "Population_2010"
names(combined_proper)[names(combined_proper) == "2000 Population"] <- "Population_2000"
names(combined_proper)[names(combined_proper) == "1990 Population"] <- "Population_1990"
names(combined_proper)[names(combined_proper) == "1980 Population"] <- "Population_1980"
names(combined_proper)[names(combined_proper) == "1970 Population"] <- "Population_1970"




################################################# fit function


fit_model_world <- function(continent = "World", year1, year2, type)
{
  if(continent == "World")
  {
    map <- combined_proper
    xlim <- c(-180,180)
    ylim <- c(-90, 90)

  }

  else if(continent == "Europe")
  {
    map <- combined_proper[combined_proper$Continent == "Europe",]

    xlim <- c(-25, 180)
    ylim <- c(30, 85)
  }

  else if(continent == "Africa")
  {
    map <- combined_proper[combined_proper$Continent == "Africa",]

    xlim <- c(-30, 60)
    ylim <- c(-40, 40)
  }

  else if(continent == "Asia")
  {
    map <- combined_proper[combined_proper$Continent == "Asia",]

    xlim <- c(20, 150)
    ylim <- c(-20, 60)
  }

  else if(continent == "Oceania")
  {
    map <- combined_proper[combined_proper$Continent == "Oceania",]

    xlim <- c(100, 180)
    ylim <- c(-60, 20)
  }

  else if(continent == "North America")
  {
    map <- combined_proper[combined_proper$Continent == "North America",]

    xlim <- c(-180,0)
    ylim <- c(0, 90)
  }

  else if(continent == "South America")
  {
    map <- combined_proper[combined_proper$Continent == "South America",]

    xlim <- c(-100,-30)
    ylim <- c(-60, 20)
  }

  else
  {
    print("not a continent")
  }

  x <- list("map" = map, "xlim" = xlim, "ylim" = ylim)
  return(x)
}

model <- fit_model("Asia") ## function just allows you to pick continent so far

map1 <- model$map
xlim <- model$xlim
ylim <- model$ylim





############################################### plot function




  ## library(tmap) is an option
  ## not in tidyverse

diff <- map1$Population_2022 - map1$Population_1970

col_pal <- colorRampPalette(c('red','blue'))

map1$Col <- rbPal(20)[as.numeric(cut(diff, breaks = 20))]

layout(matrix(1:2,ncol=2), widths = c(3,1))

plot(x = map1$geometry, col = map1$Col, main = "Title of the plot", xlim = xlim, ylim = ylim)

legend_image <- as.raster(matrix(rbPal(20), ncol=1))

legend_image <- legend_image[ nrow(legend_image):1, ]

plot(c(0,2),c(min(diff), max(diff)),type = 'n', axes = F,xlab = '', ylab = '', main = 'legend title')
## second c() ^,^^ gives bounds of possible legend values y axis

text(x = 1.5, y = seq(from = min(diff), to = max(diff), by = 100000000),
              labels = seq(from = min(diff), to = max(diff), by = 100000000))

rasterImage(legend_image, 0, min(diff), 1, max(diff))
##                           ^     ^


## rounding up and down for legend scale -> pass to y
## [rounding](https://datacornering.com/round-roundup-rounddown-trunc-in-r/)



usethis::git_sitrep()



library(devtools)
library(knitr)
library(pkgbuild)
library(roxygen2)
library(testthat)


has_devel()
check_build_tools()

devtools::build()

