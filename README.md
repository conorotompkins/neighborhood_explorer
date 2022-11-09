# Neighborhood Explorer

An app for analyzing data at the neighborhood level

### Goals

1.  Make it easier to easily compare statistics about census tracts

-   Problem: Census tracts are given numeric IDs that are not memorable
-   Solution: Put the Census data on a map in an easily consumable way

2.  Create a tool that helps people learn about their communities
3.  Learn more about R + Shiny

### Structure

-   Variables are organized into "topics"

    -   Race/ethnicity

    -   Income

    -   Commute mode

    -   Housing

-   Code is highly modular. The same functions can be applied to different "topics".

### Inspirations

-   <https://www.policymap.com/newmaps#/>
-   <https://www.energy.gov/eere/slsc/maps/lead-tool>

### Resources

-   Data:

    -   [US Census Bureau](https://www.census.gov/)

    -   [Historical Housing Unit and Urbanization Database 2010](https://osf.io/fzv5e/?view_only=)

-   [{tidycensus}](https://walker-data.com/tidycensus/)

-   [{leaflet}](https://rstudio.github.io/leaflet/)

### Next steps

-   Add more topics

    -   Variables from Census

    -   Other data sources

-   Add other areas of the US

    -   Metro areas (MSAs)

-   Improve UI

    -   Show summarizations of the tracts the user has selected

-   Incorporate variables that use other geographies (for example, data only available at the zip code level)
