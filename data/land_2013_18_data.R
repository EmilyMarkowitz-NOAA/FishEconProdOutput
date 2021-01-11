#' Modified Landings Data 2013-2018
#'
#' Modified and cleaned data from NOAA Fisheries Office of Science and Technology’s Fisheries Statistics Division’s Commercial Landings Query, Available at: https://foss.nmfs.noaa.gov/apexfoss/f?p=215:200:::::: for all coastal states combined with state and regional data.
#'
#' @docType data
#'
#' @usage data(land_2013_18)
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{Year}{four-digit year}
#'   \item{Pounds}{weight of fish caught, in pounds}
#'   \item{Dollars}{value of fish caught, in USD}
#'   \item{category}{category of organism. For our analysis, we aggregated landings and revenue data into two different fisheries: finfish (defined by all organisms in the infraphylum Gnathostomata) and shellfish (defined by all organisms in the phyla Arthropoda and Mollusca)}
#'   \item{Tsn}{Taxonomic Serial Number (TSN) as defined by the Integrated Taxonomic Information System  Distinguishing species fishery categories was done easily with the R package ‘taxize'}
#'   \item{State}{The state the fish was caught in, in full name}
#'   \item{Region}{The region the fish was caught in, in full name}
#'   \item{abbvreg}{The region the fish was caught in, abbrevated}
#' }
#'
#' @keywords datasets
#'
#' @source \href{https://foss.nmfs.noaa.gov/apexfoss/f?p=215:200::::::}{NOAA Fisheries FOSS}
#'
#' @examples
#' data(land2013_18)
"land_2013_18"
