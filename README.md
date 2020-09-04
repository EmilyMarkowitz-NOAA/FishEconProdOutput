**PRELIMINARY**: Measuring Output for U.S. Commercial Fisheries From Theory to Practice
=======================================================================================

**Code is still in development**
--------------------------------

**Emily Markowitz**<sup>1</sup> (Emily.Markowitz AT noaa.gov)

**Sun Ling Wang**<sup>2</sup> (Sun-Ling.Wang AT noaa.gov)

<sup>1</sup>Contractor, ECS Federal in support of NOAA Fisheries Office
of Science and Technology Economics & Social Analysis Division; as of
Sept. 28, 2020: Alaska Fisheries Science Center, National Marine
Fisheries Service, National Oceanic and Atmospheric Administration,
Seattle, WA 98195

<sup>2</sup>On detail with the NOAA Fisheries Office of Science and
Technology Economics & Social Analysis Division

> \*The views expressed are those of the author and should not be
> attributed to the NOAA, ECS or ERS

**GitHub:** <https://github.com/emilyhmarkowitz/FishEconProdOutput>

NOAA README
===========

> This repository is a scientific product and is not official
> communication of the National Oceanic and Atmospheric Administration,
> or the United States Department of Commerce. All NOAA GitHub project
> code is provided on an ‘as is’ basis and the user assumes
> responsibility for its use. Any claims against the Department of
> Commerce or Department of Commerce bureaus stemming from the use of
> this GitHub project will be governed by all applicable Federal law.
> Any reference to specific commercial products, processes, or services
> by service mark, trademark, manufacturer, or otherwise, does not
> constitute or imply their endorsement, recommendation or favoring by
> the Department of Commerce. The Department of Commerce seal and logo,
> or the seal and logo of a DOC bureau, shall not be used in any manner
> to imply endorsement of any commercial product or activity by DOC or
> the United States Government.

Download this package
=====================

    library(devtools)
    devtools::install_github("emilyhmarkowitz/FishEconProdOutput", dependencies = TRUE)
    library(FishEconProdOutput)

Study Purpose
=============

-   Develop alternative approaches to measure national and regional
    fishery outputs for productivity measurements.

-   Evaluate the impacts of missing data and other issues on output
    estimates.

Documentation
=============

For specifics about how the Quantitative and Price Methods are derived,
please read this
[Documentation](https://github.com/emilyhmarkowitz/FishEconProdOutput/blob/master/ProductivityIndex_DocumentationSummary.pdf).

Data requirements and source
============================

The Tornqvist quantity index requires data on quantity and revenue
shares. We employ landings quantity (pounds) and landings value ($USD)
data by year, state, and species.

-   Data source: [Fisheries One Stop Shop downloaded August 13
    2020](https://foss.nmfs.noaa.gov/apexfoss/f?p=215:200::::::)

-   More information about the data: [Commericial Fisheries Landings
    Data](https://www.fisheries.noaa.gov/national/sustainable-fisheries/commercial-fisheries-landings)

Data from FOSS looks like this:

<table>
<colgroup>
<col style="width: 2%" />
<col style="width: 2%" />
<col style="width: 2%" />
<col style="width: 1%" />
<col style="width: 3%" />
<col style="width: 3%" />
<col style="width: 3%" />
<col style="width: 3%" />
<col style="width: 8%" />
<col style="width: 5%" />
<col style="width: 4%" />
<col style="width: 4%" />
<col style="width: 4%" />
<col style="width: 3%" />
<col style="width: 4%" />
<col style="width: 4%" />
<col style="width: 2%" />
<col style="width: 3%" />
<col style="width: 8%" />
<col style="width: 1%" />
<col style="width: 2%" />
<col style="width: 2%" />
<col style="width: 3%" />
<col style="width: 1%" />
<col style="width: 2%" />
<col style="width: 3%" />
<col style="width: 4%" />
<col style="width: 2%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: right;">X</th>
<th style="text-align: left;">SS_CODE</th>
<th style="text-align: left;">TSN</th>
<th style="text-align: right;">H_OLD_ITIS</th>
<th style="text-align: right;">H_NEW_ITIS</th>
<th style="text-align: right;">HM_OLD_ITIS</th>
<th style="text-align: left;">HM_NEW_ITIS</th>
<th style="text-align: left;">TS_AFS_NAME</th>
<th style="text-align: left;">TS_SCIENTIFIC_NAME</th>
<th style="text-align: left;">CONF_SPECIES</th>
<th style="text-align: left;">AFS_NAME_CONF</th>
<th style="text-align: left;">REGION_NAME</th>
<th style="text-align: right;">REGION_ID</th>
<th style="text-align: left;">STATE_NAME</th>
<th style="text-align: left;">S_STATE_NAME</th>
<th style="text-align: right;">STATE_ID</th>
<th style="text-align: left;">S_STATE_ID</th>
<th style="text-align: left;">AFS_NAME</th>
<th style="text-align: right;">YEAR</th>
<th style="text-align: right;">POUNDS</th>
<th style="text-align: right;">DOLLARS</th>
<th style="text-align: right;">TOT_COUNT</th>
<th style="text-align: left;">CONF</th>
<th style="text-align: left;">SOURCE</th>
<th style="text-align: left;">COLLECTION</th>
<th style="text-align: left;">SUMMARY_TYPE</th>
<th style="text-align: left;">MARKER</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">216930</td>
<td style="text-align: right;">216930</td>
<td style="text-align: left;">081495</td>
<td style="text-align: left;">81495</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">CLAMS, QUAHOG, MERCENARIA **</td>
<td style="text-align: left;">MERCENARIA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">South Atlantic</td>
<td style="text-align: right;">4</td>
<td style="text-align: left;">GEORGIA</td>
<td style="text-align: left;">GEORGIA</td>
<td style="text-align: right;">63</td>
<td style="text-align: left;">13</td>
<td style="text-align: left;">CLAMS, QUAHOG, MERCENARIA</td>
<td style="text-align: right;">2016</td>
<td style="text-align: right;">348263.0</td>
<td style="text-align: right;">2402345</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">ACCSP</td>
<td style="text-align: left;">Commercial</td>
<td style="text-align: left;">L_STATE_YEAR</td>
<td style="text-align: left;">-</td>
</tr>
<tr class="even">
<td style="text-align: left;">216931</td>
<td style="text-align: right;">216931</td>
<td style="text-align: left;">081495</td>
<td style="text-align: left;">81495</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">CLAMS, QUAHOG, MERCENARIA **</td>
<td style="text-align: left;">MERCENARIA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">South Atlantic</td>
<td style="text-align: right;">4</td>
<td style="text-align: left;">GEORGIA</td>
<td style="text-align: left;">GEORGIA</td>
<td style="text-align: right;">63</td>
<td style="text-align: left;">13</td>
<td style="text-align: left;">CLAMS, QUAHOG, MERCENARIA</td>
<td style="text-align: right;">2017</td>
<td style="text-align: right;">354265.0</td>
<td style="text-align: right;">2262468</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">ACCSP</td>
<td style="text-align: left;">Commercial</td>
<td style="text-align: left;">L_STATE_YEAR</td>
<td style="text-align: left;">-</td>
</tr>
<tr class="odd">
<td style="text-align: left;">216932</td>
<td style="text-align: right;">216932</td>
<td style="text-align: left;">081495</td>
<td style="text-align: left;">81495</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">CLAMS, QUAHOG, MERCENARIA **</td>
<td style="text-align: left;">MERCENARIA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">South Atlantic</td>
<td style="text-align: right;">4</td>
<td style="text-align: left;">GEORGIA</td>
<td style="text-align: left;">GEORGIA</td>
<td style="text-align: right;">63</td>
<td style="text-align: left;">13</td>
<td style="text-align: left;">CLAMS, QUAHOG, MERCENARIA</td>
<td style="text-align: right;">2018</td>
<td style="text-align: right;">337612.0</td>
<td style="text-align: right;">2246769</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">ACCSP</td>
<td style="text-align: left;">Commercial</td>
<td style="text-align: left;">L_STATE_YEAR</td>
<td style="text-align: left;">-</td>
</tr>
<tr class="even">
<td style="text-align: left;">216933</td>
<td style="text-align: right;">216933</td>
<td style="text-align: left;">081495</td>
<td style="text-align: left;">81495</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">CLAMS, QUAHOG, MERCENARIA **</td>
<td style="text-align: left;">MERCENARIA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">South Atlantic</td>
<td style="text-align: right;">4</td>
<td style="text-align: left;">GEORGIA</td>
<td style="text-align: left;">GEORGIA</td>
<td style="text-align: right;">63</td>
<td style="text-align: left;">13</td>
<td style="text-align: left;">CLAMS, QUAHOG, MERCENARIA</td>
<td style="text-align: right;">2019</td>
<td style="text-align: right;">210352.0</td>
<td style="text-align: right;">1844863</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">ACCSP</td>
<td style="text-align: left;">Commercial</td>
<td style="text-align: left;">L_STATE_YEAR</td>
<td style="text-align: left;">-</td>
</tr>
<tr class="odd">
<td style="text-align: left;">216934</td>
<td style="text-align: right;">216934</td>
<td style="text-align: left;">081495</td>
<td style="text-align: left;">81495</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">CLAMS, QUAHOG, MERCENARIA **</td>
<td style="text-align: left;">MERCENARIA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">South Atlantic</td>
<td style="text-align: right;">4</td>
<td style="text-align: left;">NORTH CAROLINA</td>
<td style="text-align: left;">NORTH CAROLINA</td>
<td style="text-align: right;">26</td>
<td style="text-align: left;">37</td>
<td style="text-align: left;">CLAMS, QUAHOG, MERCENARIA</td>
<td style="text-align: right;">2007</td>
<td style="text-align: right;">425342.0</td>
<td style="text-align: right;">2674927</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">ACCSP</td>
<td style="text-align: left;">Commercial</td>
<td style="text-align: left;">L_STATE_YEAR</td>
<td style="text-align: left;">-</td>
</tr>
<tr class="even">
<td style="text-align: left;">216935</td>
<td style="text-align: right;">216935</td>
<td style="text-align: left;">081495</td>
<td style="text-align: left;">81495</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">CLAMS, QUAHOG, MERCENARIA **</td>
<td style="text-align: left;">MERCENARIA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">South Atlantic</td>
<td style="text-align: right;">4</td>
<td style="text-align: left;">NORTH CAROLINA</td>
<td style="text-align: left;">NORTH CAROLINA</td>
<td style="text-align: right;">26</td>
<td style="text-align: left;">37</td>
<td style="text-align: left;">CLAMS, QUAHOG, MERCENARIA</td>
<td style="text-align: right;">2008</td>
<td style="text-align: right;">382056.1</td>
<td style="text-align: right;">2385857</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">ACCSP</td>
<td style="text-align: left;">Commercial</td>
<td style="text-align: left;">L_STATE_YEAR</td>
<td style="text-align: left;">-</td>
</tr>
</tbody>
</table>

Then I append state information:

    statereg<-read.csv(file = paste0(dir.data, "statereg.csv"))

    head(statereg) %>%
        knitr::kable(row.names = T, booktabs = T)

<table>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: right;">X</th>
<th style="text-align: left;">State</th>
<th style="text-align: left;">State1</th>
<th style="text-align: right;">fips</th>
<th style="text-align: left;">Region</th>
<th style="text-align: left;">abbvst</th>
<th style="text-align: left;">abbvreg</th>
<th style="text-align: right;">xstate</th>
<th style="text-align: right;">xreg</th>
<th style="text-align: right;">State.no</th>
<th style="text-align: right;">Region.no</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: right;">1</td>
<td style="text-align: left;">Alabama</td>
<td style="text-align: left;">Alabama</td>
<td style="text-align: right;">1</td>
<td style="text-align: left;">Gulf of Mexico</td>
<td style="text-align: left;">AL</td>
<td style="text-align: left;">GOM</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">7</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">5</td>
</tr>
<tr class="even">
<td style="text-align: left;">2</td>
<td style="text-align: right;">2</td>
<td style="text-align: left;">Alaska</td>
<td style="text-align: left;">Alaska</td>
<td style="text-align: right;">2</td>
<td style="text-align: left;">North Pacific</td>
<td style="text-align: left;">AK</td>
<td style="text-align: left;">NP</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">2</td>
<td style="text-align: right;">7</td>
</tr>
<tr class="odd">
<td style="text-align: left;">3</td>
<td style="text-align: right;">3</td>
<td style="text-align: left;">California</td>
<td style="text-align: left;">California</td>
<td style="text-align: right;">6</td>
<td style="text-align: left;">Pacific</td>
<td style="text-align: left;">CA</td>
<td style="text-align: left;">Pac</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">2</td>
<td style="text-align: right;">5</td>
<td style="text-align: right;">6</td>
</tr>
<tr class="even">
<td style="text-align: left;">4</td>
<td style="text-align: right;">4</td>
<td style="text-align: left;">Connecticut</td>
<td style="text-align: left;">Connecticut</td>
<td style="text-align: right;">9</td>
<td style="text-align: left;">New England</td>
<td style="text-align: left;">CT</td>
<td style="text-align: left;">NE</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">4</td>
<td style="text-align: right;">7</td>
<td style="text-align: right;">1</td>
</tr>
<tr class="odd">
<td style="text-align: left;">5</td>
<td style="text-align: right;">5</td>
<td style="text-align: left;">Delaware</td>
<td style="text-align: left;">Delaware</td>
<td style="text-align: right;">10</td>
<td style="text-align: left;">Mid-Atlantic</td>
<td style="text-align: left;">DE</td>
<td style="text-align: left;">MA</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">5</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">2</td>
</tr>
<tr class="even">
<td style="text-align: left;">6</td>
<td style="text-align: right;">6</td>
<td style="text-align: left;">East Florida</td>
<td style="text-align: left;">Florida</td>
<td style="text-align: right;">12</td>
<td style="text-align: left;">South Atlantic</td>
<td style="text-align: left;">EFL</td>
<td style="text-align: left;">SA</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">4</td>
</tr>
</tbody>
</table>

Then I change some column names of the data:

    #Edit Landings Data
    landings.data<-landings.data[,c("TSN", "H_NEW_ITIS", "TS_AFS_NAME", "TS_SCIENTIFIC_NAME", "YEAR", 
                                    "POUNDS", "DOLLARS", "S_STATE_ID", "COLLECTION")]

    names(landings.data)[names(landings.data) %in% "S_STATE_ID"]<-"State.no"
    names(landings.data)[names(landings.data) %in% "TSN"]<-"Tsn"
    landings.data$TSN[is.na(landings.data$H_NEW_ITIS)]<-landings.data$H_NEW_ITIS[is.na(landings.data$H_NEW_ITIS)]
    landings.data$H_NEW_ITIS<-NULL
    names(landings.data)[names(landings.data) %in% "TS_AFS_NAME"]<-"AFS.Name"
    landings.data$AFS_NAME1<-landings.data$AFS.Name
    landings.data$AFS_NAME1<-gsub(pattern = " ", replacement = ".", x = landings.data$AFS_NAME1)
    landings.data$AFS_NAME1<-gsub(pattern = ",", replacement = ".", x = landings.data$AFS_NAME1)
    landings.data$AFS_NAME1<-gsub(pattern = "/", replacement = ".", x = landings.data$AFS_NAME1)
    landings.data$AFS_NAME1<-gsub(pattern = "\\(", replacement = ".", x = landings.data$AFS_NAME1)
    landings.data$AFS_NAME1<-gsub(pattern = "\\)", replacement = ".", x = landings.data$AFS_NAME1)
    landings.data$AFS_NAME1<-gsub(pattern = "&", replacement = ".", x = landings.data$AFS_NAME1)
    landings.data$AFS_NAME1<-gsub(pattern = "\\.\\.", replacement = ".", x = landings.data$AFS_NAME1)
    landings.data$AFS_NAME1<-gsub(pattern = "\\.\\.", replacement = ".", x = landings.data$AFS_NAME1)
    landings.data$AFS_NAME1<-gsub(pattern = "\\.", replacement = "_", x = landings.data$AFS_NAME1)
    names(landings.data)[names(landings.data) %in% "YEAR"]<-"Year"
    names(landings.data)[names(landings.data) %in% "POUNDS"]<-"Pounds"
    names(landings.data)[names(landings.data) %in% "DOLLARS"]<-"Dollars"
    names(landings.data)[names(landings.data) %in% "COLLECTION"]<-"Collection"
    landings.data<-merge(x = landings.data, y = statereg, by = "State.no")

    head(landings.data) %>%
        knitr::kable(row.names = T, booktabs = T)

<table style="width:100%;">
<colgroup>
<col style="width: 1%" />
<col style="width: 4%" />
<col style="width: 2%" />
<col style="width: 14%" />
<col style="width: 9%" />
<col style="width: 2%" />
<col style="width: 3%" />
<col style="width: 3%" />
<col style="width: 5%" />
<col style="width: 1%" />
<col style="width: 13%" />
<col style="width: 1%" />
<col style="width: 3%" />
<col style="width: 3%" />
<col style="width: 2%" />
<col style="width: 7%" />
<col style="width: 3%" />
<col style="width: 3%" />
<col style="width: 3%" />
<col style="width: 2%" />
<col style="width: 4%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: left;">State.no</th>
<th style="text-align: left;">Tsn</th>
<th style="text-align: left;">AFS.Name</th>
<th style="text-align: left;">TS_SCIENTIFIC_NAME</th>
<th style="text-align: right;">Year</th>
<th style="text-align: right;">Pounds</th>
<th style="text-align: right;">Dollars</th>
<th style="text-align: left;">Collection</th>
<th style="text-align: right;">TSN</th>
<th style="text-align: left;">AFS_NAME1</th>
<th style="text-align: right;">X</th>
<th style="text-align: left;">State</th>
<th style="text-align: left;">State1</th>
<th style="text-align: right;">fips</th>
<th style="text-align: left;">Region</th>
<th style="text-align: left;">abbvst</th>
<th style="text-align: left;">abbvreg</th>
<th style="text-align: right;">xstate</th>
<th style="text-align: right;">xreg</th>
<th style="text-align: right;">Region.no</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">13</td>
<td style="text-align: left;">81495</td>
<td style="text-align: left;">CLAMS, QUAHOG, MERCENARIA **</td>
<td style="text-align: left;">MERCENARIA</td>
<td style="text-align: right;">2016</td>
<td style="text-align: right;">348263</td>
<td style="text-align: right;">2402345</td>
<td style="text-align: left;">Commercial</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">CLAMS_QUAHOG_MERCENARIA_**</td>
<td style="text-align: right;">8</td>
<td style="text-align: left;">Georgia</td>
<td style="text-align: left;">Georgia</td>
<td style="text-align: right;">13</td>
<td style="text-align: left;">South Atlantic</td>
<td style="text-align: left;">GA</td>
<td style="text-align: left;">SA</td>
<td style="text-align: right;">2</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">4</td>
</tr>
<tr class="even">
<td style="text-align: left;">2</td>
<td style="text-align: left;">13</td>
<td style="text-align: left;">81495</td>
<td style="text-align: left;">CLAMS, QUAHOG, MERCENARIA **</td>
<td style="text-align: left;">MERCENARIA</td>
<td style="text-align: right;">2017</td>
<td style="text-align: right;">354265</td>
<td style="text-align: right;">2262468</td>
<td style="text-align: left;">Commercial</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">CLAMS_QUAHOG_MERCENARIA_**</td>
<td style="text-align: right;">8</td>
<td style="text-align: left;">Georgia</td>
<td style="text-align: left;">Georgia</td>
<td style="text-align: right;">13</td>
<td style="text-align: left;">South Atlantic</td>
<td style="text-align: left;">GA</td>
<td style="text-align: left;">SA</td>
<td style="text-align: right;">2</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">4</td>
</tr>
<tr class="odd">
<td style="text-align: left;">3</td>
<td style="text-align: left;">13</td>
<td style="text-align: left;">81495</td>
<td style="text-align: left;">CLAMS, QUAHOG, MERCENARIA **</td>
<td style="text-align: left;">MERCENARIA</td>
<td style="text-align: right;">2018</td>
<td style="text-align: right;">337612</td>
<td style="text-align: right;">2246769</td>
<td style="text-align: left;">Commercial</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">CLAMS_QUAHOG_MERCENARIA_**</td>
<td style="text-align: right;">8</td>
<td style="text-align: left;">Georgia</td>
<td style="text-align: left;">Georgia</td>
<td style="text-align: right;">13</td>
<td style="text-align: left;">South Atlantic</td>
<td style="text-align: left;">GA</td>
<td style="text-align: left;">SA</td>
<td style="text-align: right;">2</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">4</td>
</tr>
<tr class="even">
<td style="text-align: left;">4</td>
<td style="text-align: left;">13</td>
<td style="text-align: left;">81495</td>
<td style="text-align: left;">CLAMS, QUAHOG, MERCENARIA **</td>
<td style="text-align: left;">MERCENARIA</td>
<td style="text-align: right;">2019</td>
<td style="text-align: right;">210352</td>
<td style="text-align: right;">1844863</td>
<td style="text-align: left;">Commercial</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">CLAMS_QUAHOG_MERCENARIA_**</td>
<td style="text-align: right;">8</td>
<td style="text-align: left;">Georgia</td>
<td style="text-align: left;">Georgia</td>
<td style="text-align: right;">13</td>
<td style="text-align: left;">South Atlantic</td>
<td style="text-align: left;">GA</td>
<td style="text-align: left;">SA</td>
<td style="text-align: right;">2</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">4</td>
</tr>
</tbody>
</table>

And add categories for each organism to be binned into:

    ########***How to split species############
    ########******Taxonomic#####################
    categories<-list("Plantae" = 202422, 
                     "Chromista" = 590735,
                     "Fungi" = 555705,
                     "Bacteria" = 50,
                     "Protozoa" = 43780,
                     "Archaea" = 935939,
                     "Porifera" = 46861, 
                     "Cnidaria" = 48738, 
                     "Platyhelminthes" = 53963, 
                     "Nematoda" = 59490, 
                     "Annelida" = 64357, 
                     
                     "Arthropoda" = 82696, 
                     "Echinodermata" = 156857, 
                     "Mollusca" = 69458, 
                     # "Chordata"  = "phylum", 
                     "Urochordata" = 158853,
                     "Agnatha" = 914178,
                     "Chondrichthyes" = 159785,
                     "Sarcopterygii" = 161048, 
                     "Tetrapoda" = 914181, 
                     "Actinopterygii" = 161061)

    library(taxize)
    library(rlist)


    itis_reclassify<-function(tsn, categories, missing.name){
      
      # Find which codes are in which categories
      tsn0<-as.numeric(tsn)[!(is.na(tsn))]
      tsn.indata<-classification(sci_id = tsn0, db = 'itis')
      tsn.indata<-tsn.indata[!(names(tsn.indata) %in% 0)]
      valid0<- sciname<-category0<-bottomrank<-sppname<- TSN<-c() 
      
      TSN<-c()
      bottomrank<-c()
      category0<-c()
      sciname<-c()
      valid0<-c()
      
      
      for (i in 1:length(categories)) {

        a<-list.search(lapply(X = tsn.indata, '[', 3), categories[i][[1]] %in% . )
        
        if (length(a)!=0) {
          
          sppcode<-names(a)
          sppcode<-gsub(pattern = "[a-zA-Z]+", replacement = "", x = sppcode)
          sppcode<-gsub(pattern = "\\.", replacement = "", x = sppcode)
          
          for (ii in 1:length(sppcode)) {
            TSN<-c(TSN, sppcode[ii])
            
            bottomrank<-c(bottomrank, tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]]$rank[
              nrow(tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]])])
            
            category0<-c(category0, names(categories[i]))  
            
            sciname<-c(sciname, tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]]$name[
              nrow(tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]])])
            
            valid0<-c(valid0, 
                      ifelse(nrow(tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]])>1, 
                             "valid", "invalid"))
          }
        }
      }
      
      df.out<-data.frame(TSN = TSN, 
                         category = category0, 
                         valid = valid0, 
                         rank = bottomrank, 
                         sciname = sciname )
      
      return(list("df.out" = df.out, 
                  "tsn.indata" = tsn.indata))
    }

    spp.cat<-itis_reclassify(tsn = unique(landings.data$Tsn), 
                             categories, 
                             missing.name="Uncategorized")

    spp.cat$df.out$category.tax<-spp.cat$df.out$category
    spp.cat$df.out$category1.tax<-as.numeric(factor(spp.cat$df.out$category.tax))
    landings.data<-merge(x = landings.data, y = spp.cat$df.out, by.y = "TSN", by.x = "Tsn")

    head(landings.data) %>%
        knitr::kable(row.names = T, booktabs = T)

<table>
<colgroup>
<col style="width: 1%" />
<col style="width: 2%" />
<col style="width: 3%" />
<col style="width: 11%" />
<col style="width: 7%" />
<col style="width: 1%" />
<col style="width: 2%" />
<col style="width: 3%" />
<col style="width: 4%" />
<col style="width: 1%" />
<col style="width: 10%" />
<col style="width: 1%" />
<col style="width: 3%" />
<col style="width: 3%" />
<col style="width: 1%" />
<col style="width: 5%" />
<col style="width: 2%" />
<col style="width: 3%" />
<col style="width: 2%" />
<col style="width: 1%" />
<col style="width: 3%" />
<col style="width: 3%" />
<col style="width: 2%" />
<col style="width: 2%" />
<col style="width: 4%" />
<col style="width: 4%" />
<col style="width: 5%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: left;">Tsn</th>
<th style="text-align: left;">State.no</th>
<th style="text-align: left;">AFS.Name</th>
<th style="text-align: left;">TS_SCIENTIFIC_NAME</th>
<th style="text-align: right;">Year</th>
<th style="text-align: right;">Pounds</th>
<th style="text-align: right;">Dollars</th>
<th style="text-align: left;">Collection</th>
<th style="text-align: right;">TSN</th>
<th style="text-align: left;">AFS_NAME1</th>
<th style="text-align: right;">X</th>
<th style="text-align: left;">State</th>
<th style="text-align: left;">State1</th>
<th style="text-align: right;">fips</th>
<th style="text-align: left;">Region</th>
<th style="text-align: left;">abbvst</th>
<th style="text-align: left;">abbvreg</th>
<th style="text-align: right;">xstate</th>
<th style="text-align: right;">xreg</th>
<th style="text-align: right;">Region.no</th>
<th style="text-align: left;">category</th>
<th style="text-align: left;">valid</th>
<th style="text-align: left;">rank</th>
<th style="text-align: left;">sciname</th>
<th style="text-align: left;">category.tax</th>
<th style="text-align: right;">category1.tax</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">81495</td>
<td style="text-align: left;">13</td>
<td style="text-align: left;">CLAMS, QUAHOG, MERCENARIA **</td>
<td style="text-align: left;">MERCENARIA</td>
<td style="text-align: right;">2016</td>
<td style="text-align: right;">348263</td>
<td style="text-align: right;">2402345</td>
<td style="text-align: left;">Commercial</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">CLAMS_QUAHOG_MERCENARIA_**</td>
<td style="text-align: right;">8</td>
<td style="text-align: left;">Georgia</td>
<td style="text-align: left;">Georgia</td>
<td style="text-align: right;">13</td>
<td style="text-align: left;">South Atlantic</td>
<td style="text-align: left;">GA</td>
<td style="text-align: left;">SA</td>
<td style="text-align: right;">2</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">4</td>
<td style="text-align: left;">Mollusca</td>
<td style="text-align: left;">valid</td>
<td style="text-align: left;">genus</td>
<td style="text-align: left;">Mercenaria</td>
<td style="text-align: left;">Mollusca</td>
<td style="text-align: right;">1</td>
</tr>
<tr class="even">
<td style="text-align: left;">2</td>
<td style="text-align: left;">81495</td>
<td style="text-align: left;">13</td>
<td style="text-align: left;">CLAMS, QUAHOG, MERCENARIA **</td>
<td style="text-align: left;">MERCENARIA</td>
<td style="text-align: right;">2017</td>
<td style="text-align: right;">354265</td>
<td style="text-align: right;">2262468</td>
<td style="text-align: left;">Commercial</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">CLAMS_QUAHOG_MERCENARIA_**</td>
<td style="text-align: right;">8</td>
<td style="text-align: left;">Georgia</td>
<td style="text-align: left;">Georgia</td>
<td style="text-align: right;">13</td>
<td style="text-align: left;">South Atlantic</td>
<td style="text-align: left;">GA</td>
<td style="text-align: left;">SA</td>
<td style="text-align: right;">2</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">4</td>
<td style="text-align: left;">Mollusca</td>
<td style="text-align: left;">valid</td>
<td style="text-align: left;">genus</td>
<td style="text-align: left;">Mercenaria</td>
<td style="text-align: left;">Mollusca</td>
<td style="text-align: right;">1</td>
</tr>
<tr class="odd">
<td style="text-align: left;">3</td>
<td style="text-align: left;">81495</td>
<td style="text-align: left;">13</td>
<td style="text-align: left;">CLAMS, QUAHOG, MERCENARIA **</td>
<td style="text-align: left;">MERCENARIA</td>
<td style="text-align: right;">2018</td>
<td style="text-align: right;">337612</td>
<td style="text-align: right;">2246769</td>
<td style="text-align: left;">Commercial</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">CLAMS_QUAHOG_MERCENARIA_**</td>
<td style="text-align: right;">8</td>
<td style="text-align: left;">Georgia</td>
<td style="text-align: left;">Georgia</td>
<td style="text-align: right;">13</td>
<td style="text-align: left;">South Atlantic</td>
<td style="text-align: left;">GA</td>
<td style="text-align: left;">SA</td>
<td style="text-align: right;">2</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">4</td>
<td style="text-align: left;">Mollusca</td>
<td style="text-align: left;">valid</td>
<td style="text-align: left;">genus</td>
<td style="text-align: left;">Mercenaria</td>
<td style="text-align: left;">Mollusca</td>
<td style="text-align: right;">1</td>
</tr>
<tr class="even">
<td style="text-align: left;">4</td>
<td style="text-align: left;">81495</td>
<td style="text-align: left;">13</td>
<td style="text-align: left;">CLAMS, QUAHOG, MERCENARIA **</td>
<td style="text-align: left;">MERCENARIA</td>
<td style="text-align: right;">2019</td>
<td style="text-align: right;">210352</td>
<td style="text-align: right;">1844863</td>
<td style="text-align: left;">Commercial</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">CLAMS_QUAHOG_MERCENARIA_**</td>
<td style="text-align: right;">8</td>
<td style="text-align: left;">Georgia</td>
<td style="text-align: left;">Georgia</td>
<td style="text-align: right;">13</td>
<td style="text-align: left;">South Atlantic</td>
<td style="text-align: left;">GA</td>
<td style="text-align: left;">SA</td>
<td style="text-align: right;">2</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">4</td>
<td style="text-align: left;">Mollusca</td>
<td style="text-align: left;">valid</td>
<td style="text-align: left;">genus</td>
<td style="text-align: left;">Mercenaria</td>
<td style="text-align: left;">Mollusca</td>
<td style="text-align: right;">1</td>
</tr>
</tbody>
</table>

And then re-organized the data to work in these funcitons:

    category0<-"category.tax"
    temp00<-EditCommData(dat = landings.data, category0)
    temp<-temp00[[1]]

    head(temp) %>%
        knitr::kable(row.names = T, booktabs = T)

<table>
<colgroup>
<col style="width: 4%" />
<col style="width: 27%" />
<col style="width: 9%" />
<col style="width: 9%" />
<col style="width: 27%" />
<col style="width: 9%" />
<col style="width: 9%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: right;">Q1_1CLAMS.QUAHOG.MERCENARIA…</th>
<th style="text-align: right;">QE0_0Total</th>
<th style="text-align: right;">QE1_0Other</th>
<th style="text-align: right;">V1_1CLAMS.QUAHOG.MERCENARIA…</th>
<th style="text-align: right;">VE0_0Total</th>
<th style="text-align: right;">VE1_0Other</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">2016</td>
<td style="text-align: right;">2402345</td>
<td style="text-align: right;">2402345</td>
<td style="text-align: right;">2402345</td>
<td style="text-align: right;">348263</td>
<td style="text-align: right;">348263</td>
<td style="text-align: right;">348263</td>
</tr>
<tr class="even">
<td style="text-align: left;">2017</td>
<td style="text-align: right;">2262468</td>
<td style="text-align: right;">2262468</td>
<td style="text-align: right;">2262468</td>
<td style="text-align: right;">354265</td>
<td style="text-align: right;">354265</td>
<td style="text-align: right;">354265</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2018</td>
<td style="text-align: right;">2246769</td>
<td style="text-align: right;">2246769</td>
<td style="text-align: right;">2246769</td>
<td style="text-align: right;">337612</td>
<td style="text-align: right;">337612</td>
<td style="text-align: right;">337612</td>
</tr>
<tr class="even">
<td style="text-align: left;">2019</td>
<td style="text-align: right;">1844863</td>
<td style="text-align: right;">1844863</td>
<td style="text-align: right;">1844863</td>
<td style="text-align: right;">210352</td>
<td style="text-align: right;">210352</td>
<td style="text-align: right;">210352</td>
</tr>
</tbody>
</table>

File Organization
=================

Main fuctions of interest are:

-   PriceMethodOutput

-   PriceMethodOutput\_Category

-   PriceMethodOutput\_Plots

-   QuantityMethodOutput

-   QuantityMethodOutput\_Category

-   QuantityMethodOutput\_Plots

-   PriceIndex

-   PriceChange

-   EditCommData

-   lmCheck

-   numbers0

-   xunits

-   plotnlines

-   ReplaceFirst

-   ReplaceMid

Theoretical Framework: Törnqvist index
======================================

A Flexible Function and Superlative Quantity Index (Diewert 1976)

**Math Theory: General Total Factor Productivity (*T**F**P*) Equation**

The general form of the *T**F**P* can be measured as aggregate output
(*Y*) divided by real total inputs (*X*). Rates of TFP growth are
constructed using the Törnqvist index approach. The TFP growth over two
time periods is defined as:

$$ln(TFP\_t/TFP\_{t-1}) = \\sum\_{i=1}^n((\\frac{R\_{t,i} + R\_{t-1,i}}{2}) \* ln(\\frac{Y\_{t,i}}{Y\_{t-1,i}}))) - \\sum\_{j=1}^m((\\frac{W\_{j,t} + W\_{j,t-1}}{2}) \* ln(\\frac{X\_{j,t}}{X\_{j,t-1}})))$$

Such that:

-   Output represents
    $\\sum\_{i=1}^n((\\frac{R\_{it} + R\_{it-1}}{2}) \* ln(\\frac{Y\_{it}}{Y\_{it-1}}))$

-   Input represents
    $\\sum\_{j=1}^n((\\frac{W\_{jt} + W\_{jt-1}}{2}) \* ln(\\frac{X\_{jt}}{X\_{jt-1}}))$

where:

-   *Y*<sub>*i*</sub> = individual outputs. This will later be refered
    to as *Q*<sub>*i*</sub> in the following equations.

-   *X*<sub>*j*</sub> = individual inputs

-   *R*<sub>*i*</sub> = output revenue shares

-   *W*<sub>*j*</sub> = input cost shares

-   *t* and *t* − 1 = time, where 1 is the minimum year in the dataset

-   *i* = fishery category, e.g., Finfish (=1), Shellfish (=2)

-   *s* = species, e.g., Salmon, Alewife, Surf Clams

Output Method: From Quantity to Quantity Measures
=================================================

\#\#\#Variable Summary

Variables

-   *Q* = individual quantity outputs in pounds (lbs).

-   *V* = individual value outputs in dollars ($)

-   *Q**E* and *V**E* = simple sum of Quantity (Q) and Value (V)

-   *R* = output revenue shares

-   *b**a**s**e**y**r* is the year to base all indicides from

Subscript Inidicies

-   *t* and *t* − 1 are time subscripts, where 1 is the minimum year in
    the dataset

-   *i* is category, e.g., Finfish (=1), Shellfish (=2)

-   *s* is species, e.g., Salmon, Alewife, Surf Clams
