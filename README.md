# Purpose of Code

This code is in support of the NPS Northeast Region Fire Effects Monitoring program.

# File Structure

This repository contains 2 main files, the "scripts" file and the "output" file.

Note: [Data]{.underline} is stored on the N Drive (SHEN shared drive) at this file location:

-   "N:RAD\\Fire\\Fire Mgmt\\FIRE_ECOLOGIST_FILES\\FFI Data Management\\Exports from FFI\\NER"

## scripts

The scripts file contains all code. It may contain sub folders for organization, which separate the code by tasks

-   "Clean" folder

    -   If FFI outputs are not suitable, use this code to clean them up

-   "Field" folder

    -   Scripts related to field work (e.g. prepping data for import/export)

-   "QAQC" folder

    -   Scripts related to Quality Control and Quality Assurance

-   "Viz" folder

    -   Scripts to visualize datasets

-   "Analysis" folcer

    -   Scripts to analyze datasets

## output

The output file contains any output from the code itself. These can be recreated by running the correct script. Examples of relevant output files are shown below. These files are displayed on GitHub, but the contents are not uploaded or backed up. This output file should only store temporary outputs. Final versions of these outputs should be saved to a different file structure.

-   data_clean
-   errors
-   graphs

## data

The data folder should be located in your government file system, not on GitHub. How you organize this is up to you, but the R scripts here will assume a certain structure. The assumed file structure will be outlined at the beginning of each script.

# Datasets

FFI datasets used in these scripts can be found in the following FFI tabs.

## Project Management Tab

In the upper left hand corner of the FFI Project Management section, you will see a "Utilities" drop down. From here you have three reports available to download. This is the plot metadata.

-   Sample Event Report

-   Macroplot Report

-   Project Unit Assignment Report

## Data Entry and Edit Tab

For each plot, you may download individual protocols for any single sample event.

## Query Tab

For any protocol, you may download data with a large combination of selected filters. If no filters are selected, you may download the entire dataset for each individual protocol. Examples of protocol exports are provided below.

-   Cover - Points (metric)\_XPT.csv

-   Cover - Species Composition (metric)\_XPT.csv

-   Density - Belts (metric)\_XPT.csv

-   Density - Quadrats (metric)\_XPT.csv

-   Post Burn Severity_XPT.csv

-   Surface Fuels - 1000Hr_XPT.csv

-   Surface Fuels - Duff_Litter_XPT.csv

-   Surface Fuels - Fine_XPT.csv

-   Trees - Individuals (metric)\_XPT.csv

## Reports and Analysis Tab

In the upper left hand corner of the Reports and Analysis Details tab, you will see a "Report" drop down. From here you may download statistical reports of the selected data.

## Species Management Tab

In the upper left hand corner of the Species Management Details section, you will see a "Species" drop down. From here you have one report available to download. This is the species metadata.

-   Local Species Report (CSV)

# Column Names

## Metadata

Metadata includes columns from the Sample Event Report, Macroplot Report, and Project Unit Assignment Report. All these reports are downloaded from the Project Management Tab. This section will detail ambiguous columns and explain how they should be used. Obvious columns (such as Latitude or Elevation) will not be explained.

-   ProjectUnit

    -   A grouping of plots, either grouped spatially or grouped by ecosystem characteristics. Plots may be assigned to multiple Project Units if they can be grouped in multiple ways. Plots should only be analysed in comparison to other plots in their group.

    -   Project Units may also be referred to as Monitoring Types. A Monitoring Type is a major fuel-vegetation complex or vegetation association that is treated with a particular burn prescription.

-   Macroplot

    -   A single plot. Macroplots should contain the name of their Project Unit followed by a number.

-   Multi_PU

    -   If a Macroplot is grouped under multiple Project Units, this column will have the value "Y". If not, this column will have the value "N"

-   Purpose

    -   Plot Type, such as Fire Monitoring Handbook (FMH), Composite Burn Index (CBI), Rapid Assessment Plots (RAP), etc. Each plot type will have a unique shape and will not be directly comparable without statistical manipulation, even if similar protocols are used.

        -   FMH

            -   rectangular plots with herbaceous / shrub data collected along the long edge, tree data collected in the interior, and fuels transects at random azimuths from the interior.

        -   RAP

            -   circular plots with two fuels transects. herbaceous data is collected as percent cover of target species, tree data collected in the interior, and fuels along fuels transects

        -   CBI

            -   

-   Type

    -   Plot type refers to the major vegetation community (forest, brush, or grassland). This is a subset of the plot Purpose. FMH Forest plots, for instance, will have a more complex layout compared to FMH Grassland or FMH Brush plots.

-   Protocols

    -   Sampling techniques used to measure ecological data. Separate protocols are used to measure trees, fuels, herbs, shrubs, and seedlings. Post burn protocols are used to measure fire severity. A variety of protocols can be used for each plot purpose.

-   MonStatus

    -   A code applied to each sample event to describe the status of that plot in a single point of time. Codes are structured as so:

        -   [\##]{.underline}STATUS##

            -   [\##]{.underline} refers to the disturbance number. 00 = no historic disturbances, 01 = first disturbance. 02 = second disturbance, etc.

            -   STATUS = PRE, POST, or YEAR, depending on time since fire. Once a plot burns, it moves into POST status for immediate post burn reads. After at least a year, the plot moves into YEAR status followed by the number of years since the burn. If a plot has not burned in 20+ years, it may move back into PRE status

            -   \## refers to the number of years post burn. This number will only follow plots with YEAR status. Plots should be measured on a schedule of 1, 2, 5, 10, and 20 years post fire (YEAR01, YEAR02, YEAR05, YEAR10, or YEAR20). If the status is PRE or POST, it can be assumed that the year is 0.
