# Proposal for Semester Project


<!-- 
Please render a pdf version of this Markdown document with the command below (in your bash terminal) and push this file to Github

quarto render Readme.md --to pdf
-->

**Patterns & Trends in Environmental Data / Computational Movement
Analysis Geo 880**

| Semester:      | FS23                                     |
|:---------------|:---------------------------------------- |
| **Data:**      | Transport mode detection  |
| **Title:**     | GPS-based traffic mode detection: Implementation and validation                |
| **Student 1:** | Reto Henzmann                        |
| **Student 2:** | Oswald Gabathuler                        |

## Abstract 
<!-- (50-60 words) -->

This study aims to analyze tracking data using RStudio with a focus on determining modes of transportation accurately through GPS data and road maps. 
By leveraging speed and coordinates, various transport modes will be identified and visualized on maps. 
Additionally, comparisons with Posmo-calculated transport modes will be conducted. This research contributes to understanding transport patterns and trends in environmental data.

## Research Questions
<!-- (50-60 words) -->

To what extent can the mode of transportation be accurately determined using GPS data, in conjunction with road maps and public transport route maps?

## Results / products
<!-- What do you expect, anticipate? -->

For a data set containing movement data between different coordinate points, the corresponding mode of transport is to be determined on the basis of the recorded data and speed. 
This information is to be visualised on a map, with each mode of transport being identified by different colours or symbols. In addition, a comparison map is to be created showing the means of transport calculated by Posmo.

## Data
<!-- What data will you use? Will you require additional context data? Where do you get this data from? Do you already have all the data? -->

GPS data: We use a data set recorded by Posmo that contains coordinates and timestamps. 

The data is obtained from Swisstopo and is the swisstlnm3d landscape model. From this data, we mainly use the road network, the hiking trails, the railway and bus network with the corresponding stops and the cycle network of Switzerland:
https://www.swisstopo.admin.ch/de/landschaftsmodell-swisstlm3d (3D vector data set)

- Road network data of Switzerland: This dataset contains information on all roads, including motorways, country roads and urban roads. 
- Hiking trail map of Switzerland: Contains special routes for hikers. 
- Train and bus network of Switzerland: This dataset contains all train and bus lines, including stops and stations. 
- Cycle network of Switzerland: Shows all official cycle routes.

## Analytical concepts
<!-- Which analytical concepts will you use? What conceptual movement spaces and respective modelling approaches of trajectories will you be using? What additional spatial analysis methods will you be using? -->

To be able to assign trajectories to certain transportation modes, we first need to divide the raw data into individual movements. After that, we can filter out segments, that are too short and also static points. 
In order to detect transport modes, certain variables such as acceleration, maximum speed, average speed and sinuosity need to be calculated. This will help us to set proper tresholds to classify segments to specific travel modes.
 Additionaly, we will include geographical data like road and rail network data to better assign our trajectories.

## R concepts
<!-- Which R concepts, functions, packages will you mainly use. What additional spatial analysis methods will you be using? -->

- filter
- mutate
- Difftime
- calculations for Acc, Speed, distances etc. 
- visualization packages/functions (ggplot2)
- lag/lead
- joins to assign segments

## Risk analysis
<!-- What could be the biggest challenges/problems you might face? What is your plan B? -->

The allocation of the individual segments to different types of transportation could be difficult for some categories, as we limit ourselves exclusively to simple variables and the overlaying of geodata. 
In addition, it could be that the POSMO transport mode data is not 100% correct and we therefore achieve a lower level of accuracy.

## Questions? 
<!-- Which questions would you like to discuss at the coaching session? -->

- Are the planned analytical methods, such as the segmentation of the movement data and the calculation of the variables for the transportation modes, appropriate?

- Do you think we'll need any other concepts/functions that we didn't look at in class?
