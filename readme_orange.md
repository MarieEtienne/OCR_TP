# Orange yields - Data package

This data package contains the data that powers the chart ["Orange yields"](https://ourworldindata.org/grapher/orange-yields?v=1&csvType=full&useColumnShortNames=false) on the Our World in Data website. It was downloaded on September 16, 2026.

### Active Filters

A filtered subset of the full data was downloaded. The following filters were applied:

## CSV structure

Each row is an observation for an entity (usually a country or region) at a timepoint.

- "Entity" — the name of the entity, e.g. "United States".
- "Code" — our internal entity code. For most countries this is the [ISO alpha-3](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3) code, e.g. "USA"; historical and other non-standard entities get a custom code.
- "Year" or "Day" — the timepoint. Annual data has a "Year" column holding an integer year; otherwise a "Day" column holds a date string in the form "YYYY-MM-DD".
- The final column is the data column — the time series that powers the chart. Downloaded with the "full data" option it corresponds to the time series below; with "only selected data visible in the chart" it is transformed depending on the chart type, so the correspondence may be less direct.


## Metadata.json structure

The .metadata.json file contains metadata about the data package. The "charts" key contains information to recreate the chart, like the title, subtitle etc. The "columns" key contains information about each of the columns in the csv, like the unit, timespan covered, citation for the data etc.

## How we process data at Our World in Data

Our World in Data is almost never the original producer of the data - almost all of the data we use has been compiled by others. If you want to re-use data, it is your responsibility to ensure that you adhere to the sources' license and to credit them correctly. Please note that a single time series may have more than one source - e.g. when we stitch together data from different time periods by different producers or when we calculate per capita metrics using population data from a second source.

Preparing this data involves several processing steps. Depending on the data, this can include standardizing country names and world region definitions, converting units, calculating derived indicators such as per capita measures, as well as adding or adapting metadata such as the name or the description given to an indicator.
[Read about our data pipeline](https://docs.owid.io/projects/etl/).

## Detailed information about the data


### Orange yields – UN FAO
Yield is the amount produced per unit of land used, measured in tonnes per hectare.
Last updated: February 25, 2026  
Next expected update: February 2027  
Date range: 1961–2024  
Unit: tonnes per hectare  
Source: Food and Agriculture Organization of the United Nations (2025) – with major processing by Our World in Data  

#### How to cite this data

Food and Agriculture Organization of the United Nations (2025) – with major processing by Our World in Data

#### How this data is described by its producers
Item: Oranges

Description: Oranges This subclass includes: - oranges, Citrus sirensis -  bitter oranges, Citrus aurantium This subclass does not include: -  bergamots, cf. 01329 - chinottos, cf. 01329

Metric: Yield


## Sources

These are the sources behind the data in this package. Each time series above names the ones it draws on in its citation.

### Food and Agriculture Organization of the United Nations – Production: Crops and livestock products

Crop and livestock statistics are recorded for 278 products, covering the following categories:

1) Crops primary: Cereals, Citrus Fruit, Fiber Crops, Fruit, Oil Crops, Oil Crops and Cakes in Oil Equivalent, Pulses, Roots and Tubers, Sugar Crops, Treenuts and Vegetables. Data are expressed in terms of area harvested, production quantity and yield. Cereals: Area and production data on cereals relate to crops harvested for dry grain only. Cereal crops harvested for hay or harvested green for food, feed or silage or used for grazing are therefore excluded.

2) Crops processed: Beer of barley; Cotton lint; Cottonseed; Margarine, short; Molasses; Oil, coconut (copra); Oil, cottonseed; Oil, groundnut; Oil, linseed; Oil, maize; Oil, olive, virgin; Oil, palm; Oil, palm kernel; Oil, rapeseed; Oil, safflower; Oil, sesame; Oil, soybean; Oil, sunflower; Palm kernels; Sugar Raw Centrifugal; Wine.

3) Live animals: Animals live n.e.s.; Asses; Beehives; Buffaloes; Camelids, other; Camels; Cattle; Chickens; Ducks; Geese and guinea fowls; Goats; Horses; Mules; Pigeons, other birds; Pigs; Rabbits and hares; Rodents, other; Sheep; Turkeys.

4) Livestock primary: Beeswax; Eggs (various types); Hides buffalo, fresh; Hides, cattle, fresh; Honey, natural; Meat (ass, bird nes, buffalo, camel, cattle, chicken, duck, game, goat, goose and guinea fowl, horse, mule, Meat nes, meat other camelids, Meat other rodents, pig, rabbit, sheep, turkey); Milk (buffalo, camel, cow, goat, sheep); Offals, nes; Silk-worm cocoons, reelable; Skins (goat, sheep); Snails, not sea; Wool, greasy.

5) Livestock processed: Butter (of milk from sheep, goat, buffalo, cow); Cheese (of milk from goat, buffalo, sheep, cow milk); Cheese of skimmed cow milk; Cream fresh; Ghee (cow and buffalo milk); Lard; Milk (dry buttermilk, skimmed condensed, skimmed cow, skimmed dried, skimmed evaporated, whole condensed, whole dried, whole evaporated); Silk raw; Tallow; Whey (condensed and dry); Yogurt.

Producer: Food and Agriculture Organization of the United Nations  
Published: 2025-12-31  
Retrieved on: 2026-02-25  
Retrieved from: http://www.fao.org/faostat/en/#data/QCL  
Direct download: https://bulks-faostat.fao.org/production/Production_Crops_Livestock_E_All_Data_(Normalized).zip  
License: CC BY-NC-SA 3.0 IGO (http://www.fao.org/contact-us/terms/db-terms-of-use/en)  

Citation: Food and Agriculture Organization of the United Nations – Production: Crops and livestock products (2025).

    