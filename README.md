# Minnesota-Bee-Atlas
Raw data and code associated with "Determining Minnesota Bee Species’ Distributions and Phenologies with the Help of Participatory Science"

**Files inlcuded**
1. Trap nest data <br>
"TrapNest2016.xls" to "TrapNest2019.xls" <br>
Specimens collected from trap nests from 2016 through 2019

2. Bumble bee data<br>
"BumbleBeelocationdataformaps.xlsx" <br>
Location data MN Bee Atlas records are total abundances per species per route per year, averaged if route was run for more than one year.  iNaturalist includes research-grade bumble bee observations through November of 2022, Bumble Bee Watch includes expert-verified records through November of 2022 , and Bumble Bees of North America includes specimen-based Minnesota records from the Bumble Bees of North America database.

"atlasspmatrixyear.csv" <br>
Abundances for bumble bee species per route per year

"qtyperyearrouteeco.csv" <br>
Total bumble bee abundance per route per year by Ecological Province

"routelanduse.csv"<br>
Land use surrounding bumble bee survey routes

3. Bumble bee analysis code<br>
"BeeAtlasBumbleBeeAbundance.R"<br>
Analysis of bumble bee abundance

"MNBeeAtlasCCA.R"<br>
Canonical correspondence analysis for association of bumble bee species and land use



The Minnesota Bee Atlas project contributed new information about bee distributions, phenologies, and community structure by mobilizing volunteers to document bees statewide. Volunteers submitted iNaturalist photograph observations, monitored trap nests for tunnel-nesting bees, and conducted roadside observational bumble bee surveys. By pairing research scientists and participatory science volunteers, we overcame many geographic and temporal challenges to document the presence, phenologies, and abundances of species. By engaging over 2500 volunteers, we also promoted conservation action for pollinators through our educational programs and interactions.

**Methods**

We observed and collected bees as part of the participatory science project “Minnesota Bee Atlas” which operated between 2016-2020. We recruited volunteers by advertising to local volunteer groups and conservation organizations, on social media, and through University of Minnesota web pages. Volunteers had various affiliations including the Minnesota Master Naturalist program, Minnesota Department of Natural Resources Scientific and Natural Area stewards, Environmental Learning Centers, nature centers, county natural resource departments, Soil and Water Conservation Districts, native plant nurseries, and federal agencies including the U.S. Forest Service and the U.S. Fish and Wildlife Service.  Approximately 150 volunteers participated in the two structured sampling tiers each field season, and as of March 2021, 2300 volunteers submitted observations to iNaturalist. Volunteers learned research protocols and bee identification by attending in-person workshops, reading written manuals, viewing recorded videos, and attending online webinars. In 2020, due to restrictions on gatherings due to the COVID-19 pandemic, all training was online.


**iNaturalist**
The broadest and simplest level of participation relied on the mobile app and website iNaturalist (© California Academy of Sciences 2016). This global public biodiversity portal enables individuals to upload locations and evidence of living things, including photos or recordings, which are then identified by the observer, other users, or an algorithmic suggestion based on existing confirmed observations.s. Species cannot always be determined. Therefore, each identification is qualified based on a data validation system and considered “research grade” if an observation is not of a captive or cultivated species, has a date, photo and location, and two-thirds of users agree on genus and species-level identification. This is not foolproof, as there are no required credentials to add identification, but the quality of identification typically grows over time as additional users join the platform and as additional experts in bee identification participate. Once identifications reach “research grade”, records feed into databases such as Discover Life (discoverlife.org) and GBIF (www.gbif.org). We trained participants who attended workshops to add bee observations to iNaturalist and to identify bees to groups, usually family. 

**Tunnel-nesting bees**
Participants hung and monitored nest blocks in semi-natural habitats during the growing season. They attended in-person or online training and received a written instruction manual with photographs of different plug materials that bees and wasps use to seal the end of their completed nests. Block design and nest plug descriptions were adapted from The Bees' Needs (Rose, Scott, and Bowers, 2015). We drilled and labeled five tunnels of six different diameters (3.18 mm, 4.76 mm, 6.35 mm, 7.94 mm, 9.53 mm, and 11.11 mm) into blocks of untreated pine or Douglas fir with a cedar shingle roof. Volunteers placed blocks in a semi-sunny location facing east or south at a height of 1 to 2 m, with the flexibility to find a mounting site that fit their habitat. Volunteers recorded specific mounting conditions and reported nest evidence every 2-3 weeks via the project web page. Bee Atlas staff provided feedback on photographic observations via email and newsletters. With the goal of surveying the whole state, we actively recruited participants in rural areas and in areas with less existing data (Figure 1). In 2016, 2017 and 2018, we sent out 120, 129, and 141 nest blocks respectively. 

We received one home made stem bundle of Phragmites from one volunteer each year between 2016 and 2018. In 2019, the final year, we sent 11 additional nest bundles made with hollow or pithy plant stems to selected volunteers to observe nesting with different natural substrates. We made each bundle from stems of one of 6 plant species; Asclepias incarnata, Silphium perfoliatum, Arnoglossum atriplicifolium, Helianthus giganteus, Vernonia fasciculata, or Liatris ligulistylis, and placed bundles inside a plastic sleeve with an overhanging roof made from a 32 oz beverage bottle. We sealed the backs of the stems with cotton balls and latex. The number of stems per bundle varied due to the size differences between stems. Monitoring protocols were similar to those used for nest blocks. 

In the late fall, volunteers returned blocks and stem bundles to the University of Minnesota for overwintering and rearing in a temperature-controlled growth chamber. After a four-month period at 5°C, we stimulated emergence by increasing the temperature in steps to a high of 30°C. We covered each nest tunnel entrance with test tubes and removed emerging bees and other occupants during daily checks. Detailed descriptions of nesting blocks and rearing methods can be found in Satyshur et al. (2020). Some bees appeared to have already emerged when blocks were returned in fall 2016, so in 2017 and 2018, we removed a few blocks with similar plugs from the field mid-summer and reared at the lab at ambient temperature. We replaced those blocks with new blocks. We identified bees to species using keys and comparisons with previously identified materials (Mitchell 1962, Sandhouse 1939, Sheffield et al. 2011, Arduser 2018, online keys at www.discoverlife.org: Andrus et al. 2020a,b,c, Griswold et al. 2020, Nelson and Droege 2020a,b, Orr et al. 2020). Experts, including Jason Gibbs, Michael Orr and Ryan Oram, confirmed identification of more difficult specimens. We deposited voucher specimens in the UMN Insect Collection. For distribution mapping, we included locations of specimens in the UMN Insect Collection database. Many specimens did not have latitude or longitude associated with their records. In such cases, we used the location description to estimate the most accurate position possible.

We examined nesting phenology using volunteer-submitted nest plug observations. For each nest that produced bee offspring, project staff evaluated volunteer observations and assigned a quality value based on clarity and frequency of observations. Nests with “high” or “medium” quality values were used in phenological estimations, with 65.1% of observations meeting those criteria. Volunteer protocol did not call for daily observations, so we recorded nest completion as the interval between the last date that the volunteer recorded an empty tunnel and the first date with a complete nest plug. By assigning a value of one to each date between the last empty tunnel observation and the first complete nest plug observation for each tunnel and summing by date, we identified  the most frequent nest completion interval dates. Dates recorded with higher frequency indicate increased likelihood that nests are completed on those dates. 

**Bumble bees**
We trained volunteers in survey methods and skills to distinguish bumble bees from other insects, determine sex, identify readily-distiguishable bumble bee species, and photograph bumble bees to enable identification. Based on regional collections, we estimated that 90% of observations would be readily distinguishable species (Bombus impatiens, B. bimaculatus, B. griseocollis, or B. ternarius). We adapted survey methods from previous state-wide bumble bee surveys that used lethal collection methods (Golick and Ellis, 2006, McFarland, Richardson, and Zahendra, 2015, Richardson et al., 2019). Due to conservation concerns, we used observational data instead of specimen collections. Forty-four volunteers observed bees at five stops along 39.5 kilometer routes three times each year, between late June and mid-August with at least two-weeks between visits, between 10 a.m. and 6 p.m. on days with little or no precipitation, temperatures greater than 15.6 C, and wind speeds less than 32.2 kph. Volunteers surveyed 45 different routes between 2016 and 2020, with 17 routes run for three or more years (Figure 1, Table 1). Routes were based on established North American Breeding Bird Survey routes (USDOI, 2017) because of their accessibility and systematic spread across different ecological areas. For analysis, the single route from the TAP ecological province was combined with routes from the PP ecological province due to the low sample size in this province and ecological similarity. Volunteers chose five stops along a route by finding flower patches with bee activity located at least one 1.61km (1 mile) from each other. On average, survey stops were 5.23 kilometers apart from each other. Volunteers examined flower patches within 150 meters of the survey stop, collecting bumble bees from flowers into jars for ten minutes of collecting time, noting the flower’s identity. Volunteers placed bees in coolers with ice to avoid risk of bees overheating and to ease photography. Volunteers counted and released readily identifiable individuals, and photographed all others as well as all individuals of conservation concern (Bombus affinis, Bombus terricola, Bombus pensylvanicus, and all subgenus Psithyrus other than Bombus citrinus) as listed by the International Union for the Conservation of Nature (Hatfield et al. 2015). Volunteers submitted data through the Bee Atlas website. We (EE) verified identifications for all photo-specimens. Most specimens (89%) were identified by volunteers, with 10% of specimens verified with photographs, and 1% unverifiable. 
We used Rstudio version 1.2.5033 (Rstudio Team, 2019) for all statistical analyses. Two species, Bombus vagans and Bombus sandersoni, were grouped for analyses because most observations did not include identifying features that enabled species verification. We examined differences in abundance among ecological provinces with linear mixed-effect models with the lme4 package (Bates et al., 2015) with log-transformed abundance of bumble bees per route per year as the response variable and ecological province as the predictor. Year and route were included as random effects. We limited routes to those with three completed survey dates within a year, which equaled 150 minutes of survey time. We examined the relationship of bumble bee species to land cover using constrained correspondence analysis (CCA) of a land cover-species abundance matrix with the vegan package (Oksanen et al., 2020). We summarized land cover within 2 km of survey stops along routes using the 2016 National Land Cover Database (NLCD) (Dewitz, 2019). We verified land-cover categories by examining aerial photographs. We simplified NLCD land-cover classes to groupings that we consider to be biologically relevant to bumble bee distribution, listed in descending percentage of cover: cultivated crops (74%), wetlands (8%, woody wetlands and emergent herbaceous wetlands), forested (7%, deciduous, mixed, and evergreen forest), developed (7%, all developed categories), and open water (3%), and grasslands (1%, grasslands/herbaceous, pasture/hay) (Crowther et al. 2014, Carvell et al. 2017, Lanterman et al. 2019, Mola et al. 2020). After preliminary analysis, we removed the variable wetlands from analysis due to multi-colinearity (variance inflation factor >20), the variable open water due to poor correlation (intra-set correlations with axes 1,2, or 3 <0.4), and species accounting for less than 5% of the inertia for CCA 1 and 2 (Bombus borealis, B. citrinus, Bombus insularis, Bombus rufocinctus, and Bombus vagans group). Significance of the overall CCA and ordination axes was determined with a Monte Carlo permutation test with 999 randomizations.
