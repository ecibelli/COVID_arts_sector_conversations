## COVID-19 Arts Sector Conversations

In Spring 2020, Ingenuity conducted a series of conversations with stakeholders in the arts education sector in Chicago - the [ArtsEd Response Collective](https://www.ingenuity-inc.org/home/arc/). The goal was to understand the fallout from COVID-19 and the shelter-in-place order on arts institutions and non-profits and the school system in the city.

One goal of the process was to ensure diverse representation from participants across the city. To support that effort, basic participant statistics were collected from every participant. These were supplemented with data in the [artlook database](https://chicago.artlookmap.com) to give detailed information about geography, artistic disciplines, and demographics representated by the participants.

The output is a single image comprised of multiple ggplot objects, laid out in a customized grid.

To protect anonymity of participants, the script contained in this folder skips the initial data cleaning steps, which surfaced organization names and participant contact info. The script starts with the cleaned data files, with anonymized identifying info, and shows the steps taken to visualize the data.

#### Contents of this folder:
 + **chicago_community_area_boundaries/**: Shape files for the city of Chicago and its community areas, for mapping.
 + **data/**: Contains the anonymized participant data, anonymized school demographic data, and Chicago city census data by community area.
 + **figures/**: Output of individual ggplot objects that are part of the final visualization. Saved to disk to ensure that basic aesthetics are correct.
 + **ARC_participant_visualization_YYYY-MM-DD.png**: the final visualization.
 + **build_visualization.R**: The primary script to build the visualization.
 + **plotStandards.R**: Some organization-internal plotting conventions, including color sets and custom ggplot themse.
 
 #### Output:
 
 ![Participants in the Spring 2020 Ingenuity ArtsEd Response Collective](ARC_participant_visualization_2020-05-28.png?raw=true)
