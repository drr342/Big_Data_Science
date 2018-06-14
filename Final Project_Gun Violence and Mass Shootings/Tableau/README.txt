We used the Tableau Business Intelligence tool to generate a couple of visualizations of our data.
This tool was not contemplated in the original plan for our project and the plots we generated are
only a very basic example of the things that can be achieved whit this powerful tool.

In the Visualizations.twb file there are three plots:
    1) Sheet 1: Clustering by ZIP Code for the 2014 dataset. Similar to the task performed in RStudio,
       we let Tableau generate clusters based on the selected features (in R) for the dataset. The colors
       in the map represent the different clusters.
    2) Sheet 2: Clustering by State and City for the Stanford MSA dataset. The color and size of the
       rectangles represent the sum of fatalities of all incidents related to that location.
    3) Sheet 3: Clustering by the day of the week for the Stanford MSA dataset. Similar to sheet 2, but
       in this plot the colors represent a different day of the week. The size of the circles represent
       the sum of fatalities of incidents related to a certain location.

The .twb file allows to modify the plots generated or create new ones. However, it requires that Tableau
desktop be installed in the local computer.

For an interactive visualization (no editing) of the plots described in this document go the following link:
https://public.tableau.com/profile/daniel8125#!/
