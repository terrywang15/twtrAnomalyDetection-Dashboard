# twtrAnomalyDetection-Dashboard
A dashboard that shows unusually viral tweets given a keyword based on some metrics.  

The dashboard will show tweets that are unusually viral given the keyword, based on its right tail probability on distributions of number of retweets, favorites, and rate of increase of the metrics for each 30 minute interval.

The goal of the project is to take any arbitrary keyword as input and output the graphs on a dashboard, but will require significant more capability to ingest twitter data beyond what is permitted using a free Twitter API.

Currently the project is in MVP stage, and all essential components are working.

### Installation

Will require shiny - RStudio is the best way to use this.

### Acknowledgment

Srihari Sheshadri contributed on backend data engineering pipeline. See his repository for details.
