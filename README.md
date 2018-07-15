# NFL_Combine_Analysis
Scrapes NFL Combine Historical Performances from Pro Football Reference and nflcombineresults.com

# Status
Still a work in progress: planning on continuing to build out visualizations and examining more positional trends (e.g. average linebacker speeds over time, average height/weight by position over time, etc.)
--> In the end, I'll create a cleaner PDF format deliverable to showcase most interesting findings.

# File Summaries
1. data_wrangle.R - utilizes rvest package to scrape web data
  - creates two main data frames:
    1. Draft_Overview_Data - from Pro Football Reference - shows high-level summary data from each draft over the last several decades (e.g. total draft selections, picks by position, Hall of Fame inductees) 
    2. all_combine_data - from nflcombineresults.com - much larger dataset: shows granular performance details from all NFL combine participants since 1987 (e.g. participant names, colleges, all combine event performances)

2. analysis.R - loads data_wrangle.R file and generates a collection of visualizations 
  - visualizations include: HOF inductees per draft class, top combine performance trends (e.g. top forty time / year has steadily gotten faster over time), individual combine_event top performers with exemplary performances labelled by name
