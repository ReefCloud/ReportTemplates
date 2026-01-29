# ReportTemplates
A collection of functions to call upon the ReefCloud API to access the ReefCloud Public Dashboard data (https://reefcloud.ai/dashboard/) for generating reporting templates.

## Guidelines
These functions and the package (TBD) are designed to be used with ReefCloud data and aims to make the reporting pipeline through data collection and analysis more efficient. There are four major groups of functions and templates designed around different purposes.

### API functions
These are functions that call upon the ReefCloud API directly, to access data that are made publicly available by the different project owners. Many of these functions use the Tiers defined by reef regions or sites at the most specific level and give aggregated information based on modelled data as compared to raw individual data (https://docs.reefcloud.ai/get-results/public-dashboard).

### Plotting functions
These are functions that use the data from the API functions to create standard plots that are commonly used to in regular reporting for different organisations, made in partnership with these organisations. The idea here is that these functions can be called within the framework of a Quarto markdown file within the *Reporting templates* section to generate a relatively automated skeleton of a report that can then be filled with more information to explain the status and trends. This is designed to speed up reporting with ReefCloud data. Many of these functions are wrapped around the ggplot2 package to make plots and can be created on your own with ggplot as well.

### Reporting templates
These are templates designed to take input from the API functions, pass them through to the plotting functions to get figures usable tailored for a regular report template. These draft templates can then be added on to, to be made into regular reports for these monitoring data.

### Misc functions
Additional functions that do not directly relate to the API or plots are located here. These are functions that serve more niche purposes in changing the forms of data or converting necessary data to formats for use with the other functions available.

## Use cases
### Palau International Coral Reef Center Long-term Monitoring Report



## Recommendations
These functions call on the ReefCloud API using publicly available summarised data from the ReefCloud Public Dashboard. Data presented on the dashboard have undergone quality assurance checks based on machine learning metrics as well as general data validation by the project owners before they are presented on the public dashboard, but interpretation of the results should come with caution with guidance from the respective data providers.

