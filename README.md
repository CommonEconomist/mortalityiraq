Replication material: *"Half a million excess deaths in the Iraq war: Terms and conditions may apply"*    
Submitted: 2017.04.28  
Accepted: 2017.08.04    
URL: https://journals.sagepub.com/doi/full/10.1177/2053168017732642    

Data and code to replicate results as reported in the paper. 
Data processing and analysis were carried out in R.
For issues replicating the results contact me via email: vanweezel (at) pm.me.

#### Description
`raw_data` includes some of the original replication files of the Hagopian et al. (2013) study:
* `hh_deaths.csv`: information on the recorded fatalities;
* `hh_roster`: household survey data;
* `pop.csv`: population data for Iraq.

Additionally the folder includes a number of files starting with `IRQ_adm1`.
These are all part of the shape file needed to produce the choropleth plots `figures_annex.R`.

`data`
*  `processed.Rdata`: output of `re_analysis.R`

`code` includes all R-scripts needed for the analysis presented in the paper and supplementary material.
* `functions.R` which contains a number of functions:
    1. a bootstrap function, 
    2. robust clustered errors, and 
    3. functions for standardising the input variables for the regression
    
*  `replication.R`: replicates the results from the Hagopian et al. (2013) study and is based on the Python script provided in the original replication files; 
*  `identify_governorates.R`: identifies the governorates as described in section A.3.1;
*  `re_analysis.R`: re-analysis of the data as reported in section 2.3 of the paper. Note that this script also processes the data needed for the figures and the regression, which are saved as `processed.Rdata`;
* `bootstraps.R`: produces all the bootstrap estimates and should be run after `re_analysis.R`. Running this script can take a while; each bootstrap takes about 15 minutes;
*  `prep_regression.R`; uses `processed.Rdata` as input along with the shape file to prepare the data for the regression analysis;
*  `regression.R`: fits the regression models as reported in table 2 and A3;
*  `figure_paper.R`: produces the figures in the main text and should be run after the analysis is done;
*  `figures_annex.R`: creates all the figures included in the supplementary material, including the maps.  


#### BibTeX
```
@article{spagat2017,
  title={Half a million excess deaths in the Iraq war: Terms and conditions may apply},
  author={Spagat, Michael and van Weezel, Stijn},
  journal={Research \& Politics},
  volume={4},
  number={4},
  pages={2053168017732642},
  year={2017},
  publisher={SAGE Publications Sage UK: London, England}
}
```
