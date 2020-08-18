# Better coverage, better outcomes?

This is the repository of the simulation study for the paper 'Better coverage, better outcomes? Mapping mobile network data to official statistics using satellite imagery and propagation modelling techniques'

The replication results may slightly differ from the results presented in this study as it cannot be ensured that the 10\%-sample of the census data provided by the statistical office of Senegal are identical to the one used in this study. Following data sources not available in the repository are necessary in order to run the code:

- spss_car_individus_10eme_dr.sav: The 10\% sample of the population part of the RGPHAE 2013. Access can be requested via the microdata portal of the statistical office of Senegal (ANSD): http://anads.ansd.sn/index.php/catalog/51/
- SITE_ARR_LONLAT_EXACT.csv: This file contains the exact tower locations of SONATEL in 2013. Access can be requested as stated in the data availability statement of this study. To facilitate replication, a file with slightly randomized antenna locations is provided (SITE\_ARR\_LONLAT.csv). Keep in mind this may affect the final outcomes. The exact locations have to be requested as stated in the data availability statement.
- sen_ppp_2013.tif: This file contains the population density estimates of Senegal for the year 2013. The data can be downloaded at the WorldPop website: https://www.worldpop.org/doi/10.5258/SOTON/WP00645
- senegal.tif: This file contains the GUF data for Senegal at 0.4 arcseconds. Access can be requested for scientific, non-commerical purposes via the website of the German Aerospace Center DLR: https://www.dlr.de/eoc/en/PortalData/60/Resources/dokumente/guf/DLR-GUF_LicenseAgreement-and-OrderForm.pdf

The application is written in Python and R. The file and folder names indicate the required order of execution. Files 04 - 07 may not be run unless access to individual-level CDRs is available. It may be necessary to align directory paths to make the code run properly. Please ensure that more than 20GB RAM is available for this analysis. If needed, please contact the corresponding author for support.
