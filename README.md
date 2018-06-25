# Red-Drum-Data-Synthesis
#Synthesis of Gulf of Mexico red drum data

#Overview
Project intended to describe the recruitment dynamics of red drum populations throughout the Gulf of Mexico.
The code hosted here describes the analyses and the creation of figures and outputs intended to be used by agencies, other researchers, and the public.

#Details
Project uses both fisheries independent and dependent data.  Currently only code using fisheries independent data from the state of FL is available on this project.
All data are available via Florida Fish and Wildlife Research Institute; the raw data are not posted here.

The code currently has 6 steps or parts, described below:
1. Creating catch files
  -Currently not implemented pending review of collaborators.  It will be added when authorized.
  
2. Standardizing catch files data
  -Catch data are standardized for each of four spatially discrete sites: Apalachicola Bay (APM), Cedar Key (CKM), Tampa Bay (TBM) and Charlotte Harbor (CHM). For each site, young-of-year red drum (<125mm standard length) are standardized separately from larger red drum considered to be recruited (>125mm <400mm).  Separate files exist to standardize these data, and are identified by site name and fish size (e.g., TBM YOY is Tampa Bay recruits).
  
3. Plotting Standardized Recruits
  -A single file (indvidual and allsites IF_newrec.R) plots either individual sites or any selected sites.
  
4. Correlation analyses
   -The code simple_cor_newrec.R performs a simple Pearson's correlation analysis and will plot the resulting correlation matrices.

