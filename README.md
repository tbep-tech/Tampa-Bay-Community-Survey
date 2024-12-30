# Tampa-Bay-Community-Survey
Public data repository with associated R scripts for management and analyses of the Tampa Bay Community Survey.

The following datasets are available in the repository:
  -  <b><u>tbcs_qual_2024.csv</u></b>: Final da-taset with categorical response options, ideal for qualitative anal-yses.
  -  <b><u>tbcs_quant_2024.csv</u></b>: Final data-set with numerical response op-tions (where applicable), ideal for quantitative analyses.
  -  <b><u>public_web.csv</u></b>: Raw dataset from the public (online) sample, which requires cleaning. The data is modified from the original Qual-trics output to remove any poten-tially identifiable information. 
  -  <b><u>public_mail.csv</u></b>: Raw dataset from the public (mail) sample, which requires cleaning. The data is modified from the original re-sponses to remove any potential-ly identifiable information. 
  -  <b><u>innetwork.csv</u></b>: Raw dataset from the in-network sample, which re-quires cleaning. The data is modi-fied from the original Qualtrics output to remove any potentially identifiable information. 
  -  <b><u>public_web_coords.csv</u></b>: Point coordinates of the nearest street intersection provided by a subset of respondents from the public (on-line) sample, and manually verified by TBEP staff. Coordi-nates are based on the NAD 1983 (2011) Florida West projec-tion (CRS 6443). 
  -  <b><u>public_mail_coords.csv</u></b>: Point coordinates of the nearest street intersection provided by a subset of respondents from the public (mail) sample, and manually veri-fied by TBEP staff. Coordinates are based on the NAD 1983 (2011) Florida West projection (CRS 6443). 
  -  <b><u>innetwork_coords.csv</u></b>: Point co-ordinates of the nearest street in-tersection provided by a subset of respondents from the in-network sample, and manually verified by TBEP staff. Coordinates are based on the NAD 1983 (2011) Florida West projection (CRS 6443). 
  -  <b><u>underserved_coords.csv</u></b>: Point coordinates of the nearest street intersection for respondents with-in 300 ft. of an underserved cen-sus tract within the watershed boundary. Coordinates are based on the NAD 1983 (2011) Florida West projection (CRS 6443). 

The following R scripts are available in the repository:
  -  <b><u>data_management.R</u></b>: Script used for compiling, cleaning, and ma-nipulating the raw datasets. Gen-erates the final quantitative and qualitative datasets.
  -  <b><u>data_analysis.R</u></b>: Script used for analyzing the final quantitative and qualitative datasets. Gener-ates the summary statistics and raw figures used in this report.
  -  <b><u>cfa.R</u></b>: Script used for performing confirmatory factor analyses (CFA) and other tests of the com-posite psychometric variables. Generates the CFA tables and correlation matrices used in this report.

Users should consult the data dictionary for descriptions of each field contained in the datasets available in the repository.
