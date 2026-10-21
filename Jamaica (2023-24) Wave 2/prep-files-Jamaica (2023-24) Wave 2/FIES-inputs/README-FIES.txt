Running the FIES analysis

Share this entire FIES-inputs folder, including FIES-runtime.R, the helper
scripts and the reference RData file. micsPlusTableR is not required.

1. Install the statistical dependencies once:
   install.packages(c("memisc", "Hmisc", "survey", "RM.weights", "reshape2",
                      "dplyr", "ggplot2"))
2. Open the file ending in _Script.R. Set country and wave at the top for
   standalone runs. These generate the report prefix and default data filename.
   Adjust hh_path if your household data is stored elsewhere. path_fies defaults to this script's folder;
   path_fies_output defaults to ../FIES-outputs and is created automatically.
   Data and output paths may be absolute or relative to path_fies.
   output_prefix is generated once and reused in report titles and filenames.
   There is no configuration list or separate table setting.
3. Run the whole script with R's source(), RStudio Source or Rscript.
   Example: source("/full/path/to/FIES-inputs/JAM MICSPlus Wave2 - Table 4.1_Script.R")
   To run selected lines instead, set path_fies explicitly.

In the app, its selected paths and survey label are used automatically.
Code-only prepare_mics_data() calls supply paths; without an app survey label,
output_prefix remains the label set in the script. No setwd() is used.
The resulting hh and res.df objects remain available as before.

Calculations still assume the survey's existing variable names, response
labels, weights and breakdowns. Changing paths or the report label does not
adapt those statistical definitions to a different questionnaire.

Helper files are found by stable suffixes, independent of their survey prefix.
Keep one version of each helper and reference file in this folder.
FIES-runtime.R is generated from the package sources; it uses RM.weights but
not micsPlusTableR.
