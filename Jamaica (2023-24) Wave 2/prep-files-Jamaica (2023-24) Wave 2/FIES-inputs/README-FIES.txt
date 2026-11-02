Running the FIES analysis

Keep the main Script.R, the four original survey helper scripts, and the
reference RData file together. No runtime file or micsPlusTableR installation
is needed for standalone runs.

Standalone
1. Open the micsPlusTableR-Files R project so here::here() resolves its root.
2. Edit the explicit hh_path, path_fies, path_fies_output and output_prefix
   settings at the top of JAM MICSPlus Wave2 - Table 4.1_Script.R if needed.
   The source() and load() calls name the exact helper and reference files;
   edit those filenames directly when using a different set of files.
3. Put the helper files in FIES-inputs under the project root. The script
   creates FIES-outputs beside it automatically if missing. Existing output
   folders are kept. The script never changes working directory.
4. Run with source(), Rscript, or selected lines in RStudio.

The analysis uses ordinary RM.w() and PC.w() calls. RM.weights writes its own
model reports to the current working directory. After all calculations and
table output are finished, the final block moves the explicitly listed model
report files into FIES-outputs. Other outputs use path_fies_output directly.
Copies overwrite reports from earlier runs; originals are removed only after
all copies succeed. If the working directory is already the output folder,
the reports stay there. Edit report_files if you enable additional reports
such as PC.w(..., write.iteration = TRUE).

Packages load normally with library() or package::function(). Missing packages
produce R's usual error; the scripts do not install packages.
The analysis uses here, memisc, Hmisc, survey, RM.weights, reshape2, dplyr and ggplot2.

App
run_fies_table() supplies the selected data, helper and output paths to the
same main Script.R. The same final report-move block runs after the analysis.
The app's working directory is unchanged.

The main Script.R contains clearly labeled sections for settings, app paths,
packages, exact helper files, data preparation, models, plots and prevalence.
Calculations retain the survey's variable names, response labels, weights and
breakdowns. Changing filenames or settings does not adapt the questionnaire.

The main script also returns fies_result with group/category labels,
moderate-or-severe and severe prevalence proportions, and weighted and
unweighted household-member counts for complete FIES responses.
In the plan's Preparation File, tb_41 <- run_fies_table() creates the
registered extra table without replacing the prepared hh or hl datasets.
