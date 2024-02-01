Zheng et al. (2024) consists of three experiments on human suspicion, using a cards task.
This repository contains all data and code files to perform the analyses presented in the paper.

Each experiment has three data files: 
1. exp<study number>_meta.csv: contains task-level (aggregate) data per participant per study
2. exp<study number>_trials.csv: contains trial-level data with each row a participant's trial information
3. exp<study number>_feedback.csv: contains each participant's raw post-task survey responses

To perform all analyses, install the required libraries (commented out) in utils.R and run the full script.
Then, carefully go through analysis_template.R, which includes all statistical analysis code. Note that
data pre-processing steps differ slightly per experiment. This should be clear from the comments, but
do reach out in case of doubt.
