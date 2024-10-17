#!/bin/bash

# Activate the 'renv' conda environment
source $(conda info --base)/etc/profile.d/conda.sh
eval "$(conda shell.bash hook)"
conda activate renv

echo "Current Conda environment: $(conda info --envs | grep '*' | awk '{print $1}')"

# Check if the conda environment was activated
if [[ $? -ne 0 ]]; then
  echo "Failed to activate the conda environment 'renv'."
  exit 1
fi

# Run the R script (replace 'your_script.R' with the actual script name)
Rscript automate-teak-sm-tiles-maap.R

# Check if the R script ran successfully
if [[ $? -ne 0 ]]; then
  echo "Failed to run the R script."
  exit 1
fi

echo "R script ran successfully."

# Deactivate the conda environment (optional)
conda deactivate
