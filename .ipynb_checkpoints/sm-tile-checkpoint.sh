#!/bin/bash
# Activate the 'ms-env' conda environment
basedir=$( cd "$(dirname "$0")" ; pwd -P)

source $(conda info --base)/etc/profile.d/conda.sh
eval "$(conda shell.bash hook)"
conda activate ms-env

echo "Current Conda environment: $(conda info --envs | grep '*' | awk '{print $1}')"

# Check if the conda environment was activated
if [[ $? -ne 0 ]]; then
  echo "Failed to activate the conda environment 'renv'."
  exit 1
fi

mkdir -p output
find . > output/find.txt
find ${basedir}
find . -type f > output/findf.txt

# Run script to install r-only packages
Rscript --verbose ${basedir}/run/create-ms-env.R

# Run the R script
Rscript --verbose ${basedir}/run/automate-teak-sm-tiles-maap.R

# Check if the R script ran successfully
if [[ $? -ne 0 ]]; then
  echo "Failed to run the R script."
  exit 1
fi

echo "R script ran succedssfully."

# Deactivate the conda environment (optional)
conda deactivate
