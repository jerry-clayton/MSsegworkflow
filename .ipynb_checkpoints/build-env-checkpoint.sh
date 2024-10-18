#!/usr/bin/env -S bash --login
##!/bin/bash

set -euo pipefail

basedir=$( cd "$(dirname "$0")" ; pwd -P)
echo "Creating conda environment from ${basedir}/ms-env.yml"
conda env create --name "ms-env" -f ${basedir}/ms-env.yml
