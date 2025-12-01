#!/bin/bash -l

#SBATCH --job-name=lcr_job
#SBATCH --time=0-20:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=250G
#SBATCH --array=1-10
#SBATCH --mail-user=ftsiboe@hotmail.com
#SBATCH --mail-type=ALL
#SBATCH --output=/dev/null

module purge
module load R/4.2.1-foss-2022a GDAL/3.5.0-foss-2022a PROJ/9.0.0-GCCcore-11.3.0
R --no-save -q < replications/loss_cost_ratio_growth/estimate_empirical_loss_cost_ratio_growth.R
