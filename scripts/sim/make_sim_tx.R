## Simulated election night: TX governor + senate
##
## The generator is make_sim.R, for any state; this keeps the old TX command working.
## Usage (from the repo root):
##   Rscript scripts/sim/make_sim_tx.R [scenario] [seed]
## Checks: Rscript scripts/sim/check_sim.R [scenario]

SIM_STATE = "TX"
source("scripts/sim/make_sim.R")
