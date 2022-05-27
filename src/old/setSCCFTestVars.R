# set variables for testing baseline diff

data = usa_bfs
bl_data = usa_insects
cellsize_miles = 155
weib_iters = 30
weib_ci_iters=10
weib_ci_bootstraps=10
ncpus=1
use_genus=FALSE
simple=TRUE
first_n=300
min_ants_n=50
sample_cutoff_n=150 # the max number of observations to use when computing weib percentiles
floor_hours=FALSE
target_species=NULL
print=FALSE
daymet_manova=FALSE
kl_bootstraps=50
