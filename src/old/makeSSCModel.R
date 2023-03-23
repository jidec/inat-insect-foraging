# return the model
makeSSCModel <- function(sscs,scale=TRUE,diff_threshold=0,response='diff_third1_mean',model_type="lmm",
                         formula="vp + wingspan + daylength + season + (1 | species__) + (1 | grid_id)",
                         title="Plot"){
    library(phyr)
    library(ape)
    library(ggplot2)
    library(stringr)
    library(car)
    library(sjPlot)

    # fix species names (remove possible subsp.)
    sscs$species <- paste(str_split_fixed(sscs$species," ",3)[,1],str_split_fixed(sscs$species," ",3)[,2])

    # get abs diffs
    sscs$diff_abs_total_norm <- sscs$diff_abs_total / sscs$obs_n

    # filter
    sscs <- filter(sscs, diff_abs_total_norm >= diff_threshold)
    print(paste0("Number of SSCs after filtering:",nrow(sscs)))

    # scale all numeric columns
    if(scale){
        sscs <- sscs %>% mutate(across(where(is.numeric), scale))
    }

    # prep tree and trim sscs
    tree <- ape::read.tree("data/misc/bf_species_tree.txt")
    tree$tip.label <- str_split_fixed(tree$tip.label," ",2)[,2]
    tree$tip.label <- str_replace(tree$tip.label,"'","")
    tree <- drop.tip(tree,tree$tip.label[!unique(sscs$species) %in% tree$tip.label])
    length(tree$tip.label)
    sscs <- sscs[sscs$species %in% tree$tip.label,]
    tree$edge.length[which(is.na(tree$edge.length))] <- 0
    sscs$obs <- NULL
    sscs$bl_obs <- NULL
    sscs$season <- as.factor(sscs$season)

    # make LMM or PGLMM
    if(model_type == "lmm"){
        model <- eval(parse(text=paste0("lmer(",response,"~ ",formula,
                                        ",data = sscs)"))) #sscs
        print("VIFS:")
        print(vif(model))
        #print(step(model)) #note to understand how that works backwards regression fit global model
            # not sig mle fits are removed and refit iteratively - reduces selection by removing noncontributing stuff
        plot_model(model)

    }
    if(model_type == "pglmm"){
        model <- eval(parse(text=paste0("pglmm(",response,"~ ",formula,
                                        ",data = sscs, cov_ranef = list(species=tree),bayes=T)"))) #sscs
        plot_bayes(model)
    }

    print(summary(model))

    return(model)
}
