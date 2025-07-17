

# MAP ALIEN FLORA OF ARMENIA IN A PHYLOGENY


source('scripts/2_taxonomic backbone.R')
# devtools::install_github("jinyizju/V.PhyloMaker2")
library(V.PhyloMaker2)
# https://github.com/jinyizju/Supplementary-files-of-the-V.PhyloMaker2-paper


# check base csv for V.PhyloMaker2 functions
read.csv("data/sample_species_list.csv") %>% head()

# prepare file, use accepted names according to gbif 
species_phylocsv <- species_status[,c('species_gbif','genus','family')]
names(species_phylocsv)[names(species_phylocsv)=='species_gbif'] <- 'species'
species_phylocsv$species.relative <- NA
species_phylocsv$genus.relative <- NA

# fix some plant family names to run phylo.maker
species_phylocsv$family[which(species_phylocsv$genus=='Viburnum')] <- 'Adoxaceae'
species_phylocsv$family[which(species_status$genus=='Sambucus')] <- 'Adoxaceae'
species_phylocsv$family[which(species_status$family=='Ehretiaceae')] <- 'Boraginaceae'

# check families against V.PhyloMaker2 backbone
table(species_phylocsv$family %in% deframe(read.csv("data/family_list_for_V.PhyloMaker2.csv")))
species_phylocsv$family[!(species_phylocsv$family %in% deframe(read.csv("data/family_list_for_V.PhyloMaker2.csv")))]

# remove Cephalotaxaceae
species_phylocsv <- species_phylocsv[-which(species_phylocsv$family=='Cephalotaxaceae'),]

# generate a phylogeny for the sample species list
# tree <- phylo.maker(sp.list=species_phylocsv, tree = GBOTB.extended.TPL, nodes=nodes.info.1.TPL, scenarios="S3")
# save(tree, file='results/tree.Rdata')
# write.tree(tree$scenario.3, file="results/tree.tre")

nrow(tree$species.list)
table(tree$species.list$status)

class(tree$scenario.3)
# x11()
# plot(tree$scenario.3)

# the tree has 1042 tips because gbif considered several species as synonyms. all species are included
table(gsub(" ", "_", species_status$species_gbif) %in% tree$scenario.3$tip.label)



# create annotation file
annotation_file <- species_status[,c('species_gbif','status')]
annotation_file$species_gbif <- gsub(" ", "_", annotation_file$species_gbif)

# plot
plant.tree2 <- tree$scenario.3
geo <- factor(annotation_file$status[match(plant.tree2$tip.label, annotation_file$ species_gbif)]) # colors
mycol <- c('coral1','lightgreen','gold')[geo]
par(mar=c(0,0,0,0))
plot(plant.tree2,  tip.color=mycol, cex=0.35, type='fan')
legend(-130, 190,
       legend=c("Invasive","Watch","Ornamental"),
       fill=c('coral1','gold','lightgreen'),
       col = par("col"), bty = "n", bg = par("bg"), cex = 1, horiz = FALSE, title = ' ')


