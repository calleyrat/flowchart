


## Chargement des packages nécessaires -----------------------------------------

library(tidyverse)

## Packages pour le flowchart :
library(Gmisc) ##Fonctions boxGrob, spreadHorizontal, alignVertical
library(glue) ##Fonction glue
library(grid) ##Fonction grid.newpage


## Vignette du package Gmisc pour un flowchart :
## https://cran.r-project.org/web/packages/Gmisc/vignettes/Grid-based_flowcharts.html





#*******************************************************************************
# 2. CREATION DES DONNEES -----
#*******************************************************************************

Npat <- 82
NotINCL <- 2
screened <- data.frame(SUBJID = 1:(Npat+NotINCL), GPRND = c(rep("VR", Npat/2), rep("CTRL", Npat/2), rep(NA,NotINCL)))

globale <- screened[!is.na(screened$GPRND),]
globale$VRREMOVAL <- ifelse(!is.na(globale$GPRND) & globale$GPRND=="VR" & globale$SUBJID %in% c(3,7,9), 1, 0)

myseed <- 45678
globale$PHONECONTACT <- ifelse(runif(dim(globale)[1], 0, 1) > 0.90, 0, 1)
globale$CJP_EVAL <- ifelse(runif(dim(globale)[1], 0, 1) > 0.08, 1, 0)

globale$ELIGIBLE <- ifelse(!is.na(globale$GPRND) & globale$GPRND=="CTRL" & globale$SUBJID %in% c(54,56), 0, 1)

### RESTRICTION A L'ECHANTILLON D'ANALYSE 

anadat <- globale[!is.na(globale$GPRND) & globale$CJP_EVAL==1 & globale$ELIGIBLE==1,]



################# CATEGORIES SELON CONSORT

## Recommandations CONSORT :
## https://www.equator-network.org/reporting-guidelines/consort/

## Bibliographie :
## Moher D, Hopewell S, Schulz KF, Montori V, Gøtzsche PC, Devereaux PJ, Elbourne D, Egger M, Altman DG. 
## CONSORT 2010 explanation and elaboration: updated guidelines for reporting parallel group randomised trials. 
## BMJ. 2010 Mar 23;340:c869. doi: 10.1136/bmj.c869


# ENROLMENT 
# - Assessed for eligibility
# ==> Excluded (not meeting inclusion criteria, declined to participate...)
# - Randomised
# 
# ALLOCATION 
# - Allocated to intervention 
# - Received allocated intervention, did not receive
# 
# FOLLOW-UP 
# - Lost to follow-up (give reasons)
# - discontinued intervention (give reasons)
# 
# ANALYSIS 
# - Analysed (excluded from analysis - give reasons)




#*******************************************************************************
# 3. DEFINITION DES BOXES DU FLOWCHART -----
#*******************************************************************************

############## ENROLMENT

## Eligibility
total <- boxGrob(glue("Assessed for eligibility (n = {pop})",
                      pop = txtInt(dim(screened)[1])),
                 txt_gp = gpar(cex = 0.7), box_gp = gpar(fill = "gray90"),
                 width = 0.9)

not_randomised <- boxGrob(glue("Did not provide consent (n = {pop})",
                               pop = txtInt(dim(screened[is.na(screened$GPRND),])[1])),
                          txt_gp = gpar(cex = 0.7), box_gp = gpar(fill = "gray90"),
                          just = "left", width = 0.3)


## Randomised 
randomised <- boxGrob(glue("Randomised (n = {pop})",
                           pop = txtInt(dim(globale)[1])),
                      txt_gp = gpar(cex = 0.7), box_gp = gpar(fill = "gray90"),
                      width = 0.9)


############## ALLOCATION 

## Allocated to intervention 
RND_VR <- dim(globale[globale$GPRND=="VR",])[1]
RND_CTRL <- dim(globale[globale$GPRND=="CTRL",])[1]

## Received allocated treatment ?
REMOV_VR <- dim(globale[globale$GPRND=="VR" & globale$VRREMOVAL==1,])[1]


randomised_VR <- boxGrob(glue("Allocated to virtual reality (n = {pop})",
                              " - VR headset removal before the end of the exam (n={pop_n})",
                              pop = txtInt(RND_VR),
                              pop_n = txtInt(REMOV_VR),
                              .sep = "\n"),
                         txt_gp = gpar(cex = 0.7), box_gp = gpar(fill = "gray90"),
                         just = "left", width = 0.43)
randomised_CTRL <- boxGrob(glue("Allocated to usual care (n = {pop})",
                                pop = txtInt(RND_CTRL),
                                .sep = "\n"),
                           txt_gp = gpar(cex = 0.7), box_gp = gpar(fill = "gray90"),
                           just = "left", width = 0.43)

rm(RND_VR,RND_CTRL,REMOV_CTRL)


############## FOLLOW-UP

## No phone contact
lost_VR <- dim(globale[globale$GPRND=="VR" & globale$PHONECONTACT==0,])[1]
lost_CTRL <- dim(globale[globale$GPRND=="CTRL" & globale$PHONECONTACT==0,])[1]


no_phone_VR <- boxGrob(glue("Lost to follow-up : no phone contact (n={pop})",
                            pop = txtInt(lost_VR)),
                       txt_gp = gpar(cex = 0.7), box_gp = gpar(fill = "gray90"),
                       just = "left", width = 0.43)
no_phone_CTRL <- boxGrob(glue("Lost to follow-up : no phone contact (n={pop})",
                              pop = txtInt(lost_CTRL)),
                         txt_gp = gpar(cex = 0.7), box_gp = gpar(fill = "gray90"),
                         just = "left", width = 0.43)

rm(lost_VR,lost_CTRL)


############## ANALYSIS

ANDO_VR <- dim(anadat[anadat$GPRND=="VR",])[1] 
ANDO_CTRL <- dim(anadat[anadat$GPRND=="CTRL",])[1] 

## Wrongly included (Biblio : Fergusson et al., Post-randomisation exclusions: the intention to treat principle and 
## excluding patients from analysis, 2002)

NELIG_CTRL <- dim(globale[globale$GPRND=="CTRL" & globale$ELIGIBLE==0,])[1]

CJP_VR <- dim(globale[globale$GPRND=="VR" & globale$CJP_EVAL==0,])[1]
CJP_CTRL <- dim(globale[globale$GPRND=="CTRL" & globale$CJP_EVAL==0,])[1]



ana_VR <- boxGrob(glue("Analysed with modified intention-to-treat (n={ando})",
                       "Post-randomisation exclusions (n={exc})",
                       "- primary outcome not evaluated ({cjp})",
                       ando = txtInt(ANDO_VR),
                       exc = txtInt(CJP_VR),
                       cjp = txtInt(CJP_VR),
                       .sep = "\n"),
                  txt_gp = gpar(cex = 0.7), box_gp = gpar(fill = "gray90"),
                  just = "left", width = 0.43)
ana_CTRL <- boxGrob(glue("Analysed with modified intention-to-treat (n={ando})",
                         "Post-randomisation exclusions (n={exc})",
                         "- wrongly included ({wrong})",
                         "- primary outcome not evaluated ({cjp})",
                         ando = txtInt(ANDO_CTRL),
                         exc = txtInt(CJP_CTRL + NELIG_CTRL),
                         wrong = txtInt(NELIG_CTRL),
                         cjp = txtInt(CJP_CTRL),
                         .sep = "\n"),
                    txt_gp = gpar(cex = 0.7), box_gp = gpar(fill = "gray90"),
                    just = "left", width = 0.43)


rm(ANDO_VR,ANDO_CTRL,NELIG_CTRL,CJP_VR,CJP_CTRL)



#*******************************************************************************
# 4. EDITION DU FLOWCHART -----
#*******************************************************************************

############################# Tout exécuter en même temps !!

grid.newpage()

##### Alignement des box

## Trame globale (dispersion verticale équilibrée) :
vert <- spreadVertical(total = total,
                       not_rnd = not_randomised,
                       randomised = randomised,
                       rnd_grps = randomised_VR,
                       follow = no_phone_VR,
                       ando_grps = ana_VR,
                       .from = 0.97, .to = 0.08,
                       .type = "between")

vert$not_rnd <- moveBox(vert$not_rnd, x = .80) ##je décale à droite


## Séparation entre les deux groupes :
rnd_grps <- alignVertical(reference = vert$rnd_grps,
                          randomised_VR, randomised_CTRL,
                          .position = "top") %>%
  spreadHorizontal(.from = 0.05, .to = 0.95)
vert$rnd_grps <- NULL


follow <- alignVertical(reference = vert$follow,
                        no_phone_VR, no_phone_CTRL) %>%
  spreadHorizontal(.from = 0.05, .to = 0.95)
vert$follow <- NULL


ando_grps <- alignVertical(reference = vert$ando_grps,
                           ana_VR, ana_CTRL,
                           .position = "top") %>%
  spreadHorizontal(.from = 0.05, .to = 0.95)
vert$ando_grps <- NULL



##### Print boxes

vert
rnd_grps
follow
ando_grps


##### Connections entre les box

## ENROLMENT
connectGrob(vert$total, vert$randomised, type = "vertical", 
            arrow_obj = arrow(length = unit(0.1,"inches"), type = "closed"))
connectGrob(vert$total, vert$not_rnd, type = "L", 
            arrow_obj = arrow(length = unit(0.1,"inches"), type = "closed"))


## ALLOCATION
connectGrob(vert$randomised, rnd_grps[[1]], type = "N", 
            arrow_obj = arrow(length = unit(0.1,"inches"), type = "closed"))
connectGrob(vert$randomised, rnd_grps[[2]], type = "N", 
            arrow_obj = arrow(length = unit(0.1,"inches"), type = "closed"))


## FOLLOW-UP

connectGrob(rnd_grps[[1]], follow[[1]], type = "vertical", 
            arrow_obj = arrow(length = unit(0.1,"inches"), type = "closed"))
connectGrob(rnd_grps[[2]], follow[[2]], type = "vertical", 
            arrow_obj = arrow(length = unit(0.1,"inches"), type = "closed"))


## ANALYSIS

connectGrob(follow[[1]], ando_grps[[1]], type = "vertical", 
            arrow_obj = arrow(length = unit(0.1,"inches"), type = "closed"))
connectGrob(follow[[2]], ando_grps[[2]], type = "vertical", 
            arrow_obj = arrow(length = unit(0.1,"inches"), type = "closed"))

############################# FIN -- EDITION DU FLOWCHART


