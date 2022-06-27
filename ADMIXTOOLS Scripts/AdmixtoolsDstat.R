setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
###   Chanfe dara based on ABBA BABA test you want to do
data = 'eigenstratallcov5'
### Change outfile name
outfile = 'Dstatintpolmexicana.txt'
### Change popfile based on populations you want to test
popfile = 'dstathuehuepoplist.txt'
library(data.table)
library(admixtools)
library(tidyverse)

dstat = f4(data, pop1 = popfile,  f4mode = FALSE)

write_tsv(dstat, outfile)

### For testing mexicana introgression for specified mexicana pop against all populations
out="DstatMexicanaChalcolowAllsnps.svg"
svg(out)

ggplot(dstat, aes(fct_reorder(pop1, est), est, color = abs(z) > 2)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_errorbar(aes(ymin = est - 2 * se, ymax = est + 2 * se)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("D") + 
  xlab("Population") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Zea mays ssp. mexicana - Huehue")

dev.off()

### For testing all mexican pop introgressions against all maize
out="Dstatmaizemexicana.svg"
svg(out)
ggplot(dstat, aes(fct_reorder(pop2, est), est, color = abs(z) > 2)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_errorbar(aes(ymin = est - 2 * se, ymax = est + 2 * se)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("D") + 
  scale_x_discrete(labels=c("Zeamaysssp.mexicana-Chalco" = "Chalco", "Zeamaysssp.parviglumis-Huehuetenango" = "Huehuetenango",
                                "Zeamaysssp.mexicana-CentralPlateau" = "Central Plateau", "Zeamaysssp.mexicana-NorthMexico" = "North Mexico")) +
  xlab("Population") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("All Maize")
dev.off()
