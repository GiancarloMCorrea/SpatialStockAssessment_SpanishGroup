
## Bio trend from base model ---------------------------------------------------

library(r4ss)
run <- 'base model CPUE st 1A' ## *CHANGE name
mod_path <- paste0(getwd(), "/model/", run, sep="") 
dir.create(mod_path) ## check that exists

replist <- SS_output(dir = mod_path, verbose=TRUE, printstats=TRUE) ## read


## Maturity at length
## Length increments in population length bins (distance between bins)
increments <- replist$lbinspop[-1]-replist$lbinspop[-replist$nlbinspop] 
increments <- c(increments, increments[length(increments)])

## Length at mid-point of population length bins
len <- replist$lbinspop + increments/2
matslope <- as.numeric(replist$MGparmAdj$"Mat_slope_Fem")[1]
matl50 <- as.numeric(replist$MGparmAdj$"Mat50%_Fem")[1]  
matlen <- 1/( 1 + exp(matslope*(len - matl50 )) ) # Ogiva: % matures by length

## Weight at length 
wlena <- as.numeric(replist$MGparmAdj$Wtlen_1_Fem)[1] 
wlenb <- as.numeric(replist$MGparmAdj$Wtlen_2_Fem)[1] 
wlen <- wlena*(len^wlenb) # Weight at length for each one of the lengths 

library(dplyr)
natlen<-replist$natlen
names(natlen)
natlen<-natlen %>% filter(Sex==1, `Beg/Mid`=="B", Seas==1)
library(plyr)
vec=colnames(natlen)
ind=which(vec=="10"); lvec=length(vec)
vec=vec[ind:lvec]

N=unique(natlen$Yr)
for (i in 1:length(vec)){
  
  aux=aggregate(natlen[,ind+i-1], by=list(Category=natlen$Yr), FUN=sum)
  
  N=cbind(N,aux[,2])
}
colnames(N)=c("Yr",vec)

## Total
laux=length(unique(natlen$Yr))
year=unique(natlen$Yr)[1:(laux-3)] # 3 forecast years
l_l=length(unique(wlen))
l_y=length((year))
SSB=matrix(0,ncol=l_l,nrow=l_y)
B=matrix(0,ncol=l_l,nrow=l_y)
for (i in 1:l_y){
  for (j in 1:l_l){
    SSB[i,j]=N[i,j+1]*wlen[j]*matlen[j]
    B[i,j]=N[i,j+1]*wlen[j]
  }
}

ssb_t=apply(SSB, 1,sum)
b_t=apply(B, 1,sum)

bio<-cbind(b_t, year)
bio<-as.data.frame(bio)
colnames(bio)<-c("bio","year")

plot(bio$bio~bio$year, type="l")

save(bio, file="bio trend base model CPUE st 1A.RData")


# Plot bio ---------------------------------------------------------------------

# Read data
load("D:/Tesis/C2) SS spatial/SPiCT plot/spict.RData") # SPICT trend
load("D:/Tesis/C2) SS spatial/SPiCT plot/bio trend base model CPUE st 1A.RData")
bio1A<-bio
load("D:/Tesis/C2) SS spatial/SPiCT plot/bio trend base model CPUE st 4A.RData")
bio4A<-bio

head(dat)


library(dplyr)
data<-dat%>%filter(Model=="SPiCT year besag")
colnames(data)<-c("year", "bio", "Model")
data$Model<-rep("SPiCT")


bio1A<-bio1A%>%filter(year>1971)
bio1A$Model<-rep("SS 1Ay")
bio1A<-bio1A[,c(2,1,3)]

bio4A<-bio4A%>%filter(year>1971)
bio4A$Model<-rep("SS 4Ay")
bio4A<-bio4A[,c(2,1,3)]

data_all<-rbind(data, bio1A, bio4A )

library(ggplot2)
ggplot(data = data_all, aes(x = year, y = bio, col=Model)) +
  geom_line(size=0.9)+
  scale_color_manual(values = c("orange","dodgerblue3","black", "grey"))+
  ylab("Biomass")+
  xlab("Year")+
  theme_light() 

ggsave("SPiCT and BIO plot.png", dpi=300, height = 3, width = 6)
