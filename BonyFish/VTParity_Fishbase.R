library("remotes")

remotes::install_version("arkdb", "0.0.10")
remotes::install_github("ropensci/rfishbase")

#For some reason have to re-install rfishbase package everytime
install.packages("rfishbase")

library("rfishbase")
library("devtools")
library("calibrate")
library("reshape")
library("wesanderson")
library("dplyr")
library("writexl")
library("matrixStats")

data(fishbase)

#Need to compile a species list before do anything else because that is how you can query everything else
#Maybe get all species, make dataframe, then delete species with NAs later

#Import family names (Not comprehensive list of all Actinopterygii families, just ones where we were able to confirm fertilization type and egg bearing strategy)
#In a list
families<-read.csv("C:/Users/haileyconrad/Documents/VTParity/FamilyList.csv")
#typeof(families)
#Is a list

#Species list
for(i in families){
  fish<-species_list(Family=i)
}


#####################################################################
#Obsolete code from when I was just trying was of compiling the species list
#Make species list
#Couldn't find a way to do this without manually writing all family names
#out <- sapply(c("Chlamydoselachidae", "Hexanchidae", "Heptranchidae"), function(x) species_list(Family=x))


#Below is a good way of getting all the taxonomic info about the families and the species/species codes, does not produce
#A result that can be used as the species list  in other rfishbase functions though
#fish <- rfishbase::load_taxa()
#myfish <- fish %>%
#  filter(Family %in%  c("Acipenseridae","Polyodontidae","Amiidae","Albulidae","Anguillidae","Muraenidae","Protanguillidae",
#                        "Synaphobranchidae","Heterenchelyidae","Myrocongridae","Chlopsidae","Derichthyidae","Ophichthidae",
#                        "Muraenesocidae","Nettastomatidae","Congridae","Moringuidae","Cyematidae","Monognathidae",
#                        "Saccopharyngidae","Eurypharyngidae","Nemichthyidae","Serrivomeridae","Atherinidae","Melanotaeniidae",
#                        "Atherinopsidae","Notocheiridae","Isonidae","Atherionidae","Dentatherinidae","Phallostethidae",
#                        "Synodontidae","Aulopidae","Pseudotrichonotidae","Paraulopidae","Ipnopidae","Giganturidae",
#                        "Bathysauroididae","Chlorophthalmidae","Notosudidae","Scopelarchidae","Evermannellidae","Sudidae",
#                        "Paralepididae","Alepisauridae","Lestidiidae","Batrachoididae","Adrianichthyidae"))

####################################################################################
#Actual code again

#Maturity data
mat<-maturity(fish,fields=c("SpecCode","Species","AgeMatMin","AgeMatMin2","LengthMatMin","LengthMatMin2"))
#AgeMatMin and AgeMatMin2 give an age range for age at maturity in years
#LenghtMatMin and LengthMatMin2 give length range for length at maturity in cm

fecund<-fecundity(fish,fields=c("SpecCode","Species","FecundityMin","FecundityMax"))
#FecundityMin and FecundityMax give the range of absolute fecundity in number of eggs (clutch size)

age<-popchar(fish,fields=c("SpecCode","Species","tmax"))
#tmax=max age,longevity in years

spawn<-spawning(fish,fields=c("SpecCode","Species","SpawningCycles","LengthOffspringMin","LengthOffspringMax"))
#I'm pretty sure SpawningCycles is the number of clutches per year, sparse data available
#Might want to make the assumption that the answer is 1 unless otherwise specified
#We might want to double check to see how semelparous species are accounted for (salmonids)
#LengthOffspringMin and LengthOffspringMax are offspring size information in cm

#Will eventually need to find a way to combine taxonomic data with other data
#Data needed
  #Class
  #Order
  #Family
  #Species
  #Mass / Body Size (body size more important because we don't have mass for most other things, but if both, take both)
  #Offspring Size / Egg Size
  #Age at Maturity
  #Longevity
  #Clutch Size
  #Breeding Frequency (clutches per year)
  #Reproductive Mode (egg-laying = 0, live-birth = 1)
  #Fertilization Mode (0 = external, 1 = internal)

#Make a dataframe with all the attributes together
dat<-merge(mat,fecund, by=c("SpecCode","Species"), by.mat=by, by.fecund=by,no.dups=TRUE)

dat2<-merge(dat,age,by=c("SpecCode","Species"),by.dat=by,by.age=by,no.dups=TRUE)

dat3<-merge(dat2,spawn,by=c("SpecCode","Species"),by.dat2=by,by.spawn=by,no.dups=TRUE)

#Combine taxonomy data with species attribute data
taxonomy<-load_taxa(fish)

dat4<-merge(dat3,taxonomy,by=c("SpecCode","Species"),by.dat3=by,by.taxonomy=by,no.dups=TRUE)
all_data<-dat4

write.csv(all_data,"C:/Users/haileyconrad/Documents/VTParity/Master_Copy_BonyFish.csv")

#Remove Subfamily
all_data_2<-all_data[,-14]

#Tried to omit NAs, there are no rows with all values

#What to do about more than one entry per species
#Need to do weighted mean
#Get count/frequency data for species
data_withcounts<-all_data_2%>%group_by(Species,SpecCode)%>%mutate(count=n())
head(data_withcounts,20)

#Remove spawning cycles, juvenile size data
#Not enough data, will need to assume 1 cycle/year
#10,11,12
data_withcounts<-data_withcounts[,-12]
data_withcounts<-data_withcounts[,-11]
data_withcounts<-data_withcounts[,-10]

#Write new CSV with counts and missing extraneous columns
write.csv(data_withcounts,"C:/Users/haileyconrad/Documents/VTParity/BonyFish_withCounts.csv")



#Average all column values by species code
#Will also average counts
#Will need to merge taxonomic info back later
#So many NAs, so much ommitted data
#AG1 <- aggregate(SpecCode ~ AgeMatMin + AgeMatMin2 + LengthMatMin+LengthMatMin2+FecundityMin+FecundityMax+tmax+count,
  #              data_withcounts, FUN=c("mean"),na.action = na.pass)
#AG2<-aggregate(AG1[,1:8],list(AG1$SpecCode),mean)
#Only have complete data for 90 SPECIES!!!!!!
#Export to see composition of dataset
#write.csv(AG2,"C:/Users/haileyconrad/Documents/VTParity/BonyFish_specieswithcompleterawdata.csv")


#Need to try averaging Length and Age and Fecundity column data before averaging to see if can minimize loss
#Make NA values blanks
data_withblanks <- sapply(data_withcounts, as.character)
data_withblanks[is.na(data_withblanks)] <- ""

#Need to set things up so that if you have one or the other everything is fine
data_withblanks<-as.data.frame(data_withblanks)

#Add blank column for overall age at maturity
data_withblanks$AgeMat<-""
#Add blank column for overall length at maturity
data_withblanks$LengthMat<-""
#Add blank column for overall annual fecundity
data_withblanks$Fecundity<-""
#Rename tmax column for clarity
names(data_withblanks)[9]<-"Longevity"

#Calculate age at maturity with conditional statements
#Did not work because of mean 
#within(data_withblanks,data_withblanks$AgeMat<-if(data_withblanks$AgeMatMin !="" & data_withblanks$AgeMatMin2 !=""){mean(data_withblanks$AgeMatMin,data_withblanks$AgeMatMin2)
#} else if (data_withblanks$AgeMatMin =="" & data_withblanks$AgeMatMin2 !="") {data_withblanks$AgeMatMin2
#    } else if (data_withblanks$AgeMatMin2 == "" & data_withblanks$AgeMatMin != "") {data_withblanks$AgeMatMin
#    } else {""})

#Trying again
#data_withblanks$AgeMat<-ifelse(data_withblanks$AgeMatMin !="" & data_withblanks$AgeMatMin2 !="", (data_withblanks$AgeMatMin + data_withblanks$AgeMatMin2) / 2,
#ifelse(data_withblanks$AgeMatMin =="" & data_withblanks$AgeMatMin2 !="",data_withblanks$AgeMatMin2,
#ifelse(data_withblanks$AgeMatMin2 == "" & data_withblanks$AgeMatMin != "",data_withblanks$AgeMatMin,"")))

data_withblanks2<-data_withblanks


#Want to use conditional statement to repeat data across columns for categories where missing, then average between columns after
ifelse(data_withblanks2$AgeMatMin =="" & data_withblanks2$AgeMatMin2 !="",data_withblanks2$AgeMatMin2, data_withblanks2$AgeMatMin)
ifelse(data_withblanks2$AgeMatMin2 =="" & data_withblanks2$AgeMatMin !="", data_withblanks2$AgeMatMin, data_withblanks2$AgeMatMin2)

#       ifelse(data_withblanks2$AgeMatMin2 == "" & data_withblanks2$AgeMatMin != "",data_withblanks2$AgeMatMin,""))

#Try converting things to NAs first



#data_withblanks1<-within(data_withblanks, data_withblanks$AgeMatMin<-ifelse(AgeMatMin=="",AgeMatMin2,AgeMatMin))

#data_withblanks2<-within(data_withblanks1,data_withblanks1$AgeMatMin2<-ifelse(AgeMatMin2=="",AgeMatMin,AgeMatMin2))

#Need to calculate length at maturity
#Need to calculate annual fecundity
#Subtract longevity from age at maturity to get #years reproductively active
#Multiple #years reproductively active by annual fecundity to get lifetime fecundity
#Need to merge with family dataset
#Assign fert and repro mode to binary #s

