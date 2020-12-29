#load in libraries
library(data.table)
library(caret)



#read in data, notice the path will always look like this because the assumed working directory is the repo level folder
train<-fread("./project/volume/data/raw/start_train.csv")
test<-fread("./project/volume/data/raw/start_test.csv")
card_tab<-fread("./project/volume/data/raw/card_tab.csv")
Set_table<-fread("./project/volume/data/raw/set_tab.csv")

#######################
# make a master table #
#######################

# First make train and test the same dim, then bind into one table so you can do the same thing to both datasets

# make a future price column for test, even though it is unknown. We will not use this, this is only to make
# them two tables the same size

test$future_price<-0

#add a column that lets you easily differentiate between train and test rows once they are together
test$train<-0
train$train<-1

#now bind them together

master<-rbind(train,test)


###################
# add in features #
###################

setkey(master,id)
setkey(card_tab,id)



#Not every card has supertype,loyalty,power,toughness and color so we show for the ones that do
#supertype
card_tab$Legendary<-0
card_tab$Legendary[grep("Legendary",card_tab$supertypes)]<-1
#loyalty
card_tab$loyalty<-0
card_tab$loyalty[grep("4",card_tab$loyalty)]<-1
card_tab$loyalty[grep("5",card_tab$loyalty)]<-1
card_tab$loyalty[grep("6",card_tab$loyalty)]<-1
card_tab$loyalty[grep("7",card_tab$loyalty)]<-1
card_tab$loyalty[grep("2",card_tab$loyalty)]<-1
card_tab$loyalty[grep("3",card_tab$loyalty)]<-1
#Power
card_tab$power<-0
card_tab$power[grep("0",card_tab$power)]<-1
card_tab$power[grep("1",card_tab$power)]<-1
card_tab$power[grep("2",card_tab$power)]<-1
card_tab$power[grep("3",card_tab$power)]<-1
card_tab$power[grep("4",card_tab$power)]<-1
card_tab$power[grep("5",card_tab$power)]<-1
card_tab$power[grep("6",card_tab$power)]<-1
card_tab$power[grep("7",card_tab$power)]<-1
card_tab$power[grep("8",card_tab$power)]<-1
card_tab$power[grep("9",card_tab$power)]<-1
card_tab$power[grep("10",card_tab$power)]<-1
card_tab$power[grep("11",card_tab$power)]<-1
card_tab$power[grep("12",card_tab$power)]<-1
card_tab$power[grep("13",card_tab$power)]<-1
card_tab$power[grep("14",card_tab$power)]<-1
card_tab$power[grep("15",card_tab$power)]<-1
card_tab$power[grep("16",card_tab$power)]<-1
card_tab$power[grep("*",card_tab$power)]<-1
#Toughness
card_tab$toughness<-0
card_tab$toughness[grep("0",card_tab$toughness)]<-1
card_tab$toughness[grep("1",card_tab$toughness)]<-1
card_tab$toughness[grep("2",card_tab$toughness)]<-1
card_tab$toughness[grep("3",card_tab$toughness)]<-1
card_tab$toughness[grep("4",card_tab$toughness)]<-1
card_tab$toughness[grep("5",card_tab$toughness)]<-1
card_tab$toughness[grep("6",card_tab$toughness)]<-1
card_tab$toughness[grep("7",card_tab$toughness)]<-1
card_tab$toughness[grep("8",card_tab$toughness)]<-1
card_tab$toughness[grep("9",card_tab$toughness)]<-1
card_tab$toughness[grep("10",card_tab$toughness)]<-1
card_tab$toughness[grep("11",card_tab$toughness)]<-1
card_tab$toughness[grep("12",card_tab$toughness)]<-1
card_tab$toughness[grep("13",card_tab$toughness)]<-1
card_tab$toughness[grep("15",card_tab$toughness)]<-1
card_tab$toughness[grep("16",card_tab$toughness)]<-1
card_tab$toughness[grep("*",card_tab$toughness)]<-1
#colors
card_tab$colors<-0
card_tab$colors[grep("Blue",card_tab$colors)]<-1
card_tab$colors[grep("Red",card_tab$colors)]<-1
card_tab$colors[grep("White",card_tab$colors)]<-1
card_tab$colors[grep("Green",card_tab$colors)]<-1
card_tab$colors[grep("Black",card_tab$colors)]<-1
#subtypes
card_tab$subtypes<-0
card_tab$subtypes[grep("\\.",card_tab$subtypes)]<-1

#only get the base types, then we can see combinations for those base types
types_tab<-as.data.table(tstrsplit(card_tab$types," "))
types_tab$id<-card_tab$id
m_types_tab<-melt(types_tab,id.vars = "id")
m_types_tab<-m_types_tab[!is.na(m_types_tab$value)]
m_types_tab$True<-1
types_tab<-dcast(m_types_tab,id ~ value,length,value.var="True")

#only get base type
type_tab<-as.data.table(tstrsplit(card_tab$type," "))
type_tab$id<-card_tab$id
m_type_tab<-melt(type_tab,id.vars = "id")
m_type_tab<-m_type_tab[!is.na(m_type_tab$value)]
m_type_tab$True<-1
type_tab<-dcast(m_type_tab,id ~ value,length,value.var="True")

#only get base colors
colors_tab<-as.data.table(tstrsplit(card_tab$colors," "))
colors_tab$id<-card_tab$id
colors_types_tab<-melt(colors_tab,id.vars = "id")
colors_types_tab<-colors_types_tab[!is.na(colors_types_tab$value)]
colors_types_tab$True<-1
colors_tab<-dcast(colors_types_tab,id ~ value,length,value.var="True")

#merge everything with master#
master<-merge(master,card_tab[,.(id,rarity,Legendary,loyalty,power,toughness,colors)],all.x=T)
master<-merge(master,types_tab,all.x=T)
master<-merge(master,type_tab,all.x=T)
master<-merge(master,colors_tab,all.x = T)
master$current_price[is.na(master$current_price)]<-mean(master$current_price,na.rm=T)

master[is.na(master)]<-0

#####################
# add in BS columns #
#####################

BS_data<-replicate(500,sample(c(1,0),nrow(master),replace=T))
BS_data<-data.table(BS_data)

setnames(BS_data,paste0("V",1:500),paste0("BS_",1:500))

master<-cbind(master,BS_data)

# split back to train/test #
train<-master[train==1]
test<-master[train==0]

# clean up columns
train$train<-NULL
test$train<-NULL
test$future_price<-NULL


# write out to interim #
fwrite(train,"./project/volume/data/interim/train_v1.csv")
fwrite(test,"./project/volume/data/interim/test_v1.csv")

