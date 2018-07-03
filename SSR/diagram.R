#source("http://bioconductor.org/biocLite.R")
#biocLite("rhdf5")
library(tcltk) # for interactive file choosing
library(rhdf5) # required for H5 files


pathToSet <- tk_choose.dir(default = "", caption = "Select directory")

# set a hardcoded Path to the MillionSongSubset
# pathToSet = '/Users/Kostja/Desktop/Master/Sem 2 (18 SoSe)/Data Visualization/Tasks/MillionSongSubset'

# create array with found Ids in beforehand containing prefered songs
TrackIDs <- array(c('TRAPZTV128F92CAA4E','TRANNZZ128F92C22F7','TRAQZQX128F931338F','TRALONM128EF35A199','TRAWBHE12903CBC4CB'))

# find automaticaly all paths with names of trackIDs
SubPaths <- lapply(TrackIDs,function(x){
  list.files(pathToSet, x, recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
})

# beautify the dataset 
SubPaths <- data.frame(SubPaths = t(unlist(SubPaths)))
names(SubPaths) <- c('beyonce', 'justin', 'kanye', 'madonna', 'bruno')



# read the H5 files and create a readable output
artist <- lapply(SubPaths, function(x){
  h5ls(toString(x))
})


#data from analysis song
Analyze_song <- apply(SubPaths,2,function(x){
  h5read(x,"/analysis/songs")
})
Analyze_song <- do.call(rbind, Analyze_song)

#data from meta song
Meta_song <- apply(SubPaths,2,function(x){
  h5read(x,"/metadata/songs")
})
Meta_song <- do.call(rbind, Meta_song)


#radar chart
library(fmsb)

radarFrame <- function(df1, df2){
  matrix <- cbind('artist_familiarity' = df1$artist_familiarity, 'artist_hotttnesss' = df1$artist_hotttnesss, 'tempo'= df2$tempo, 'key' = df2$key, 'mode' = df2$mode) 
  rownames(matrix) <- rownames(df1)
  matrix <- data.frame(matrix)
}

namesLegend <- paste(Meta_song$artist_name,Meta_song$title)

radar <- function(df, namesLeg = namesLegend, x = -2.8 , y= -1.1){
  transparency <- adjustcolor(1:dim(df)[1], alpha.f = 0.2) 
  # Custom the radarChart !
  radarchart( df  , axistype=1 , maxmin = FALSE,
              #custom polygon
              pcol=1:dim(df)[1], plwd=1 , pfcol = transparency ,
              #custom the grid
              cglcol="grey", cglty=1, axislabcol=FALSE ,
              #custom labels
              vlcex=0.8 
  )
  par(xpd=TRUE)
  legend(x,y, legend = namesLeg, bty = "n", pch=20 , col=1:dim(df)[1] , cex=0.8, pt.cex=2)
}

#create radar chart from MSD
data <- radarFrame(Meta_song, Analyze_song)

radar(data)
par(mar=c(4,4,4,4))

#create radar chart from tunebat
compareFrame <- data.frame(rbind(
  beyonce = c('tempo' = 97, 'popularity' = 70, 'energy'=58, 'dancebility'=43, 'happiness'=27),
  justin = c('tempo' = 76, 'popularity' = 70, 'energy'=72, 'dancebility'=69, 'happiness'=43),
  kanye = c('tempo' = 106, 'popularity' = 65, 'energy'=85, 'dancebility'=60, 'happiness'=55),
  madonna = c('tempo' = 130, 'popularity' = 16, 'energy'=78, 'dancebility'=62, 'happiness'=64),
  bruno = c('tempo' = 104, 'popularity' = 70, 'energy'=85, 'dancebility'=69, 'happiness'=74)
))

# because all timesignatuires are 4, there is no proper graph 
# radarchart draws relatively
radar(compareFrame)
par(mar=c(1,1,1,1))

#data from artist term
#h5read(toString(SubPaths$beyonce[[1]]),"/metadata/artist_terms")
#h5read(toString(SubPaths$beyonce[[1]]),"/metadata/artist_terms_freq")
#h5read(toString(SubPaths$beyonce[[1]]),"/metadata/artist_terms_weight")
#h5read(toString(SubPaths$beyonce[[1]]),"/musicbrainz/artist_mbtags")
#h5read(toString(SubPaths$beyonce[[1]]),"/musicbrainz/songs")
#h5ls(toString(SubPaths$beyonce[[1]]))
#h5ls(toString(SubPaths$bruno[[1]]))
#H5close()

mb_tags <- apply(SubPaths,2,function(x){
  h5read(x,"/musicbrainz/artist_mbtags")
})


#create barchart MSD
par(las=2) # make label text perpendicular to axis
par(mar=c(5,5,4,2)) # increase y-axis margin.

barplot(Meta_song$artist_familiarity, main="familiarity", col =1:5, horiz=TRUE, names.arg=names(SubPaths), cex.names=0.8)

barplot(Meta_song$artist_hotttnesss, main="hotness", col =1:5, horiz=TRUE, names.arg=names(SubPaths), cex.names=0.8)

barplot(Analyze_song$tempo, main="tempo", col =1:5, horiz=TRUE, names.arg=names(SubPaths), cex.names=0.8)

barplot(Analyze_song$key, main="key", col =1:5, horiz=TRUE, names.arg=names(SubPaths), cex.names=0.8)

barplot(Analyze_song$mode, main="mode", col =1:5, horiz=TRUE, names.arg=names(SubPaths), cex.names=0.8)


#create barchart tunebat
par(las=2) # make label text perpendicular to axis
par(mar=c(5,5,4,2)) # increase y-axis margin.

barplot(compareFrame$tempo, main="tempo", col =1:5, horiz=TRUE, names.arg=names(SubPaths), cex.names=0.8)

barplot(compareFrame$popularity, main="polularity", col =1:5, horiz=TRUE, names.arg=names(SubPaths), cex.names=0.8)

barplot(compareFrame$energy, main="energy", col =1:5, horiz=TRUE, names.arg=names(SubPaths), cex.names=0.8)

barplot(compareFrame$dancebility, main="dancebility", col =1:5, horiz=TRUE, names.arg=names(SubPaths), cex.names=0.8)

barplot(compareFrame$happiness, main="happiness", col =1:5, horiz=TRUE, names.arg=names(SubPaths), cex.names=0.8)


########################################################
### recommended songs ################################## 
########################################################

# create array with found Ids in beforehand containing prefered songs
R_TrackIDs <- array(c('TRAZWGK128F93141E3','TRBDOVF128E0795641','TRAAKDG128F42A0ECB'))

# find automaticaly all paths with names of trackIDs
R_SubPaths <- lapply(R_TrackIDs,function(x){
  list.files(pathToSet, x, recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
})

# beautify the dataset 
R_SubPaths <- data.frame(SubPaths = t(unlist(R_SubPaths)))
names(R_SubPaths) <- c('Lady Gaga', 'Sean Paul', 'Shakira')



# read the H5 files and create a readable output
R_artist <- lapply(R_SubPaths, function(x){
  h5ls(toString(x))
})


#data from analysis song
R_Analyze_song <- apply(R_SubPaths,2,function(x){
  h5read(x,"/analysis/songs")
})
R_Analyze_song <- do.call(rbind, R_Analyze_song)

#data from meta song
R_Meta_song <- apply(R_SubPaths,2,function(x){
  h5read(x,"/metadata/songs")
})
R_Meta_song <- do.call(rbind, R_Meta_song)

#data from artist term
R_mb_tags <- apply(R_SubPaths,2,function(x){
  h5read(x,"/musicbrainz/artist_mbtags")
})

#create radar for msd
P_songs <- cbind('artist_familiarity' =  mean(Meta_song$artist_familiarity), 'artist_hotttnesss' = mean(Meta_song$artist_hotttnesss), 'tempo' = mean(Analyze_song$tempo))
R_songs <- cbind('artist_familiarity' = R_Meta_song$artist_familiarity, 'artist_hotttnesss' = R_Meta_song$artist_hotttnesss, 'tempo' = R_Analyze_song$tempo)
all_songs <- rbind(P_songs,R_songs)
MSD_R <- data.frame(all_songs, row.names = c("p_avg",row.names(R_Meta_song)[1:3]))

radar(MSD_R, namesLeg=row.names(MSD_R), -1,-1)
radar(MSD_R, row.names(MSD_R))
par(mar=c(1,1,1,1))

#barchart msd
par(las=2) # make label text perpendicular to axis
par(mar=c(5,5,4,2)) # increase y-axis margin.

barplot(MSD_R$artist_familiarity, main="familiarity", col =1:5, horiz=TRUE, names.arg=row.names(MSD_R), cex.names=0.8)

barplot(MSD_R$artist_hotttnesss, main="artist hotness", col =1:5, horiz=TRUE, names.arg=row.names(MSD_R), cex.names=0.8)

barplot(MSD_R$tempo, main="tempo", col =1:5, horiz=TRUE, names.arg=row.names(MSD_R), cex.names=0.8)

barplot(R_Analyze_song$key, main="key", col =1:5, horiz=TRUE, names.arg=names(R_SubPaths), cex.names=0.8)

barplot(R_Analyze_song$mode, main="mode", col =1:5, horiz=TRUE, names.arg=names(R_SubPaths), cex.names=0.8)


#boxplot msd
par(xpd=FALSE)
boxplot(Meta_song$artist_familiarity, las = 2,  names = c("p_avg"), main="familiarity", ylim=c(0.7,1))
abline(h=R_Meta_song$artist_familiarity, col =2:4)
axis(1, labels=c("playlist"), at=1, las=1)

par(xpd=FALSE)
boxplot(Meta_song$artist_hotttnesss, las = 2,  names = c("p_avg"), main="artist hotness", ylim=c(0.5,0.9))
abline(h=R_Meta_song$artist_hotttnesss, col =2:4)
axis(1, labels=c("playlist"), at=1, las=1)

par(xpd=FALSE)
boxplot(Analyze_song$tempo, las = 2,  names = c("p_avg"), main="tempo")
abline(h=R_Analyze_song$tempo, col =2:4)
axis(1, labels=c("playlist"), at=1, las=1)


#create radar for tunebat
compare_avg <- apply(compareFrame,2,mean)

R_compare_songs <- rbind(
  Gaga = c('tempo' = 119, 'popularity' = 68, 'energy'=81, 'dancebility'=85, 'happiness'=83),
  Paul = c('tempo' = 91, 'popularity' = 66, 'energy'=49, 'dancebility'=65, 'happiness'=79),
  Shakira = c('tempo' = 100, 'popularity' = 83, 'energy'=82, 'dancebility'=78, 'happiness'=76)
)

R_compareFrame_ori <- data.frame(R_compare_songs)

R_compare_songs_mix <- rbind(
  playlist_avg = compare_avg,
  R_compare_songs
)

R_compareFrame <- data.frame(R_compare_songs_mix)

radar(R_compareFrame, namesLeg=row.names(R_compareFrame), -1,-1)
radar(R_compareFrame, row.names(R_compareFrame))
par(mar=c(1,1,1,1))

#barchart tunebat

par(las=2) # make label text perpendicular to axis
par(mar=c(5,5,4,2)) # increase y-axis margin.

barplot(R_compareFrame$tempo, main="tempo", col =1:5, horiz=TRUE, names.arg=row.names(MSD_R), cex.names=0.8)

barplot(R_compareFrame$popularity, main="polularity", col =1:5, horiz=TRUE, names.arg=row.names(MSD_R), cex.names=0.8)

barplot(R_compareFrame$energy, main="energy", col =1:5, horiz=TRUE, names.arg=row.names(MSD_R), cex.names=0.8)

barplot(R_compareFrame$dancebility, main="dancebility", col =1:5, horiz=TRUE, names.arg=row.names(MSD_R), cex.names=0.8)

barplot(R_compareFrame$happiness, main="happiness", col =1:5, horiz=TRUE, names.arg=row.names(MSD_R), cex.names=0.8)


#boxplot tunebat
par(xpd=FALSE)
boxplot(compareFrame$tempo, las = 2,  names = c("p_avg"), main="tempo")
abline(h=R_compareFrame_ori$tempo, col =2:4)
axis(1, labels=c("playlist"), at=1, las=1)

par(xpd=FALSE)
boxplot(compareFrame$popularity, las = 2,  names = c("p_avg"), main="polularity", ylim=c(60,90))
abline(h=R_compareFrame_ori$popularity, col =2:4)
axis(1, labels=c("playlist"), at=1, las=1)

par(xpd=FALSE)
boxplot(compareFrame$energy, las = 2,  names = c("p_avg"), main="energy", ylim=c(35,90))
abline(h=R_compareFrame_ori$energy, col =2:4)
axis(1, labels=c("playlist"), at=1, las=1)

par(xpd=FALSE)
boxplot(compareFrame$dancebility, las = 2,  names = c("p_avg"), main="dancebility", ylim=c(55,90))
abline(h=R_compareFrame_ori$dancebility, col =2:4)
axis(1, labels=c("playlist"), at=1, las=1)

par(xpd=FALSE)
boxplot(compareFrame$happiness, las = 2,  names = c("p_avg"), main="happiness", ylim=c(20,90))
abline(h=R_compareFrame_ori$happiness, col =2:4)
axis(1, labels=c("playlist"), at=1, las=1)



########################################################
### PCA ################################################
########################################################

#playlist
#plot timbre pca
track_timbre <- apply(SubPaths,2,function(x){
  h5read(x,"/analysis/segments_timbre")
})

mean_timbre <- NULL
for(i in names(track_timbre)){
  res <- apply(track_timbre[[i]],1,mean)
  mean_timbre <- rbind(mean_timbre,res)
}
dim(mean_timbre)
par(mar=c(5,5,4,2))
row.names(mean_timbre) <- names(track_timbre)
plot(mean_timbre[,1], mean_timbre[,2], ylim=c(-50,50), xlim=c(35,50))
text(mean_timbre[,1], mean_timbre[,2], labels=row.names(mean_timbre), cex= 0.7, pos=3)

#timbre recommended song
R_track_timbre <- apply(R_SubPaths,2,function(x){
  h5read(x,"/analysis/segments_timbre")
})

R_mean_timbre <- NULL
for(i in names(R_track_timbre)){
  res <- apply(R_track_timbre[[i]],1,mean)
  R_mean_timbre <- rbind(R_mean_timbre,res)
}
dim(R_mean_timbre)
par(mar=c(5,5,4,2))
row.names(R_mean_timbre) <- names(R_track_timbre)
R_mean_timbre
points(R_mean_timbre[,1], R_mean_timbre[,2], pch=21,  bg="lightgreen")
text(R_mean_timbre[,1], R_mean_timbre[,2], labels=row.names(R_mean_timbre), cex= 0.7, pos=3)



#plot pitch pca
track_pitch <- apply(SubPaths,2,function(x){
  h5read(x,"/analysis/segments_pitches")
})

mean_pitch <- NULL
for(i in names(track_pitch)){
  #pca
  pr <-prcomp(t(track_pitch[[i]]),scale=TRUE)
  
  #mean from pca
  res <- apply(t(pr$x),1,mean)
  mean_pitch <- rbind(mean_pitch,res)
}
dim(mean_pitch)
par(mar=c(5,6,4,2))
row.names(mean_pitch) <- names(track_pitch)
plot(mean_pitch[,1], mean_pitch[,2], ylab="", xlab="")
text(mean_pitch[,1], mean_pitch[,2], labels=row.names(mean_pitch), cex= 0.7, pos=3)
title(xlab="pc1",ylab="pc2", mgp=c(4,1,0))

#pitch recommended songs
R_track_pitch <- apply(R_SubPaths,2,function(x){
  h5read(x,"/analysis/segments_pitches")
})
R_mean_pitch <- NULL
for(i in names(R_track_pitch)){
  #pca
  pr <-prcomp(t(R_track_pitch[[i]]),scale=TRUE)
  
  #mean from pca
  res <- apply(t(pr$x),1,mean)
  R_mean_pitch <- rbind(R_mean_pitch,res)
}
dim(R_mean_pitch)
par(mar=c(5,5,4,2))
row.names(R_mean_pitch) <- names(R_track_pitch)
points(R_mean_pitch[,1], R_mean_pitch[,2], pch=21,  bg="lightgreen")
text(R_mean_pitch[,1], R_mean_pitch[,2], labels=row.names(R_mean_pitch), cex= 0.7, pos=3)



