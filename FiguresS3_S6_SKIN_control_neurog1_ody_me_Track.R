#Figure S3 Skin
############Dataframe Skin
timewtmengod<- read.csv2("wtmengod_EONSkin.csv", header=TRUE)
View(timewtmengod)
subtabletSkin = rbind( timewtmengod[grep( "Skin", timewtmengod[, 1] ), ])
View(subtabletSkin)
###Control SKIN
subtabletSkin_wt = rbind( subtabletSkin[grep( "wt", subtabletSkin[, 1] ), ])
View(subtabletSkin_wt)
WTSkin_Dis=subtabletSkin_wt
View(WTSkin_Dis)

## Transform values for A, C & E, and store all values for col 3 in sym
sym = list();
for( n in c( "1wt_A1Skin", "1wt_A2Skin", "2wt_A1Skin", "2wt_A2Skin", "3wt_A1Skin", "3wt_A2Skin", "1wt_C1Skin", "1wt_C2Skin", "2wt_C1Skin", "2wt_C2Skin", "3wt_C1Skin", "3wt_C2Skin", "1wt_E1Skin", "1wt_E2Skin", "2wt_E1Skin", "2wt_E2Skin", "3wt_E1Skin", "3wt_E2Skin" ) ){
  sym[[n]] <- WTSkin_Dis [WTSkin_Dis$Track.name == n, 3]*-1;
}
for( n in c( "1wt_B1Skin", "1wt_B2Skin", "2wt_B1Skin", "2wt_B2Skin", "3wt_B1Skin", "3wt_B2Skin", "1wt_D1Skin", "1wt_D2Skin", "2wt_D1Skin", "2wt_D2Skin", "3wt_D1Skin", "3wt_D2Skin", "1wt_F1Skin", "1wt_F2Skin", "2wt_F1Skin", "2wt_F2Skin", "3wt_F1Skin", "3wt_F2Skin" ) ){
  sym[[n]] <- WTSkin_Dis [WTSkin_Dis$Track.name == n, 3];
}


## Define plot function
addPoints = function( tabToPlot, varToSelect = WTSkin_Dis[WTSkin_Dis$Track.name == "1wt_A1Skin", 2], Thr = 37, col1 = 1, col2 = 8 ){
  points( tabToPlot[varToSelect < Thr, 1:2], col = col1, lwd = 3,  type = 'l' );
  points( tabToPlot[varToSelect > Thr - 1, 1:2], col = col2, lwd = 3,  type = 'l' );
}

## Plot1 : Anterior Plot WT - Fig S3
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1wt_A1Skin", "1wt_A2Skin", "1wt_B1Skin", "1wt_B2Skin", "2wt_A1Skin", "2wt_A2Skin", "2wt_B1Skin", "2wt_B2Skin", "3wt_A1Skin", "3wt_A2Skin", "3wt_B1Skin", "3wt_B2Skin" ) ){
  addPoints( tabToPlot = cbind( sym[[n]], WTSkin_Dis[WTSkin_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

## Plot2 : Middel WT- Fig S3
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1wt_C1Skin", "1wt_C2Skin", "1wt_D1Skin", "1wt_D2Skin", "2wt_C1Skin", "2wt_C2Skin", "2wt_D1Skin", "2wt_D2Skin", "3wt_C1Skin", "3wt_C2Skin", "3wt_D1Skin", "3wt_D2Skin" ) ){
  addPoints( tabToPlot = cbind( sym[[n]], WTSkin_Dis[WTSkin_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

## Plot3 : Posterior WT- Fig S3
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1wt_E1Skin", "1wt_E2Skin", "1wt_F1Skin", "1wt_F2Skin", "2wt_E1Skin", "2wt_E2Skin", "2wt_F1Skin", "2wt_F2Skin", "3wt_E1Skin", "3wt_E2Skin", "3wt_F1Skin", "3wt_F2Skin" ) ){
  addPoints( tabToPlot = cbind( sym[[n]], WTSkin_Dis[WTSkin_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

#### Read data neurog1mut SKIN
subtabletSkin_ng = rbind( subtabletSkin[grep( "ng", subtabletSkin[, 1] ), ])
View(subtabletSkin_ng)
ngSkin_Dis=subtabletSkin_ng
View(ngSkin_Dis)

## Transform values for A, C & E, and store all values for col 3 in ngsym
ngsym = list();
for( n in c( "1ng_A1Skin", "1ng_A2Skin", "2ng_A1Skin", "2ng_A2Skin", "3ng_A1Skin", "3ng_A2Skin", "1ng_C1Skin", "1ng_C2Skin", "2ng_C1Skin", "2ng_C2Skin", "3ng_C1Skin", "3ng_C2Skin", "1ng_E1Skin", "1ng_E2Skin", "2ng_E1Skin", "2ng_E2Skin", "3ng_E1Skin", "3ng_E2Skin" ) ){
  ngsym[[n]] <- ngSkin_Dis [ngSkin_Dis$Track.name == n, 3]*-1;
}
for( n in c( "1ng_B1Skin", "1ng_B2Skin", "2ng_B1Skin", "2ng_B2Skin", "3ng_B1Skin", "3ng_B2Skin", "1ng_D1Skin", "1ng_D2Skin", "2ng_D1Skin", "2ng_D2Skin", "3ng_D1Skin", "3ng_D2Skin", "1ng_F1Skin", "1ng_F2Skin", "2ng_F1Skin", "2ng_F2Skin", "3ng_F1Skin", "3ng_F2Skin" ) ){
  ngsym[[n]] <- ngSkin_Dis [ngSkin_Dis$Track.name == n, 3];
}

## Define plot function
addPoints = function( tabToPlot, varToSelect = ngSkin_Dis[ngSkin_Dis$Track.name == "1ng_A1Skin", 2], Thr = 37, col1 = 6, col2 = 8 ){
  points( tabToPlot[varToSelect < Thr, 1:2], col = col1, lwd = 3,  type = 'l' );
  points( tabToPlot[varToSelect > Thr - 1, 1:2], col = col2, lwd = 3,  type = 'l' );
}

## Plot1 : Anterior Plot ng - Fig S3
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1ng_A1Skin", "1ng_A2Skin", "1ng_B1Skin", "1ng_B2Skin", "2ng_A1Skin", "2ng_A2Skin", "2ng_B1Skin", "2ng_B2Skin", "3ng_A1Skin", "3ng_A2Skin", "3ng_B1Skin", "3ng_B2Skin" ) ){
  addPoints( tabToPlot = cbind( ngsym[[n]], ngSkin_Dis[ngSkin_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

## Plot2 : Middel ng - Fig S3
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1ng_C1Skin", "1ng_C2Skin", "1ng_D1Skin", "1ng_D2Skin", "2ng_C1Skin", "2ng_C2Skin", "2ng_D1Skin", "2ng_D2Skin", "3ng_C1Skin", "3ng_C2Skin", "3ng_D1Skin", "3ng_D2Skin" ) ){
  addPoints( tabToPlot = cbind(ngsym[[n]], ngSkin_Dis[ngSkin_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

## Plot3 : Posterior ng - Fig S3
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1ng_E1Skin", "1ng_E2Skin", "1ng_F1Skin", "1ng_F2Skin", "2ng_E1Skin", "2ng_E2Skin", "2ng_F1Skin", "2ng_F2Skin", "3ng_E1Skin", "3ng_E2Skin", "3ng_F1Skin", "3ng_F2Skin" ) ){
  addPoints( tabToPlot = cbind( ngsym[[n]], ngSkin_Dis[ngSkin_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

######Figure S6 control, ody and medusa
#### Read data ody mutant
odSkin_Dis = rbind( subtabletSkin[grep( "od", subtabletSkin[, 1] ), ])
View(odSkin_Dis)

## Transform values and store all values for col 3 in odsym
odsym = list();
for( n in c( "1od_A1Skin", "1od_A2Skin", "2od_A1Skin", "2od_A2Skin","2od_B1Skin", "3od_A1Skin", "3od_A2Skin", "1od_C1Skin","1od_C2Skin", "1od_D1Skin",  "2od_C1Skin", "2od_C2Skin", "3od_C1Skin", "3od_C2Skin", "1od_E1Skin","1od_E2Skin",  "2od_E1Skin", "2od_E2Skin","3od_E1Skin", "3od_E2Skin" ) ){
  odsym[[n]] <- odSkin_Dis [odSkin_Dis$Track.name == n, 3]*-1;
}
for( n in c( "1od_B1Skin", "1od_B2Skin",  "2od_B2Skin", "3od_B1Skin", "3od_B2Skin", "1od_D2Skin", "2od_D1Skin", "2od_D2Skin", "3od_D1Skin", "3od_D2Skin", "1od_F1Skin", "1od_F2Skin", "2od_F1Skin", "2od_F2Skin", "3od_F1Skin", "3od_F2Skin" ) ){
  odsym[[n]] <- odSkin_Dis [odSkin_Dis$Track.name == n, 3];
}

## Define plot function
addPoints = function( tabodToPlot, varodToSelect = odSkin_Dis[odSkin_Dis$Track.name == "1od_A1Skin", 2], Thr = 37, col1 = 4, col2 = 8 ){
  points( tabodToPlot[varodToSelect < Thr, 1:2], col = col1, lwd = 3,  type = 'l' );
  points( tabodToPlot[varodToSelect > Thr - 1, 1:2], col = col2, lwd = 3,  type = 'l' );
}

## Plot1 : Anterior Plot od - Figure S6
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1od_A1Skin", "1od_A2Skin", "1od_B1Skin", "1od_B2Skin", "2od_A1Skin", "2od_A2Skin", "2od_B1Skin", "2od_B2Skin", "3od_A1Skin", "3od_A2Skin", "3od_B1Skin", "3od_B2Skin" ) ){
  addPoints( tabodToPlot = cbind( odsym[[n]], odSkin_Dis[odSkin_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

## Plot2 : Middel od - Figure S6
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1od_C1Skin", "1od_C2Skin", "1od_D1Skin", "1od_D2Skin", "2od_C1Skin", "2od_C2Skin", "2od_D1Skin", "2od_D2Skin", "3od_C1Skin", "3od_C2Skin", "3od_D1Skin", "3od_D2Skin" ) ){
  addPoints( tabodToPlot = cbind( odsym[[n]], odSkin_Dis[odSkin_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );


## Plot3 : Posterior od - Figure S6
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1od_E1Skin", "1od_E2Skin", "1od_F1Skin", "1od_F2Skin", "2od_E1Skin", "2od_E2Skin", "2od_F1Skin", "2od_F2Skin", "3od_E1Skin", "3od_E2Skin", "3od_F1Skin", "3od_F2Skin" ) ){
  addPoints( tabodToPlot = cbind( odsym[[n]], odSkin_Dis[odSkin_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

#### Read data medusa (me) mutant
meSkin_Dis = rbind( subtabletSkin[grep( "me", subtabletSkin[, 1] ), ])
View(meSkin_Dis)

## Transform values and store all values for col 3 in mesym
mesym = list();
for( n in c(  "1me_B1Skin", "1me_B2Skin", "2me_B2Skin", "2me_B1Skin", "3me_B1Skin", "3me_B2Skin", "2me_D1Skin", "2me_D2Skin", "3me_D1Skin", "3me_D2Skin", "1me_E1Skin","1me_E2Skin","1me_F2Skin","2me_F1Skin", "2me_F2Skin", "3me_F1Skin", "3me_F2Skin" ) ){
  mesym[[n]] <- meSkin_Dis [meSkin_Dis$Track.name == n, 3]*-1;
}
for( n in c( "1me_A1Skin", "1me_A2Skin", "2me_A1Skin", "2me_A2Skin","3me_A1Skin", "3me_A2Skin", "1me_C1Skin","1me_C2Skin", "1me_D1Skin", "1me_D2Skin", "2me_C1Skin", "2me_C2Skin", "3me_C1Skin", "3me_C2Skin", "1me_F1Skin",  "2me_E1Skin", "2me_E2Skin", "3me_E1Skin", "3me_E2Skin" ) ){
  mesym[[n]] <- meSkin_Dis [meSkin_Dis$Track.name == n, 3];
}

## Define plot function
addPoints = function( tabmeToPlot, varmeToSelect = meSkin_Dis[meSkin_Dis$Track.name == "1me_A1Skin", 2], Thr = 37, col1 = 3, col2 = 8 ){
  points( tabmeToPlot[varmeToSelect < Thr, 1:2], col = col1, lwd = 3,  type = 'l' );
  points( tabmeToPlot[varmeToSelect > Thr - 1, 1:2], col = col2, lwd = 3,  type = 'l' );
}

## Plot1 : Anterior Plot me- Figure S6

plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1me_A1Skin", "1me_A2Skin", "1me_B1Skin", "1me_B2Skin", "2me_A1Skin", "2me_A2Skin", "2me_B1Skin", "2me_B2Skin", "3me_A1Skin", "3me_A2Skin", "3me_B1Skin", "3me_B2Skin" ) ){
  addPoints( tabmeToPlot = cbind( mesym[[n]], meSkin_Dis[meSkin_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

## Plot2 : Middel me - Figure S6
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1me_C1Skin", "1me_C2Skin", "1me_D1Skin", "1me_D2Skin", "2me_C1Skin", "2me_C2Skin", "2me_D1Skin", "2me_D2Skin", "3me_C1Skin", "3me_C2Skin", "3me_D1Skin", "3me_D2Skin" ) ){
  addPoints( tabmeToPlot = cbind( mesym[[n]], meSkin_Dis[meSkin_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

## Plot3 : Posterior me - Figure S6
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1me_E1Skin", "1me_E2Skin", "1me_F1Skin", "1me_F2Skin", "2me_E1Skin", "2me_E2Skin", "2me_F1Skin", "2me_F2Skin", "3me_E1Skin", "3me_E2Skin", "3me_F1Skin", "3me_F2Skin" ) ){
  addPoints( tabmeToPlot = cbind( mesym[[n]], meSkin_Dis[meSkin_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

