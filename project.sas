proc import datafile= "C:\\Users\\amyha\\Dropbox\\NCSU-MSA\\MSA - Fall\\Time Series II\\Final Project\\Well Data\\clean.csv"
	out = well
	replace 
	dbms = csv;
run;

data well;
	set well;
    drop var1;
    n = _N_;
run;

proc sql;
	create table train as
	select *
	from well
	where year(datepart(datetime)) ge 2014 and n le 93623;
quit;

proc sql;
	create table well2014 as
	select *
	from well
	where year(datepart(datetime)) ge 2014;
quit;

/*
visualize data
*/
proc sgplot data=train;
	series x=datetime y=height;
	series x=datetime y=tide_ft;
	series x=datetime y=rain;
run;
quit;

/*
use p=2 to fit regressors
*/
proc arima data=train;
identify var=height crosscorr=(tide_ft rain);
estimate input=(tide_ft rain) p=2 method=ML;
forecast out=trainres;
run;
quit;

/*
tide is not significant, try with just rain
*/
proc arima data=train;
identify var=height crosscorr=(rain);
estimate input=(rain) p=2 method=ML;
forecast out=trainres2;
run;
quit;

proc arima data=train;
identify var=height crosscorr=(tide_ft);
estimate input=(tide_ft) p=2 method=ML;
forecast out=trainres2;
run;
quit;


/*
check for stationarity of residuals
*/
proc arima data=trainres;
identify var=residual stationarity=(adf=3);
run;
quit;

proc arima data=trainres2;
identify var=residual stationarity=(adf=3);
run;
quit;

/*look for p and q values*/
proc arima data=train;
identify var=height(1) crosscorr=(rain(1));
estimate input=(rain) p=12 q=1 method=ML;
run;
quit;

proc arima data=train;
identify var=height(1,4383) crosscorr=(rain(1,4383));
estimate input=(rain) p=12 q=1 method=ML;
run;
quit;

proc arima data=train;
identify var=height(1,4383) crosscorr=(rain(1,4383));
estimate input=(rain) p=12 q=0 method=ML;
run;
quit;

proc arima data=train;
identify var=height(1,4383) crosscorr=(rain(1,4383));
estimate input=(rain) p=12 q=1 method=ML;
run;
quit;

proc arima data=train;
identify var=height(1,4383) crosscorr=(rain(1,4383));
estimate input=(rain) p=13 q=0 method=ML;
run;
quit;

proc arima data=train;
identify var=height(1,4383) crosscorr=(rain(1,4383));
estimate input=(rain) p=13 q=1 method=ML;
run;
quit;

proc arima data=train;
identify var=height(1,4383) crosscorr=(rain(1,4383));
estimate input=(rain) p=13 q=2 method=ML;
run;
quit;


/*
forecast rain values to use in well forecast
*/
proc sgplot data=train;
	series x=datetime y=rain;
run;
quit;

proc arima data=train;
identify var=rain stationarity=(adf=3);
run;
quit;

proc arima data=train;
identify var=rain(1,4383);
estimate p=13 q=1 method=ML;
run;
quit;


proc arima data=well2014;
identify var=rain(1,4383);
estimate p=13 q=1 method=ML;
forecast back=168 lead=168 out=rainres;
run;
quit;






