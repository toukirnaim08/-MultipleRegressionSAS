proc means data = mydata.daily N mean min max skewness kurtosis median q1 q3 clm;
	var casual;
	run;
	
data daily_transf; /* Creating transformed data */
	set mydata.daily;
	log_casual= log(casual);
	sqrt_casual= sqrt(casual);
	cube_casual= casual**(1/3);
run;



/*1 a*/
proc means data = daily_transf N mean stddev min max skewness kurtosis q1 median q3 maxdec=2;
	var casual ;
run;
proc means data = daily_transf N mean stddev min max skewness kurtosis q1 median q3 maxdec=2;
	var log_casual ;
run;
proc means data = daily_transf N mean stddev min max skewness kurtosis q1 median q3 maxdec=2;
	var sqrt_casual ;
run;
proc means data = daily_transf N mean stddev min max skewness kurtosis q1 median q3 maxdec=4;
	var cube_casual;
run;


	
proc univariate data=daily_transf normal;
 	var casual log_casual sqrt_casual cube_casual;
 	histogram / normal(mu=est sigma=est) ;
 	qqplot / normal (mu=est sigma=est);
 	
 	ods select moments means goodnessoffit histogram qqplot ;
run;

proc univariate data=daily_transf normal PLOT;
 	var casual log_casual sqrt_casual cube_casual;

 	ods select moments goodnessoffit Plots ;
run;

proc sgplot data=daily_transf;
	hbox casual;
	run;
proc sgplot data=daily_transf;
	hbox log_casual;
	run;
proc sgplot data=daily_transf;
	hbox sqrt_casual;
	run;
proc sgplot data=daily_transf;
	hbox cube_casual;
	run;
	
/*1 b*/
proc format;
	value workingdayf 0='weekend and public holiday' 1='Working day';
	value yrf 0='2011' 1='2012';
	run;
proc sgplot data=daily_transf;
format workingday workingdayf.;
vbox casual / category=season Group=workingday;
run;

proc sgplot data=daily_transf;
format yr yrf.;
vbox casual / category=season Group=yr;
run;

/*2 a*/

proc corr data=daily_transf fisher spearman plots=matrix(histogram) plots(maxpoints=none);
	var cube_casual atemp temp hum windspeed;
	run;
	
/*2 b*/

proc sgscatter data=daily_transf;
plot cube_casual*atemp;
run;
	
proc reg data=daily_transf plots(only)=diagnostics;
model cube_casual = atemp;
output out=residual_cube_casual residual= cube_casual_residual;
run;

proc reg data=daily_transf plots=diagnostics(unpack);
	model cube_casual = atemp/ clb;
	
	run;
/* 2 c */
proc reg data=daily_transf plots=none;
	model cube_casual = atemp temp hum windspeed / selection = rsquare adjrsq cp best=3;
	run;
	
proc reg data=daily_transf plots=none;
	model cube_casual = atemp temp hum windspeed / vif;
	run;

proc reg data=daily_transf plots=diagnostics(unpack);
	model cube_casual = atemp hum windspeed / clb;
	run;

proc reg data=residual_cube_casual plots=diagnostics(unpack);
	model cube_casual_residual = hum  / clb;
	run;

proc reg data=residual_cube_casual plots=diagnostics(unpack);
	model cube_casual_residual = temp  / clb;
	run;

proc reg data=residual_cube_casual plots=diagnostics(unpack);
	model cube_casual_residual = atemp temp hum windspeed / clb;
	run;

proc reg data=residual_cube_casual plots=none;
	model cube_casual_residual = atemp temp hum windspeed / selection = rsquare adjrsq cp best=3;
	run;
	
/* 2 d */

proc reg data=daily_transf plots=none;
	model cube_casual = atemp temp hum windspeed registered count / vif;
	run;
	
proc sgplot data=daily_transf;
	var month;
	run;

data newdaily_transf;
	set daily_transf;
	if season='winter' then Winter =1; else Winter =0;
	if season='spring' then Spring =1; else Spring =0;
	if season='summer' then Summer =1; else Summer =0;
	if season='fall' then Autumn =1; else Autumn =0;
	
	if weekday='Monday' then Monday =1; else Monday =0;
	if weekday='Tuesday' then Tuesday =1; else Tuesday =0;
	if weekday='Wednesday' then Wednesday =1; else Wednesday =0;
	if weekday='Thursday' then Thursday =1; else Thursday =0;
	if weekday='Friday' then Friday =1; else Friday =0;
	if weekday='Saturday' then Saturday =1; else Saturday =0;
	if weekday='Sunday' then Sunday =1; else Sunday =0;
	
	
	if month='January' then January =1; else January =0;
	if month='February' then February =1; else February =0;
	if month='March' then March =1; else March =0;
	if month='April' then April =1; else April =0;
	if month='May' then May =1; else May =0;
	if month='June' then June =1; else June =0;
	if month='July' then July =1; else July =0;
	if month='August' then August =1; else August =0;
	if month='September' then September =1; else September =0;
	if month='October' then October =1; else October =0;
	if month='November' then November =1; else November =0;
	if month='December' then December =1; else December =0;
	run;

proc reg data=newdaily_transf plots=none;
	model cube_casual = atemp hum windspeed registered count
						workingday yr
						/ vif;
	run;

proc reg data=newdaily_transf plots=none;
	model cube_casual = atemp hum windspeed registered count
						Winter Spring Autumn workingday yr
						January February March April
						May June July August September
						October November 
						/ vif;
	run;

proc reg data=newdaily_transf;
	model cube_casual = atemp hum windspeed workingday yr
						Winter Spring Autumn / selection=rsquare adjrsq cp best=3;
	run;
	
proc reg data=newdaily_transf plots=none;
	model cube_casual = atemp hum windspeed workingday yr
						Winter Spring Autumn Summer 
						
						Monday Tuesday Wednesday Thursday Friday Saturday Sunday
						
						January February March April May June
						July August September October November
						December
						
						/ selection=stepwise;
	run;
	
/*proc reg data=newdaily_transf plots(only)=diagnostics;
atemp hum windspeed workingday yr
						Winter  Autumn  
						
						Monday Friday Saturday Sunday
						
						January February March April May 
						July  September October November
						
						
						atemp hum windspeed workingday yr
						Winter    
						
						Monday Friday Saturday Sunday
						
						  March April May 
						July  September October November
*/
proc reg data=newdaily_transf plots(only)=diagnostics;
	model cube_casual = atemp hum windspeed workingday yr
						Winter    
						
						Monday Friday Saturday Sunday
						
						  March April May 
						July  September October November
						
						/vif;

run;

data newdaily_transf;
	set daily_transf;
	if season='winter' then Winter =1; else Winter =0;
	if season='spring' then Spring =1; else Spring =0;
	if season='summer' then Summer =1; else Summer =0;
	if season='fall' then Autumn =1; else Autumn =0;
	
	if weekday='Monday' then Monday =1; else Monday =0;
	if weekday='Tuesday' then Tuesday =1; else Tuesday =0;
	if weekday='Wednesday' then Wednesday =1; else Wednesday =0;
	if weekday='Thursday' then Thursday =1; else Thursday =0;
	if weekday='Friday' then Friday =1; else Friday =0;
	if weekday='Saturday' then Saturday =1; else Saturday =0;
	if weekday='Sunday' then Sunday =1; else Sunday =0;
	
	
	if month='January' then January =1; else January =0;
	if month='February' then February =1; else February =0;
	if month='March' then March =1; else March =0;
	if month='April' then April =1; else April =0;
	if month='May' then May =1; else May =0;
	if month='June' then June =1; else June =0;
	if month='July' then July =1; else July =0;
	if month='August' then August =1; else August =0;
	if month='September' then September =1; else September =0;
	if month='October' then October =1; else October =0;
	if month='November' then November =1; else November =0;
	if month='December' then December =1; else December =0;
	run;

proc reg data=newdaily_transf plots=none;
	model cube_casual = atemp hum windspeed registered count
						workingday yr
						/ vif;
	run;

proc reg data=newdaily_transf plots=none;
	model cube_casual = atemp hum windspeed registered count
						Winter Spring Autumn workingday yr
						January February March April
						May June July August September
						October November 
						/ vif;
	run;

proc reg data=newdaily_transf;
	model cube_casual = atemp hum windspeed workingday yr
						Winter Spring Autumn / selection=rsquare adjrsq cp best=3;
	run;
	
proc reg data=newdaily_transf plots=none;
	model cube_casual = atemp hum windspeed workingday yr
						Winter Spring Autumn Summer 
						
						Monday Tuesday Wednesday Thursday Friday Saturday Sunday
						
						January February March April May June
						July August September October November
						December
						
						/ selection=stepwise;
	run;
	
/*proc reg data=newdaily_transf plots(only)=diagnostics;
atemp hum windspeed workingday yr
						Winter  Autumn  
						
						Monday Friday Saturday Sunday
						
						January February March April May 
						July  September October November
						
						
						atemp hum windspeed workingday yr
						Winter    
						
						Monday Friday Saturday Sunday
						
						  March April May 
						July  September October November
*/
proc reg data=newdaily_transf plots(only)=diagnostics;
	model cube_casual = atemp hum windspeed workingday yr
						Winter    
						
						Monday Friday Saturday Sunday
						
						  March April May 
						July  September October November
						
						/vif;

run;
	
	
	