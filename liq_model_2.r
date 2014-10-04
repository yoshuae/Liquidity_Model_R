# install.packages("lubridate")
# require(lubridate) # R package to easily handle dates and modify them
# setwd("\\\\server5/Data/Users/jedgerton/Desktop/Liquidity/")
# input=read.csv("test.csv",header=T,as.is=c(4,5,6,8,11,13,14,17))
# draft 1 just tells us the time we can take all of our money out not accounting for some audit reserve redemption period of unknown length

if(!exists("input")){
		setwd("\\\\server5/Data/Users/jedgerton/Desktop/Liquidity/")   # specify your own current file path for reading and writing
		input=read.csv("test.csv",header=T,as.is=c(4,5,6,8,11,13,14,17))  # load input file in csv format with specific column formatting
	}
	
liq_model<-function(input,parameter=2,deadline="1/1/2099",fund=1) # file specifies file with all fund information/characteristics, parameter 1=> fastest path with fees; 2=> fastest no fee path
{
	check.input();
	check.packages();
	input<<-input   # make the input global variable
	yes<-c("y","Yes","yes","Y")  #yes and no vectors are easy to accommodate peoples different styles of input
	no<-c("n","No","no","N")
	#create relevant variables for output
	#dates<-c(); withdraw<-c(); note<-c();
	deadline<-as.Date(deadline,format="%m/%d/%Y") # convert deadline input to proper R date object
	start.date<-as.Date(input$Start.Day[fund],format="%m/%d/%Y");
	if(fund>dim(input)[1]){return("Error: Fund doesn't exist in input file.");}  # error check that a fund that is referenced exists in the file. 
	if((parameter %in% c(1,2))==0){return("Error: Incorrect parameter value, choose 1 for quickest with fees or 2 for quickest without fees.");} # error check on parameter call, must be 1 or 2
	#size=dim(input); funds=size[1]; factors=size[2]; # grab dimensions and qualities of the input file
	tm<-as.Date(input$Start.Day[fund],format="%m/%d/%Y"); # lets use t as the total time it takes to get all your money out. 
	p<<-100; #initialize a 'global' variable "p" specifying the percent remaining in the fund
	# output various inputs of the file and fund
	print(paste("Fund Identifier:",fund))
	print(sprintf("Investment Size: $%.2f", input$Amount[fund]))
	print(paste("Start date:",date.format(start.date)))
	print(paste("Withdrawal Deadline:",date.format(deadline)))
	if(deadline<=tm){return("Error: Deadline Passed")}  # print error if deadline has already passed for this fund


	# if we do quickest w/fees, parameter 1 specifies that you want to withdraw money as soon as possible even with fees
	if(parameter==1){
		if(!(input$soft.lock.up[fund]%in%no)){
			return.string<-paste("Can immediately withdraw all money with $",as.numeric(input$Amount[fund])*as.numeric(input$fee[fund])," fee. Withdrawal Date: ",date.format(settle(tm,fund)),sep=""); 
			return(return.string);} #if soft lock is available, specify the fee that must be paid and the date with which money can be withdrawn
	if(input$Hardlock[fund]%in%yes | input$redemption[fund]%in%yes){parameter<-2;} # if there is no soft lock up but there are either redemptions or hard-lock up, go to hard lock up set conditions, i.e. assign parameter=2
	else return(c("Can withdraw all money immediately",date.format(settle(tm,fund)))); # if there is no soft luck up, no hard lock up, and no redemptions, withdraw all money immediately
	}



	# quickest way to withdraw money without paying any fees
	if(parameter==2){
		# need to check that deadline does not occur during hard lock up period 
		if((input$Hardlock[fund]%in%no) & (input$redemption[fund]%in%yes) & (input$inv.level.gate[fund]%in%no)){
			return(date.format(settle(red_cyc(tm,fund,deadline)[1],fund)))} # no hard-lock, yes redemptions, no investor level gate => withdraw all money less settlement date and audit 
		if(input$Hardlock[fund]%in%yes) {
			tm<-adj_date(tm,input$lockup.units[fund],input$initial.lockup.period[fund]);
			if(tm>deadline){return("Unable to withdraw any money under hard-lock up period before deadline!");}
			}	# adjust t for the initial hard lock up period by incrementing by the hard lock up period
		if((input$rolling.hard.lock[fund]%in%yes) && (input$inv.level.gate[fund]%in%no)) {return(c("Return all money during roll-lock after",date.format(settle(tm,fund))));}
		if(input$redemption[fund]%in%no){str=paste("Can withdraw all money by:",date.format(settle(tm,fund))); return(str);}  # if no redemption period
		if(input$redemption[fund]%in%yes)
			{
			if(is.numeric(input$forward.redemption.months[fund])){tm<-adj_date(tm,"m",input$forward.redemption.months[fund])}
			if(input$inv.level.gate[fund]==0){return("Error: Investor Level Gate of 0%")}
			if((input$redemption[fund]%in%yes) & (input$inv.level.gate[fund]<100)){ 
				output<-red_cyc(tm,fund,deadline); # output is the return from red_cyc the redemption cycle function of type 'list' containing 
				tm<-output[[1]];  # update date_object
				p<-output[[2]];  # update 'p' variable
				balance=100-p; # p is the amount (%) left in the fund thus balance=100-p tells you what percent you have withdrawn.
				string=paste("Can withdraw ", balance ,"% of money by ",date.format(settle(tm,fund)),sep=""); 
				return(string);}
			#if(is.numeric(input$irregular.redemption.cycle[1])){t<-adj_date(t,"m",input$irr.red.cyl.months[1])}
			if(input$rolling.hard.lock[fund]%in%no){string2=paste("Approximate date you'll have your money back:",date.format(settle(tm,fund))); return(string2);} # no investor level gate, withdraw all money at first redemption period
			}
	else  return(c("***Can withdraw all money immediately***",date.format(settle(tm,fund)))); #Safety check... If we get here we might be missing something since we should have covered all bases in prior conditionals
	}
}

#function to adjust date, given unit and duration. Identify units and adjust date for given length of time with respective units. If date_object is not correctly formatted, return error. 
adj_date<-function(date_object,time_unit,len){
	if(match(time_unit,c("months","Months","m","month","Month","M"),nomatch=0)) {date_object<-update(date_object,month=month(date_object)+len); return(date_object);}
	if(match(time_unit,c("Years","years","y","year","Year","Y"),nomatch=0)) {date_object<-update(date_object,year=year(date_object)+len); return(date_object);}
	if(match(time_unit,c("quarter","quarters","q","Quarters","Quarter","Q"),nomatch=0)) {date_object<-update(date_object,month=month(date_object)+3*len); return(date_object);}   # 3 months to 1 quarter
	if(match(time_unit,c("d","day","days","Day","Days","D"),nomatch=0)) {date_object<-update(date_object,day=day(date_object)+len); return(date_object);}
	return("incorrect date format, use mm/dd/yyyy");
}
#function to adjust for settlement date
settle<-function(date_object,index){
	if(input$settlement.period[index]%in%c("EOY","eoy","Eoy")){
		date_object<-update(date_object,month=12); 
		date_object<-update(date_object,day=31);
		return(date_object);}
	settlement_holder<-as.numeric(input$settlement.period[index]);
	if(is.na(settlement_holder)){print("Error in settlement period format. Must be numeric or EOY."); return("ERROR");}
	if(is.numeric(settlement_holder)){
		date_object<-update(date_object,day=day(date_object)+settlement_holder); 
		return(date_object)};  # check if there is a numeric value and if there is, increment the date object for the settlement date
	return(0);
}
#function to adjust the redemption cycle. Also makes sure we don't do more redemption cycles than needed nor do we bypass the deadline
red_cyc<-function(date_object,index,deadline){
	lim<-ceiling(p/input$inv.level.gate[index]); # lim specifies the min number of redemptions(also the fastest way) to withdraw the rest of the money 
	check<-date_object;
	while(deadline>check & lim>0){
		check<-adj_date(date_object,input$redemption.freq[index],1)  # need to finish this section. use adjust date to continuously update date until deadline hits or you have fully redeemed fund! 
		if(check<deadline){
			date_object<-check; 
			p<-adj_p(p,input$inv.level.gate[index]); 
			lim=lim-1;} #check ensures that the update to date_object doesn't exceed the deadline
	}
	return(list(date_object,p));  # return a vector with both the date object and the percent remaining in the fund
}
#function to adjust for the audit amount withheld and the audit date held out
audit<-function(date_object,fund){
	if(is.numeric(input$Audit.Reserve.Redemption[fund])){
		date_object<-update(date_object,day=day(date_object)+input$Audit.Reserve.Redemption[fund]);
		p<-adj_p(p,input$inv.level.gate[fund]); 
		return(date_object);}
	return(list(date_object,p));
}

# function to adjust the percent of money left in fund. Never goes below 0
adj_p<-function(p,fee){
	p<-max(p-fee,0);
	return(p);
}

# check.packages() makes sure all the necessary R packages are installed and loaded into the terminal
check.packages<-function(){
	if(!require(lubridate)){
		install.packages("lubridate")   # we need to use the R package 'lubridate' else the formatting and handling of dates will not work. Choose any USA cran mirror
		require(lubridate) # R package to easily handle dates and modify them
	}
	print("All required packages correctly installed (lubridate) and the input file is loaded.");
	return(0);
}

check.input<-function(){
	if(!exists("input")){
		setwd("\\\\server5/Data/Users/jedgerton/Desktop/Liquidity/")   # specify your own current file path for reading and writing
		input=read.csv("test.csv",header=T,as.is=c(4,5,6,8,11,13,14,17))  # load input file in csv format with specific column formatting
	}
	return(0);
}

# function to format date objects to a easily readable version for final printing to terminal
date.format<-function(date.in){
	return(format(date.in, format="%B %d, %Y"))
}


#Example of how functions is called once compiled:
#input=read.csv("test.csv",header=T,as.is=c(4,5,6,8,11,13,14,17))
#source("liquidity_model.r")
#liq_model(input,2,"1/1/2015",6)

