matched=read.csv("/Users/ps22344/Downloads/ugs/matchedguise.csv")
setwd("/Users/ps22344/Downloads/ugs/rplots")
print (summary(matched))
library(ggplot2)

sect1=matched[matched$SECTION == 1, ]

summary(sect1)


plotter = function (dataset)
#for each  question, plot the distribution over options
{
	for (s in dataset[['STIMULUS']])
	{
		cat ("working on", s, "\n");
		#print (summary(dataset[dataset['STIMULUS'] == s,]));
		for (q in levels(dataset['STIMULUS' == s,][['QUESTION']]))
		 	{
			outputfile=paste("stimulus", s, q, sep="_")
			postscript(paste(outputfile, ".ps"), width = 960, height = 960);
			png(paste(outputfile, ".png"),  width=640, height=640, res=100);
			qset= dataset[which(dataset['STIMULUS'] == s & dataset['QUESTION']== q),];
			options= c("Option 1"=qset[['OPTION1']], "Option 2"=qset[['OPTION2']], "Option 3"=qset[['OPTION3']]);
			barplot(options, 
			ylim= c(0,15), 
			ylab= "Number of respondents",
			col= c("white", "grey", "black"),
			main= paste("Stimulus: ", s, ", question: ", q,  ", respondents: ", sum (options), sep="")
			)
			dev.off()
			dev.off()
			# #counts=str(qset[['OPTION1']]);
			# #print(counts)
			}
	}
	
	
}



comparisonplotter = function (dataset)
#we need to plot stimulus 2 and 4 side by side
#for each question, plot options 1, 2, and 3 for stim 2 and 4 side by side
{
	subseti= dataset[which(dataset['STIMULUS'] == 2 | dataset['STIMULUS'] == 4 ),]
	#print (subseti)
	for (q in levels(subseti[['QUESTION']]))
	 {
		cat ("working on", q, "\n");
		qsubseti= subseti[subseti['QUESTION']==q,];
		print (qsubseti)	;	

		option1= c(
		"Stimulus 2"= qsubseti[qsubseti['STIMULUS'] == 2, ][['OPTION1']], 
		"Stimulus 4"= qsubseti[qsubseti['STIMULUS'] == 4, ][['OPTION1']]
		);
		
		option2= c(
		"Stimulus 2"= qsubseti[qsubseti['STIMULUS'] == 2, ][['OPTION2']], 
		"Stimulus 4"= qsubseti[qsubseti['STIMULUS'] == 4, ][['OPTION2']]
		);
		
		option3= c(
		"Stimulus 2"= qsubseti[qsubseti['STIMULUS'] == 2, ][['OPTION3']], 
		"Stimulus 4"= qsubseti[qsubseti['STIMULUS'] == 4, ][['OPTION3']]
		);
		
		outputfile=paste("comparing_stimuli", q, sep="_")
		postscript(paste(outputfile, ".ps"), width = 960, height = 960);
		png(paste(outputfile, ".png"),  width=640, height=640, res=100);
		
		barplot (
		cbind(option1, option2, option3), 
		names.arg= c(' > 60,000','40,000 - 60,000','< 40,000'), 
		ylim=c(0,15),
		ylab= "Number of responses",
		beside= TRUE, 
		legend= c("BrE guise", "GA guise"),
		args.legend= list(x= "topright"),
		main= paste("Comparing Stimuli, question: ", q, ", respondents: ", sum(
		option1['Stimulus 2'], option2['Stimulus 2'], option3['Stimulus 2']), sep=""),
		)	
		dev.off()
		dev.off()
	 }
	
	
}




comparisonplotter(sect1)