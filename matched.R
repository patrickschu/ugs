matched=read.csv("/Users/ps22344/Downloads/ugs/matchedguise.csv")
setwd("/Users/ps22344/Downloads/ugs/rplots")
print (summary(matched))


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
	print (subseti)
	for (q in levels(subseti[['QUESTION']]))
	 {
		cat ("working on", q, "\n");
		print (subseti[subseti['QUESTION']==q,])
		# print (summary(dataset[which(dataset['STIMULUS'] == 2 | dataset['STIMULUS'] == 4 ),]['STIMULUS']));
		# for (q in levels(dataset['STIMULUS' == s,][['QUESTION']]))
		 	# {
			# outputfile=paste("compare_stimulus", s, q, sep="_")
			# postscript(paste(outputfile, ".ps"), width = 960, height = 960);
			# png(paste(outputfile, ".png"),  width=640, height=640, res=100);
						
			# qset= dataset[dataset['QUESTION']== q,];
			# #print (qset)
			# options= c("Option 1"=qset[['OPTION1']], "Option 2"=qset[['OPTION2']], "Option 3"=qset[['OPTION3']]);
			# barplot(options, 
			# ylim= c(0,15), 
			# ylab= "Number of respondents",
			# col= c("white", "grey", "black"),
			# main= paste("Stimulus: ", s, ", question: ", q,  ", respondents: ", sum (options), sep="")
			# )
			# dev.off()
			# dev.off()
			# # #counts=str(qset[['OPTION1']]);
			# # #print(counts)
			# }
	 }
	
	
}




comparisonplotter(sect1)