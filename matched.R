matched=read.csv("/Users/ps22344/Downloads/ugs/matchedguise.csv")
setwd("/Users/ps22344/Downloads/ugs/rplots")
print (summary(matched))


sect1=matched[matched$SECTION == 1, ]

summary(sect1)


plotter = function (dataset)
{
	for (s in dataset[['STIMULUS']])
	{
		cat ("working on", s, "\n");
		#print (summary(dataset[dataset['STIMULUS'] == s,]));
		#print (levels(dataset['STIMULUS' == s,][['QUESTION']]));
		for (q in levels(dataset['STIMULUS' == s,][['QUESTION']]))
		 	{
			#print (q)
			# #for each  question plot the distribution over options
			outputfile=paste("stimulus", s, q, sep="_")
			postscript(paste(outputfile, ".ps"), width = 960, height = 960);
			png(paste(outputfile, ".png"),  width=640, height=640, res=100);
			qset= dataset[which(dataset['STIMULUS'] == s & dataset['QUESTION']== q),];
			options= c("Option 1"=qset[['OPTION1']], "Option 2"=qset[['OPTION2']], "Option 3"=qset[['OPTION3']]);
			print (options);
			barplot(options, 
			ylim= c(0,15), 
			col= c("white", "grey", "black"),
			main= paste("Stimulus", s, ", ", sum (options), "respondents")
			)
			dev.off()
			# #counts=str(qset[['OPTION1']]);
			# #print(counts)
			}
	}
	
	
}



plotter(sect1)