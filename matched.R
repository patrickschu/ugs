matched=read.csv("/Users/ps22344/Downloads/ugs/matchedguise.csv")

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
			qset= dataset[which(dataset['STIMULUS'] == s & dataset['QUESTION']== q),];
			options= c("Option 1"=qset[['OPTION1']], "Option 2"=qset[['OPTION2']], "Option 3"=qset[['OPTION3']]);
			print (options);
			barplot(options, 
			ylim= c(0,15), 
			col= c("white", "grey", "black"),
			#density= c(100, 100, 100),
			#col=c("red", "blue", "green"), 
			main= paste("Stimulus", s)
			)
			# #counts=str(qset[['OPTION1']]);
			# #print(counts)
			}
	}
	
	
}



plotter(sect1)