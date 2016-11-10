matched=read.csv("/Users/ps22344/Documents/matchedguise.csv")

print (summary(matched))


sect1=matched[matched$SECTION == 1, ]

summary(sect1)


plotter = function (dataset)
{
	for (s in dataset[['STIMULUS']])
	{
		cat ("working on", s, "\n");
		#print (dataset[dataset['STIMULUS'] == s,]);
		
		for (q in levels(dataset['STIMULUS' == s,][['QUESTION']]))
		{
			#for each  question plot the distribution over options
			qset=dataset[which(dataset['STIMULUS'] == s & dataset['QUESTION']== q),];
			#print (qset);
			counts=str(qset[['OPTION1']]);
			print(counts)
		}
	}
	
	
}



plotter(sect1)