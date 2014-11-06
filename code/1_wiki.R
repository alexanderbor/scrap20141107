#	Wikipedia scraping example
#	Juraj Medzihorsky
#	2014-11-06


#	We will extract tables from a single webpage, Wikipedia's link of countries
#	by number of police officers
#	http://en.wikipedia.org/wiki/List_of_countries_by_number_of_police_officers


library(XML)


#	create a variable that contains the link

wiki_link <- 'http://en.wikipedia.org/wiki/List_of_countries_by_number_of_police_officers'


#	download the html file into a character string

wiki_html <- readLines(wiki_link)


#	each item in the vector is a line of html

wiki_html[44:48]


#	how long is it?

length(wiki_html)


#	extract only the tables, save text as character

wiki_tables <- readHTMLTable(wiki_html, stringsAsFactors=FALSE)


#	the new object is a list of tables

typeof(wiki_tables)


#	how many are there?

length(wiki_tables)


#	what are their dimensions?

sapply(wiki_tables, dim)


#	let's inspect them

#	first:
wiki_tables[[1]][1:4, ]

#	second:
wiki_tables[[2]][1:4, ]

#	third:
wiki_tables[[3]][1:4, ]


#	we want the first

p <- wiki_tables[[1]]


#	there is something messed up because of the flags and footnotes

head(p)


#	fix it

#	fix Size and Per 100 tho columns 
#	1.	remove footnote numbers
strsplit(p[1,2], '\\[')[[1]][1]

#	2.	the first dozen or so digits are sorting code
wiki_html[224]
substr(p[1,2], 24, nchar(p[1,2]))

#	3.	remove commas as thousands separators
gsub(',', '', p[6,2])

#	4.	turn it into a number
as.numeric('8267')


#	a function that combines all these steps

fix.col <-
	function(x)
	{
		y <- strsplit(x, '\\[')[[1]][1]	
		z <- substr(y, 20, nchar(y))
		w <- gsub(',', '', z)
		n <- as.numeric(w)
		return(n)
	}


#	check what happens with the Size column

sapply(p[,2], fix.col, USE.NAMES=FALSE)


#	check what happens with the per inhabitants column

sapply(p[,4], fix.col, USE.NAMES=FALSE)


#	good practice: create a new copy of the dataset

P <- p

#	now replace the columns

P[,2] <- sapply(p[,2], fix.col, USE.NAMES=FALSE)
P[,4] <- sapply(p[,4], fix.col, USE.NAMES=FALSE)


#	check the new dataset

head(P)

#	that's it, we can save it with write.table(P, ... )
