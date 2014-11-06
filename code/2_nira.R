#	NIRA WDTT 2005 scraping
#	Juraj Medzihorsky
#	2014-11-06

#	Here we scarpe NIRA WDTT 2005 
#	http://www.nira.or.jp/past/ice/nwdtt/2005/

library(XML)


#	load the main page

main_link <- 'http://www.nira.or.jp/past/ice/nwdtt/2005/'

main_html <- readLines(main_link)


#	we need links to subpages
#	from the source we see that they contain 'IDX1'

idx1_lines <- grep('IDX1', main_html)
idx1_html <- main_html[idx1_lines]


#	we want the stuff between "s, without the "./"

get.idx1 <-
	function(x)
	{
		y <- strsplit(x, '"')[[1]][2]
		z <- gsub('\\./', '', y)
		return(z)
	}


#	create links to the subpages

idx1_suffixes <- sapply(idx1_html, get.idx1, USE.NAMES=F) 

a_links <- paste(main_link, idx1_suffixes, sep='')

a_links


#	download all of them

a_htmls <- sapply(a_links, readLines)

length(a_htmls)

sapply(a_htmls, length)



#	now for each of them we want to extraxt the links to TTs
#	we know that they contain '/DAT/' 

get.ttlinks <-
	function(x)
	{
		d_lines <- grep('/DAT/', x)
		d_html <- x[d_lines]
		d1 <- sapply(d_html, function(i) strsplit(i, 'DAT/')[[1]][2], USE.NAMES=F)
		d2 <- sapply(d1, function(j) strsplit(j, '"')[[1]][1], USE.NAMES=F)
		return(d2)
	}


#	get them all

tt_links <- lapply(a_htmls, get.ttlinks)


#	some are duplicated, remove them

tt_links <- unlist(tt_links)

length(tt_links)

tt_links <- unique(tt_links)

length(tt_links)


#	456 unique links, make them full

full_links <- paste(main_link, 'DAT/', tt_links, sep='')

full_links[1]


#	Now we can download them.  For expediency only the first ten.


tt_htmls <- lapply(full_links[1:1e1], readLines)

sapply(tt_htmls, length)


hh <- htmlParse(tt_htmls[[1]])


#	get all the paragraphs

hh_p <- getNodeSet(hh, '//p')
hh_p


#	get the header and footer

hh_p_3 <- toString.XMLNode(hh_p[[3]])
hh_p_3


#	get only the text

gsub('<.*?>', '', hh_p_3)


#	now all of this for the whole page

hh_pv <- lapply(hh_p, toString.XMLNode)
hh_pv

hh_pv <- sapply(hh_pv, function(j) gsub('<.*?>', '', j))
hh_pv



#	get header and footer

hh_c <- getNodeSet(hh, '//center')
hh_c_1 <- toString.XMLNode(hh_c[[1]])
hh_c_1

hh_c_1_t <- gsub('<.*?>', '', hh_c_1)
hh_c_1_t 


#	remove the page running head

hh_c_1_t <- gsub(substr(hh_c_1_t, 1, 38), '', hh_c_1_t)
hh_c_1_t 


#	now a function that does all of this

clean.tt <-
	function(x)
	{
		h <- htmlParse(x)
		#	pars
		np <- getNodeSet(h, '//p')
		tp <- lapply(np, toString.XMLNode)
		tp <- sapply(tp, function(j) gsub('<.*?>', '', j)) 	
		# 	header
		nc <- getNodeSet(h, '//center')
		org <- gsub('<.*?>', '', toString.XMLNode(nc[[1]]))
		org <- gsub(substr(org, 1, 38), '', org)	
		#	return	
		return(c(org, tp))
	}


#	get it from all pages

tt_text <- lapply(tt_htmls, clean.tt)


#	check the 2nd and 3rd organization

tt_text[[2]]

tt_text[[3]]


#	That's it, the next thing would be to merge them into a table.
