# The code reads in a .txt file which contains information about a game of Dominion.
# It calculates the number of cards and the score for each player turn by turn. 
# The repo should contain a test file for which the code can be run. 
#
# Note that this only valid for the base game of Dominion with only two players. However, the code is a work in progress. 


#reads in the game log
dom <- readLines("dominion.txt")

#prints the supply cards
x <- sub("Supply cards: ", "", dom[2])
supply <- strsplit(x, ", ")
supply <- unlist(supply)

print("Supply cards")
print(supply)

#initialises several variables needed
#initial score
p1score <- 3
p2score <- 3

#initial score vector
p1scorecounter <- numeric(0)
p1scorecounter <- c(p1scorecounter,3)
p1cardcounter <- numeric(0)
p1cardcounter <- c(p1cardcounter, 10)

p2scorecounter <- numeric(0) 
p2scorecounter <- c(p2scorecounter,3)
p2cardcounter <- numeric(0)
p2cardcounter <- c(p2cardcounter, 10)

#inital number of cards
p1cards <- 10
p2cards <- 10



#player names
if (length(grep("starting", unlist(strsplit(dom[4]," ")))) == 1){
	p1 <- unlist(strsplit(dom[4]," "))[1]
	}

if (length(grep("starting", unlist(strsplit(dom[5]," ")))) == 1){
	p2 <- unlist(strsplit(dom[5]," "))[1]
	}


#calculates P1 score
for (i in 1:length(dom)) {
	a <- strsplit(dom[i], " ")
	b <- unlist(a)
	if (length(grep(p1, b)) == 1 && length(grep("gains", b)) == 1 && length(grep("Estate", b)) == 1) {
		p1score <- p1score + 1
		}
	if (length(grep(p1, b)) == 1 && length(grep("trashes", b)) == 1 && length(grep("Estate", b)) == 1) {
		p1score <- p1score - 1
		}
	if (length(grep(p1, b)) == 1 && length(grep("gains", b)) == 1 && length(grep("Curse", b)) == 1) {
		p1score <- p1score - 1
		}
	if (length(grep(p1, b)) == 1 && length(grep("trashes", b)) == 1 && length(grep("Curse", b)) == 1) {
		p1score <- p1score + 1
		}
	if (length(grep(p1, b)) == 1 && length(grep("gains", b)) == 1 && length(grep("Duchy", b)) == 1) {
		p1score <- p1score + 3
		}
	if (length(grep(p1, b)) == 1 && length(grep("trashes", b)) == 1 && length(grep("Duchy", b)) == 1) {
		p1score <- p1score - 3
		}
	if (length(grep(p1, b)) == 1 && length(grep("gains", b)) == 1 && length(grep("Province", b)) == 1) {
		p1score <- p1score + 6
		}
	if (length(grep(p1, b)) == 1 && length(grep("trashes", b)) == 1 && length(grep("Province", b)) == 1) {
		p1score <- p1score - 6
		}
}

#calculates P2 score
for (i in 1:length(dom)) {
	a <- strsplit(dom[i], " ")
	b <- unlist(a)
	if (length(grep(p2, b)) == 1 && length(grep("gains", b)) == 1 && length(grep("Estate", b)) == 1) {
		p2score <- p2score + 1
		}
	if (length(grep(p2, b)) == 1 && length(grep("trashes", b)) == 1 && length(grep("Estate", b)) == 1) {
		p1score <- p1score - 1
		}
	if (length(grep(p2, b)) == 1 && length(grep("gains", b)) == 1 && length(grep("Curse", b)) == 1) {
		p2score <- p2score - 1
		}
	if (length(grep(p2, b)) == 1 && length(grep("trashes", b)) == 1 && length(grep("Curse", b)) == 1) {
		p1score <- p1score + 1
		}
	if (length(grep(p2, b)) == 1 && length(grep("gains", b)) == 1 && length(grep("Duchy", b)) == 1) {
		p2score <- p2score + 3
		}
	if (length(grep(p2, b)) == 1 && length(grep("trashes", b)) == 1 && length(grep("Duchy", b)) == 1) {
		p1score <- p1score - 3
		}
	if (length(grep(p2, b)) == 1 && length(grep("gains", b)) == 1 && length(grep("Province", b)) == 1) {
		p2score <- p2score + 6
		}
	if (length(grep(p2, b)) == 1 && length(grep("trashes", b)) == 1 && length(grep("Province", b)) == 1) {
		p1score <- p1score - 6
		}

}


#count the number of cards in each person's deck
#number of cards for P1
for (i in 1:length(dom)) {
	a <- strsplit(dom[i], " ")
	b <- unlist(a)
	if (length(grep(p1, b)) == 1 && length(grep("gains", b)) == 1) {
		p1cards <- p1cards + 1
		}
	if (length(grep(p1, b)) == 1 && length(grep("trashes", b)) == 1) {
		p1cards <- p1cards - 1
		}
	
}

#number of cards for P2
for (i in 1:length(dom)) {
	a <- strsplit(dom[i], " ")
	b <- unlist(a)
	if (length(grep(p2, b)) == 1 && length(grep("gains", b)) == 1) {
		p2cards <- p2cards + 1
		}
	if (length(grep(p2, b)) == 1 && length(grep("trashes", b)) == 1) {
		p2cards <- p2cards - 1
		}
	
}

# Here we consider the complications introduced by the alternative VP card, Gardens.

#initial number of gardens
p1jardin <- 0
p2jardin <- 0

#finds number of gardens for both players at end of game
if (length(grep("Gardens",supply)) == 1) {
	for (i in 1:length(dom)) {
		a <- strsplit(dom[i], " ")
		b <- unlist(a)

		if (length(grep(p1, b)) == 1 && length(grep("gains", b)) == 1 && length(grep("Gardens", b)) == 1) {
		p1jardin <- p1jardin + 1
		}
		if (length(grep(p2, b)) == 1 && length(grep("trashes", b)) == 1 && length(grep("Gardens", b)) == 1) {
		p1jardin <- p1jardin - 1
		}

		if (length(grep(p2, b)) == 1 && length(grep("gains", b)) == 1 && length(grep("Gardens", b)) == 1) {
		p2jardin <- p2jardin + 1
		}
		if (length(grep(p2, b)) == 1 && length(grep("trashes", b)) == 1 && length(grep("Gardens", b)) == 1) {
		p2jardin <- p2jardin - 1
		}

	}
}

#converting jardins to VP

#calculates how much each garden is worth
p1jardinfactor <- floor(p1cards/10)
p1jardinscore <- p1jardin*p1jardinfactor
	
p2jardinfactor <- floor(p2cards/10)
p2jardinscore <- p2jardin*p2jardinfactor
	
#score adjustment to include VP from gardens
p1score <- p1score + p1jardinscore
p2score <- p2score + p2jardinscore 


# In this section, we attempt to track both the score and the number of cards for each player. 
# The aim is to create a graph showing how the score changed throughout the game.

#stores which lines are empty
blanks <- which(dom == " ")

#remove last element to avoid missing line error
blanks <- blanks[-length(blanks)]

#stores the position of all of the lines that begin with "Turn X" (also has extra values)
turns <- blanks + 1 

#intialise variables
#tracks score
p1count <- p1scorecounter
p2count <- p2scorecounter

#tracks numbers of cards
p1cardcount <- p1cardcounter
p2cardcount <- p2cardcounter

#tracks number of Gardens
p1jardincount <- 0
p2jardincount <- 0

#tracks how much a Gardens is worth
p1jardinfactor <- 1
p2jardinfactor <- 1


#goes through each turn tracking and recording the score and the number of cards of both players
for (i in 1:length(turns) - 1) {
	a <- strsplit(dom[turns[i]], " ")
	b <- unlist(a)

	if (length(grep("Game",b)) == 1 && length(grep("Over",b)) == 1) {
	break
	}

	if (length(grep(p1,b)) | length(grep(p2,b)) == 1) {
		p1count <- p1count
		for (j in blanks[i]:blanks[i+1]) {
			c <- strsplit(dom[j], " ")
			d <- unlist(c)
			#print(d)
			#p1 score per turn
			if (length(grep(p1,d)) == 1 && length(grep("gains",d)) == 1 &&length(grep("Estate",d)) == 1) {
				p1count <- p1count + 1
			}
			if (length(grep(p1,d)) == 1 && length(grep("trashes",d)) == 1 &&length(grep("Estate",d)) == 1) {
				p1count <- p1count - 1
			}

			if (length(grep(p1,d)) == 1 && length(grep("gains",d)) == 1 &&length(grep("Duchy",d)) == 1) {
				p1count <- p1count + 3
			}
			if (length(grep(p1,d)) == 1 && length(grep("trashes",d)) == 1 &&length(grep("Duchy",d)) == 1) {
				p1count <- p1count - 3
			}

			if (length(grep(p1,d)) == 1 && length(grep("gains",d)) == 1 &&length(grep("Province",d)) == 1) {
				p1count <- p1count + 6
			}
			if (length(grep(p1,d)) == 1 && length(grep("trashes",d)) == 1 &&length(grep("Province",d)) == 1) {
				p1count <- p1count - 6
			}

			if (length(grep(p1,d)) == 1 && length(grep("gains",d)) == 1 &&length(grep("Curse",d)) == 1) {
				p1count <- p1count - 1
			}
			if (length(grep(p1,d)) == 1 && length(grep("trashes",d)) == 1 &&length(grep("Curse",d)) == 1) {
				p1count <- p1count + 1
			}

			#counting p1 cards per turn
			if (length(grep(p1,d)) == 1 && length(grep("gains",d)) == 1) {
				p1cardcount <- p1cardcount + 1
			}
			if (length(grep(p1,d)) == 1 && length(grep("trashes",d)) == 1) {
				p1cardcount <- p1cardcount - 1
			}

			#accounting for Jardins per turn
	
			if (length(grep(p1,d)) == 1 && length(grep("gains",d)) == 1 &&length(grep("Gardens",d)) == 1) {
				p1jardincount <- p1jardincount + 1
				p1jardinfactor <- floor(tail(p1cardcounter, n=1)/10)
				p1count <- p1count + p1jardinfactor
			}
			if (length(grep(p1,d)) == 1 && length(grep("trashes",d)) == 1 &&length(grep("Gardens",d)) == 1) {
				p1jardincount <- p1jardincount - 1
				p1jardinfactor <- floor(tail(p1cardcounter, n=1)/10)
				p1count <- p1count - p1jardinfactor
			}

			# p2 score per turn
			if (length(grep(p2,d)) == 1 && length(grep("gains",d)) == 1 &&length(grep("Estate",d)) == 1) {
				p2count <- p2count + 1
			}
			if (length(grep(p2,d)) == 1 && length(grep("trashes",d)) == 1 &&length(grep("Estate",d)) == 1) {
				p2count <- p2count - 1
			}

			if (length(grep(p2,d)) == 1 && length(grep("gains",d)) == 1 &&length(grep("Duchy",d)) == 1) {
				p2count <- p2count + 3
			}
			if (length(grep(p2,d)) == 1 && length(grep("trashes",d)) == 1 &&length(grep("Duchy",d)) == 1) {
				p2count <- p2count - 3
			}

			if (length(grep(p2,d)) == 1 && length(grep("gains",d)) == 1 &&length(grep("Province",d)) == 1) {
				p2count <- p2count + 6
			}
			if (length(grep(p2,d)) == 1 && length(grep("trashes",d)) == 1 &&length(grep("Province",d)) == 1) {
				p2count <- p2count - 6
			}

			if (length(grep(p2,d)) == 1 && length(grep("gains",d)) == 1 &&length(grep("Curse",d)) == 1) {
				p2count <- p2count - 1
			}
			if (length(grep(p2,d)) == 1 && length(grep("trashes",d)) == 1 &&length(grep("Curse",d)) == 1) {
				p2count <- p2count + 1
			}

			# counting p2 cards per turn
			if (length(grep(p2,d)) == 1 && length(grep("gains",d)) == 1) {
				p2cardcount <- p2cardcount + 1
			}
			if (length(grep(p2,d)) == 1 && length(grep("trashes",d)) == 1) {
				p2cardcount <- p2cardcount - 1
			}

			if (length(grep(p2,d)) == 1 && length(grep("gains",d)) == 1 &&length(grep("Gardens",d)) == 1) {
				p2jardincount <- p2jardincount + 1
				p2jardinfactor <- floor(tail(p2cardcounter, n=1)/10)
				p2count <- p2count + p2jardinfactor
			}
			if (length(grep(p2,d)) == 1 && length(grep("trashes",d)) == 1 &&length(grep("Gardens",d)) == 1) {
				p2jardin2count <- p2jardincount - 1
				p2jardinfactor <- floor(tail(p2cardcounter, n=1)/10)
				p2count <- p2count - p2jardinfactor
			}

		}
	}


#if deck increases to a multiple of 10 (for Gardens)
if (floor(p1cardcount/10) != p1jardinfactor && p1jardincount > 0) {
	difference = floor(p1cardcount/10) - p1jardinfactor
	p1jardinfactor = floor(p1cardcount/10)
	p1count <- p1count + p1jardincount*difference
	}

if (floor(p2cardcount/10) != p2jardinfactor && p2jardincount > 0) {
	difference = floor(p2cardcount/10) - p2jardinfactor
	p2jardinfactor = floor(p2cardcount/10)
	p2count <- p2count + p2jardincount*difference
	}



p1scorecounter <- c(p1scorecounter, p1count)
p2scorecounter <- c(p2scorecounter, p2count)
p1cardcounter <- c(p1cardcounter, p1cardcount)
p2cardcounter <- c(p2cardcounter, p2cardcount)

} 

#remove initial value
p1scorecounter <- p1scorecounter[2:length(p1scorecounter)]
p2scorecounter <- p2scorecounter[2:length(p2scorecounter)]
p1cardcounter <- p1cardcounter[2:length(p1cardcounter)]
p2cardcounter <- p2cardcounter[2:length(p2cardcounter)]





#Graph both scores (rudimentary)

#g_range <- range(p1scorecounter, p2scorecounter)

#plot(p1scorecounter, type = "o", col = "blue", ylim = g_range, axes = F, ann = F)
#axis(1, at=1:length(p1scorecounter))
#axis(2, at=1:40)
#lines(p2scorecounter, type = "o", pch = 22, lty = 1, col = "green")
#title(main = "Victory points", col.main = "red", font.main = 4)
#title(xlab="Turns", col.lab=rgb(0,0.5,0))
#title(ylab="VP Score", col.lab=rgb(0,0.5,0))




#prints results
print(p1)
print(tail(p1scorecounter,n=1))
print("Cards in deck")
print(p1cards)
print("------------------------------")
print(p2)
print(tail(p2scorecounter,n=1))
print("Cards in deck")
print(p2cards)

#print(p1scorecounter)
#print(p2scorecounter)

#print(p1cardcounter)
#print(p2cardcounter)



	
		