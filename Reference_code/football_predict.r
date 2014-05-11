
library("DAAG")
library(randomForest)

football.process.v1 = function(datafile, fixtures, country, divname, season, teamsfile, winPoints = 3, drawPoints = 1, lossPoints = 0)
{

	if (missing(datafile))
	{
	stop("Results csv file not specified.")
	}
	if (missing(fixtures))
	{
	stop("fixtures csv file not specified.")
	}
	if (missing(teamsfile))
	{
	stop("teamsfile csv file not specified.")
	}
	if (missing(country))
	{
	warning("Country of league not specified.")
	country = ""
	}
	if (missing(divname))
	{
	warning("Name of league division not specified.")
	divname = ""
	}
	# import the data and columns of interest
	tmpResults  = read.csv(datafile)[,c("Date","HomeTeam","AwayTeam","FTR","FTHG","FTAG")]


        #
	tmpFixtures = read.csv(fixtures)[,c("Date","HomeTeam","AwayTeam")]

        tmpFixtures$FTR = "X" 
        tmpFixtures$FTHG = 0
        tmpFixtures$FTAG = 0
        tmpResults = rbind(tmpResults,tmpFixtures)

	#Then we check whether the team names have been specified by the user and if not
	#extract them from the data provided:
	#if (missing(teams))
	#{
	#warning("Team names not specified - extracted from results data.")
	#teams = sort(unique(c(as.character(tmpResults$HomeTeam), as.character(tmpResults$AwayTeam))))
	#}
        tmpteams = read.csv(teamsfile)[,c("team")]
        teams = tmpteams


	#We then convert the columns HomeTeam and AwayTeam into factors, which allows teams
	#that haven’t played a fixture yet to be included in the table.
        print("HOME TEAMS")
        print(unique(tmpResults$HomeTeam))
        print("AWAY TEAMS")
        print(unique(tmpResults$AwayTeam))
        #
        #
        print("Teams")
        print(teams)
	tmpResults$HomeTeam = factor(tmpResults$HomeTeam, levels = teams)
	tmpResults$AwayTeam = factor(tmpResults$AwayTeam, levels = teams)
        # need all dates

        uniquedate  = unique(tmpResults$Date)
        print(uniquedate) 
        xx = length(uniquedate)
        print("Number of unique date is XX")
        print(xx)

        #
        # add julian date (vector arithmetic)
        #
        tmpResults$juliandate <- as.numeric(as.Date(tmpResults$Date,format="%d/%m/%y",origin="01-01-1970"))

	julianuniquedate <- as.Date(uniquedate,format="%d/%m/%y")
	print (julianuniquedate)
     
        orig_tmpResults <- tmpResults
        print("Original fixture results")
        print(orig_tmpResults)


	## Create Empty League Table
	alltmpTable = data.frame(Team = teams, juliandate=0,Games = 0, Win = 0, Draw = 0, Loss = 0, HomeGames = 0, HomeWin = 0, HomeDraw = 0, HomeLoss = 0,
	AwayGames = 0, AwayWin = 0, AwayDraw = 0, AwayLoss = 0, Points = 0, HomeFor = 0, HomeAgainst = 0, AwayFor = 0, AwayAgainst = 0,
	For = 0, Against = 0, GoalDifference = 0
        )

	## Create tmp Empty League Table
	tmpTable  <- alltmpTable


#data.frame(Team = teams, juliandate=0,Games = 0, Win = 0, Draw = 0, Loss = 0, HomeGames = 0, HomeWin = 0, HomeDraw = 0, HomeLoss = 0,
#	AwayGames = 0, AwayWin = 0, AwayDraw = 0, AwayLoss = 0, Points = 0, HomeFor = 0, HomeAgainst = 0, AwayFor = 0, AwayAgainst = 0,
#	For = 0, Against = 0, GoalDifference = 0
#        )

	#To round off the first part of creating the result processing function we create a list
	#object to return at the end of the function.
        tmpSummary = list(Country = country, Division = divname, Season = season)

        for (i in 1:xx)
        {
                print("FOR NEXT data....")
                print(uniquedate[i])



		#
		#  subset data
		#
		print("FILTERED DATA FRAME SUBSET")
		tmpResults = subset(orig_tmpResults,juliandate < julianuniquedate[i])
		tmpResults = subset(tmpResults,FTR != "X")

                print(tmpResults)

                #
                # setup temp results
                #
                tmpTable$juliandate = julianuniquedate[i]

                
		#########
		# PART 2
		#########

		## Count Number of Games Played
		tmpTable$HomeGames = as.numeric(table(tmpResults$HomeTeam))
		tmpTable$AwayGames = as.numeric(table(tmpResults$AwayTeam))


		# 
		# print together
		#
		print (table(tmpResults$HomeTeam))

		## Count Number of Wins/Draws/Losses
		tmpTable$HomeWin = as.numeric(table(tmpResults$HomeTeam[tmpResults$FTR == "H"]))
		tmpTable$HomeDraw = as.numeric(table(tmpResults$HomeTeam[tmpResults$FTR == "D"]))
		tmpTable$HomeLoss = as.numeric(table(tmpResults$HomeTeam[tmpResults$FTR == "A"]))
		tmpTable$AwayWin = as.numeric(table(tmpResults$AwayTeam[tmpResults$FTR == "A"]))
		tmpTable$AwayDraw = as.numeric(table(tmpResults$AwayTeam[tmpResults$FTR == "D"]))
		tmpTable$AwayLoss = as.numeric(table(tmpResults$AwayTeam[tmpResults$FTR == "H"]))
		tmpTable$Games = tmpTable$HomeGames + tmpTable$AwayGames
		tmpTable$Win = tmpTable$HomeWin + tmpTable$AwayWin
		tmpTable$Draw = tmpTable$HomeDraw + tmpTable$AwayDraw
		tmpTable$Loss = tmpTable$HomeLoss + tmpTable$AwayLoss
		tmpTable$Points = winPoints * tmpTable$Win + drawPoints * tmpTable$Draw + lossPoints * tmpTable$Loss

		## Count Goals Scored and Conceeded
		tmpTable$HomeFor = as.numeric(tapply(tmpResults$FTHG, tmpResults$HomeTeam, sum, na.rm = TRUE))
		tmpTable$HomeAgainst = as.numeric(tapply(tmpResults$FTAG, tmpResults$HomeTeam, sum, na.rm = TRUE))
		tmpTable$AwayFor = as.numeric(tapply(tmpResults$FTAG, tmpResults$AwayTeam, sum, na.rm = TRUE))
		tmpTable$AwayAgainst = as.numeric(tapply(tmpResults$FTHG, tmpResults$AwayTeam, sum, na.rm = TRUE))

		#The ifelse statement is used to handle situations where a team hasn’t played a home and/or away fixture yet.
		tmpTable$For = ifelse(is.na(tmpTable$HomeFor), 0, tmpTable$HomeFor) +
		   ifelse(is.na(tmpTable$AwayFor), 0, tmpTable$AwayFor)
		tmpTable$Against = ifelse(is.na(tmpTable$HomeAgainst), 0, tmpTable$HomeAgainst) +
		   ifelse(is.na(tmpTable$AwayAgainst), 0, tmpTable$AwayAgainst)
		# calculate the goal difference
		tmpTable$GoalDifference = tmpTable$For - tmpTable$Against

                 print("Partial results table")
                 print(tmpTable)
                 alltmpTable = rbind(alltmpTable,tmpTable)
         }


	## Sort Table
	## By Points
	## By Goal Difference
	## By Team Name (Alphabetical)
	alltmpTable = alltmpTable[order(alltmpTable$juliandate,- alltmpTable$Points, - alltmpTable$GoalDifference, alltmpTable$Team),]
	alltmpTable = alltmpTable[,c("juliandate","Team", "Games", "Win", "Draw", "Loss", "Points", "For", "Against", "GoalDifference")]
        print("Concatenated results table")
        print(alltmpTable)


        #
        print("Start  join S1")
        S1  <- merge(orig_tmpResults,alltmpTable,by.x=c("AwayTeam","juliandate" ),by.y=c("Team","juliandate"))
        print(S1)
        print("Start  join S2")
        S2  <- merge(S1,alltmpTable,by.x=c("HomeTeam","juliandate" ),by.y=c("Team","juliandate"))
        print("Start  join")

        S2[is.na(S2)] <- 0     # repalce all na with zero?

        print("Random forest additional variables model")
        S2$Goalsdiff <- S2$GoalDifference.x  - S2$GoalDifference.y
        S2$pointsdiff <- S2$Points.x  - S2$Points.y

        print("PRINT S2")
        print(S2)

        print("Load model")
        load("models\\football_ctree.Rdata")     

        #
  
        print("predict vector")
        predictions <- predict(football_ctree,newdata=S2,type="response")
        print(predictions)

        print("RESULTS")
        S3 <- cbind(S2,predictedresults=predictions)
        S3 = S3[order(S3$juliandate,S3$HomeTeam),]
        print(S3[,c("Date","HomeTeam","AwayTeam","FTR","predictedresults")])

	# Return Division Information
	tmpSummary = list(Country = country, Division = divname, Season = season)
	invisible(tmpSummary)
        
} # end function


###
###
###


#
# MAIN
# The order is important for calcualting the league table.
# The table function will produce tabulated calculations in same order as teams listed!!!
#
#####################################################################################################
hardcopy(width = 3.75, height = 3.75, color = FALSE, trellis = FALSE,
                 device = c("pdf"), path = getwd(), file = "output\\hardcopy_football_predict.pdf"
                 , format = c("nn-nn", "name"), split = "\\.",
                 pointsize = c(8, 4), fonts=NULL, horiz = FALSE)


#
#
print(football.process.v1("data\\E0_2013.csv", "data\\fixture_2013_2014.csv","England", "Premiership", "2013-2014", "data\\E0teams_2013.csv"))

