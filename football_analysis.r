
library("DAAG")
library(randomForest)

library("sqldf")
library(gridExtra) 

options( warn = 2 )
football.process.v1 = function(datafile, country, divname, season, teamsfile, fixtures,winPoints = 3, drawPoints = 1, lossPoints = 0)
{

	if (missing(datafile))
	{
	stop("Results csv file not specified.")
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
	if (missing(fixtures))
	{
	warning("Fixture list not provided")
	fixtures = ""
	}

        ###############################
        # Div = League Division
        # Date = Match Date (dd/mm/yy)
        # HomeTeam = Home Team
        # AwayTeam = Away Team
        # FTHG = Full Time Home Team Goals
        # FTAG = Full Time Away Team Goals
        # FTR = Full Time Result (H=Home Win, D=Draw, A=Away Win)
        # HTHG = Half Time Home Team Goals
        # HTAG = Half Time Away Team Goals
        # HTR = Half Time Result (H=Home Win, D=Draw, A=Away Win)

        # Match Statistics (where available)
        # Attendance = Crowd Attendance
        # Referee = Match Referee
        # HS = Home Team Shots
        # AS = Away Team Shots
        # HST = Home Team Shots on Target
        # AST = Away Team Shots on Target
        # HHW = Home Team Hit Woodwork
        # AHW = Away Team Hit Woodwork
        ##########################################

	# import the data and columns of interest
        # These are resulted matches sofar
	tmpResults = read.csv(datafile)[,c("Date","HomeTeam","AwayTeam","FTR","FTHG","FTAG","HS","AS","HST","AST")]
        print("tmpResults - results sofar")
        print(tmpResults)

        #
        # should be filtered by max date of results sofar
        ##################################################
        print("tmpResults - and fixtures form all fixture list")
        print(tmpResults)

        sql_string <-  "SELECT
                DISTINCT tmpResults.HomeTeam, tmpResults.AwayTeam,tmpResults.HomeTeam||tmpResults.AwayTeam
                FROM tmpResults
                ORDER BY tmpResults.HomeTeam,tmpResults.AwayTeam
                "
        sqlAlreadyResults <- sqldf(sql_string,stringsAsFactors = FALSE)
        print("sqlAleadyResults......")
        print(sqlAlreadyResults)
        

        ##################################################
        #
        ##################################################
        if (datafile != fixtures)
        {
                print("CONCATENATE THE FIXTURES FOR THE YEAR...")
		tmpFixtures = read.csv(fixtures)[,c("Date","HomeTeam","AwayTeam")]

		tmpFixtures$FTR = "X" 
		tmpFixtures$FTHG = 0
		tmpFixtures$FTAG = 0
		tmpFixtures$HS = 0
		tmpFixtures$AS = 0
		tmpFixtures$HST = 0
		tmpFixtures$AST = 0

                ##################################################
                #
                ##################################################
                sql_string2 <- "SELECT
                          tmpFixtures.*
                         FROM   tmpFixtures
                         WHERE (tmpFixtures.HomeTeam||tmpFixtures.AwayTeam) NOT IN
                         (
                           SELECT sqlAlreadyResults.HomeTeam||sqlAlreadyResults.AwayTeam
                           FROM   sqlAlreadyResults
                         )
                         "

                 sqltmpResults <- sqldf(sql_string2,stringsAsFactors = FALSE)
                 print("sqltmpResults......")
                 print(sqltmpResults)

		tmpResults = rbind(tmpResults,sqltmpResults)
        }

        print("tmpResults ... all matches")
        print(tmpResults)

        #########################################################################################
	#Then we check whether the team names have been specified by the user and if not
	#extract them from the data provided:
	#if (missing(teams))
	#{
	#warning("Team names not specified - extracted from results data.")
	#teams = sort(unique(c(as.character(tmpResults$HomeTeam), as.character(tmpResults$AwayTeam))))
	#}
        #########################################################################################
        tmpteams = read.csv(teamsfile)[,c("team")]
        teams = tmpteams


	#We then convert the columns HomeTeam and AwayTeam into factors, which allows teams
	#that haven’t played a fixture yet to be included in the table.
        print("HOME TEAMS")
        print(unique(tmpResults$HomeTeam))
        print("AWAY TEAMS")
        print(unique(tmpResults$AwayTeam))

	#We then convert the columns HomeTeam and AwayTeam into factors, which allows teams
	#that haven’t played a fixture yet to be included in the table.
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

        #
        # copy full orginal fixture list
        orig_tmpResults <- tmpResults
        print("Original fixture results")
        print(orig_tmpResults) 


	## Create Empty League Table
        # All new elements must be here...
        # not very convienient
        #########################################################
	alltmpTable = data.frame(Team = teams, juliandate=0,Games = 0, Win = 0, Draw = 0, Loss = 0, HomeGames = 0, 
        HomeWin = 0, HomeDraw = 0, HomeLoss = 0,
	AwayGames = 0, 
        AwayWin = 0, AwayDraw = 0, AwayLoss = 0, 
        Points = 0, 
        Pointsrankf = 0,
        HomeFor = 0, HomeAgainst = 0, AwayFor = 0, AwayAgainst = 0,
	For = 0, Against = 0, 
        GoalDifference = 0, 
        GoalDifferencef = 0, 
        HomeShots = 0,AwayShots = 0,HomeShotsOnTarget = 0,AwayShotsOnTarget = 0,
        fperHomeWin=0, fperHomeDraw=0, fperHomeLoss=0, fperAwayWin=0, fperAwayDraw=0, fperAwayLoss=0,
        HomeGamesShotsf=0,AwayGamesShotsf=0,
        Points30f=0,
        HomeWinpoint30sf=0,
        AwayWinpoint30sf=0
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

        #
        # build a version of league table for date where we have a football match
        #
        #################################################################################
        for (i in 1:xx)
        {
                print("DATES TO CONSIDERED")
	        backstop <- as.Date(uniquedate[i],format="%d/%m/%y") -30
                print(backstop)
                print(uniquedate[i])
                backstopdate <- as.Date(backstop,format="%d/%m/%y")
                print(backstopdate)


		#
		#  subset data
		#
		print("FILTERED DATA FRAME SUBSET")
		tmpResults = subset(orig_tmpResults,juliandate <  julianuniquedate[i])     # filter fixtures on date
                tmpResults = subset(tmpResults,FTR != "X")                                 # in actual data future results not known
		print(tmpResults)

		print("FILTERED DATA FRAME SUBSET (short short term n days)")
		tmpResults30 = subset(orig_tmpResults,(juliandate > backstopdate & juliandate <  julianuniquedate[i]))     # filter fixtures on date
                tmpResults30 = subset(tmpResults30,FTR != "X")                                 # in actual data future results not known
		print(tmpResults30)
              
                #############################################
                # set date of this league table
                #############################################
                tmpTable$juliandate = julianuniquedate[i]

                ##################################################
                #   Grap a tail of dataframe for short term view
                ##################################################



		#########
		# PART 2
		#########

		## Count Number of Games Played
                # use tabulate to count
		tmpTable$HomeGames = as.numeric(table(tmpResults$HomeTeam))
		tmpTable$AwayGames = as.numeric(table(tmpResults$AwayTeam))
		# 
		# print together
		#
                print("tmpReuslts$HomeTeam")
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

                ## 
                # result filtered between two dates for SHORT TERM factor(30 days)
                #
		#  HomeWin30 = as.numeric(table(tmpResults30$HomeTeam[tmpResults30$FTR == "H"]))
		HomeGames30 = as.numeric(table(tmpResults30$HomeTeam))
		AwayGames30 = as.numeric(table(tmpResults30$AwayTeam))
		HomeWin30 = as.numeric(table(tmpResults30$HomeTeam[tmpResults30$FTR == "H"]))
		HomeDraw30 = as.numeric(table(tmpResults30$HomeTeam[tmpResults30$FTR == "D"]))
		HomeLoss30 = as.numeric(table(tmpResults30$HomeTeam[tmpResults30$FTR == "A"]))
		AwayWin30 = as.numeric(table(tmpResults30$AwayTeam[tmpResults30$FTR == "A"]))
		AwayDraw30 = as.numeric(table(tmpResults30$AwayTeam[tmpResults30$FTR == "D"]))
		AwayLoss30 = as.numeric(table(tmpResults30$AwayTeam[tmpResults30$FTR == "H"]))
		Games30 = HomeGames30 + AwayGames30
		Win30 = HomeWin30 + AwayWin30
		Draw30 = HomeDraw30 + AwayDraw30
		Loss30 = HomeLoss30 + AwayLoss30
		Points30 = winPoints * Win30 + drawPoints * Draw30 + lossPoints * Loss30
                #
                mypointsbreaks = c(-1,0,3,6,9,12,15,18,1000)
                HomeWinpoint30s = HomeWin30 * winPoints
                AwayWinpoint30s = AwayWin30 * winPoints
                tmpTable$HomeWinpoint30sf <- as.numeric(cut(HomeWinpoint30s,mypointsbreaks))
                tmpTable$AwayWinpoint30sf <- as.numeric(cut(AwayWinpoint30s,mypointsbreaks))
                #
                print("HomeWin30")
                print(HomeWin30)
                tmpTable$Points30f <- as.numeric(cut(Points30,mypointsbreaks))

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

                #
		# calculate the goal difference(needs to be replicated for predictions
                #########################################################################
		tmpTable$GoalDifference = tmpTable$For - tmpTable$Against

                #
                # accumulate the HS,AS,HST,AST -  shots
                ##################################################
                tmpTable$HomeShots         = as.numeric(tapply(tmpResults$HS, tmpResults$HomeTeam, sum, na.rm = TRUE))
                tmpTable$HomeShotsOnTarget = as.numeric(tapply(tmpResults$HST, tmpResults$HomeTeam, sum, na.rm = TRUE))
                tmpTable$AwayShots         = as.numeric(tapply(tmpResults$AS, tmpResults$AwayTeam, sum, na.rm = TRUE))
                tmpTable$AwayShotsOnTarget = as.numeric(tapply(tmpResults$AST, tmpResults$AwayTeam, sum, na.rm = TRUE))

                HomeGamesShots = tmpTable$HomeShotsOnTarget / tmpTable$HomeGames
                AwayGamesShots = tmpTable$AwayShotsOnTarget / tmpTable$AwayGames
                myshbreaks = c(-1,0,5,10,15,20,25,40,60,70,1000)
                tmpTable$HomeGamesShotsf <- as.numeric(cut(HomeGamesShots,myshbreaks))
                tmpTable$AwayGamesShotsf <- as.numeric(cut(AwayGamesShots,myshbreaks))

                #
                # GENERATE EXTRA VARIABLESI(HERE ON MINI LEAGUE TABLES
                # Random foresst only like numerics, so CUT banding to numerics.
                ################################################################
                tmpTable[is.na(tmpTable)] <- 0     # repalce all na with zero?
                mygdbreaks = c(-1000,-40,-20,-10,0,10,20,40,50,1000)
                tmpTable$GoalDifferencef <- as.numeric(cut(tmpTable$GoalDifference,mygdbreaks))
#
                myprbreaks = c(-1,0,1,5,7,10,15,20,25,30,40)
                Pointsrank <- as.numeric(factor(tmpTable$Points))     # rank...
                tmpTable$Pointsrankf <- as.numeric(cut(Pointsrank,myprbreaks))
#
                #
                # percentage of results from totals : also set ranges
                # define breakpoints
                # x.breaks = c(0, 0.4, 0.5, 0.6, 1.0)
                # x.factornames = c( "0 - 0.4", "0.4 - 0.5", "0.5 - 0.6", "0.6 - 1.0" )
                #  x.factor = cut( x, x.breaks, x.factornames )
                #  table(x.factor)
                ############################################################################
                mybreaks = c(-1,0,0.2,0.3,0.4,0.5,0.6,0.7,0.8,1.0)
                #
                # Home Team Form
                ########################################
                perHomeWin   <- tmpTable$HomeWin / (tmpTable$HomeWin+ tmpTable$HomeDraw + tmpTable$HomeLoss)
                perHomeDraw  <- tmpTable$HomeDraw / (tmpTable$HomeWin+ tmpTable$HomeDraw+ tmpTable$HomeLoss)
                perHomeLoss  <- tmpTable$HomeLoss / (tmpTable$HomeWin+ tmpTable$HomeDraw+ tmpTable$HomeLoss)
                perAwayWin   <- tmpTable$AwayWin / (tmpTable$AwayWin+ tmpTable$AwayDraw  +tmpTable$AwayLoss)
                perAwayDraw  <- tmpTable$AwayDraw / (tmpTable$AwayWin+ tmpTable$AwayDraw +tmpTable$AwayLoss)
                perAwayLoss  <- tmpTable$AwayLoss / (tmpTable$AwayWin+ tmpTable$AwayDraw +tmpTable$AwayLoss)

               perHomeWin[is.na(perHomeWin)] <- 0
               perHomeDraw[is.na(perHomeDraw)] <- 0
               perHomeLoss[is.na(perHomeLoss)] <- 0
               perAwayWin[is.na(perAwayWin)] <- 0
               perAwayDraw[is.na(perAwayDraw)] <- 0
               perAwayLoss[is.na(perAwayLoss)] <- 0


               perHomeWin[perHomeWin==0.0] <- 0.01     # replace
               perHomeDraw[perHomeDraw==0.0] <- 0.01     # replace
               perHomeLoss[perHomeLoss==0.0] <- 0.01     # replace
               perAwayWin[perAwayWin==0.0] <- 0.01     # replace
               perAwayDraw[perAwayDraw==0.0] <- 0.01     # replace
               perAwayLoss[perAwayLoss==0.0] <- 0.01     # replace

               tmpTable$fperHomeWin <- as.numeric(cut(perHomeWin,mybreaks))
               tmpTable$fperHomeDraw <- as.numeric(cut(perHomeDraw,mybreaks))
               tmpTable$fperHomeLoss <- as.numeric(cut(perHomeLoss,mybreaks))
               tmpTable$fperAwayWin <- as.numeric(cut(perAwayWin,mybreaks))
               tmpTable$fperAwayDraw <- as.numeric(cut(perAwayDraw,mybreaks))
               tmpTable$fperAwayLoss <- as.numeric(cut(perAwayLoss,mybreaks))


               print("END Generate addition variables")
               #
	       # P A R T I A L   Results 
               #  Rbind requires that tables have the same number of columns
               ###############################################################
               #print(tmpTable)
               ##################################################
               #
               ##################################################
               sql_string3 <- "SELECT
                          tmpTable.*
                         FROM   tmpTable
                         ORDER BY tmpTable.Points DESC
                         "

                ordertmpTable <- sqldf(sql_string3,stringsAsFactors = FALSE)
                print("ordertmpTable......")
                print("Partial results table for specific date...")
                print(ordertmpTable)

               #print("Partial results table alltmpTable")
               #print(alltmpTable)


               alltmpTable = rbind(alltmpTable,tmpTable)
         }


	## Sort Table
	## By Points
	## By Goal Difference
	## By Team Name (Alphabetical)
	alltmpTable = alltmpTable[order(alltmpTable$juliandate,- alltmpTable$Points, - alltmpTable$GoalDifference, alltmpTable$Team),]
	alltmpTable = alltmpTable[,c("juliandate","Team", "Games", "Win", "Draw", "Loss", 
                                     "Points", 
                                     "Pointsrankf",
                                     "For", "Against", 
                                     "GoalDifference",
                                     "GoalDifferencef",
                                     "HomeWin","HomeDraw","HomeLoss","AwayWin","AwayDraw","AwayLoss",
                                     "HomeShots" ,"AwayShots" ,"HomeShotsOnTarget" ,"AwayShotsOnTarget" ,
                                     "fperHomeWin","fperHomeDraw","fperHomeLoss","fperAwayWin","fperAwayDraw","fperAwayLoss",
                                     "HomeGamesShotsf","AwayGamesShotsf",
                                     "Points30f",
                                     "HomeWinpoint30sf",
                                     "AwayWinpoint30sf"
                                 )]
        #print(alltmpTable)

        #
        # join multiple date league table to fixture list
        # this way can predict future results using the most up to date league table info
        # Even if the last offical result where from months ago
        #################################################################################
        print("COmplete join of three tables")
        S1  <- merge(orig_tmpResults,alltmpTable,by.x=c("AwayTeam","juliandate" ),by.y=c("Team","juliandate"))
        S2  <- merge(S1,alltmpTable,by.x=c("HomeTeam","juliandate" ),by.y=c("Team","juliandate"))

        S2[is.na(S2)] <- 0     # repalce all na with zero?
        print("PRINT S2")
        print(S2)
	# Return Division Information
        return(S2)
        
} # end function



football.model.v1 = function(S2)
{
print("FINAL BEFORE PREDICT str(S2)")
#str(S2)    #print structur of data frame

        football_ctree <- randomForest( FTR ~ 
                             ### Goalsdiff + pointsdiff + 
                             fperHomeWin.x+ fperHomeDraw.x+ fperHomeLoss.x+
                             fperAwayWin.x+ fperAwayDraw.x+ fperAwayLoss.x+
                             fperHomeWin.y+ fperHomeDraw.y+ fperHomeLoss.y+
                             fperAwayWin.y+ fperAwayDraw.y+ fperAwayLoss.y+
                             GoalDifferencef.x +
                             GoalDifferencef.y +
                             Pointsrankf.x +
                             Pointsrankf.y +
                             HomeGamesShotsf.x +  
                             AwayGamesShotsf.x +  
                             HomeGamesShotsf.y +  
                             AwayGamesShotsf.y +
                             Points30f.x +
                             Points30f.y +
                             HomeWinpoint30sf.x +
                             AwayWinpoint30sf.x +
                             HomeWinpoint30sf.y +
                             AwayWinpoint30sf.y 
                             , data=S2,xtest=NULL,ntree=200,proximity=TRUE)

        pdf("output\\Analysis_matrix.pdf") 
        print("print Random forest Summary ... ")
        print(football_ctree)
        varImpPlot(football_ctree)
        print("plot Random forest ")
        print(plot(football_ctree))

        print("importance Random forest ")
        #text(football_ctree)
        print(importance(football_ctree))


        #print("predict vector")
        #probabilities <- predict(football_ctree,type="prob")

        print("values and model summary")
#        print(S2$FTR)
        print(football_ctree)
     
        dev.off()

        return (football_ctree) 
}


football.predict.v1 = function(football_ctree,S2)
{ 
        pdf("output\\Analysis_confusion_matrix.pdf") 
        print("INSIDE PREDICT ... ")
        print("predict table ... confusion matrix")
        print(table(predict(football_ctree),S2$FTR))
        dev.off()
#
# 
        print("predict vector")
        predictions <- predict(football_ctree,type="response")
        print(predictions)
#
#
        print("RESULTS with predicted values")
        S3 <- cbind(S2,PFTR=predictions)
        S3 = S3[order(S3$juliandate,S3$HomeTeam),]
        print(S3[,c("Date","HomeTeam","AwayTeam","FTR","PFTR")])
} 
###
###
###

  
football.predict_newdata.v1 = function(football_ctree,S2)
{ 
        print("INSIDE PREDICT NEWDATA...")
        print("predict vector")
        print(football_ctree)
        predictions <- predict(football_ctree,newdata=S2,type="response")
        print("predict vector complete")
        print(predictions)

        print("RESULTS")
        S3 <- cbind(S2,PFTR=predictions)
        S3 = S3[order(S3$juliandate,S3$HomeTeam),]
        print(S3[,c("Date","HomeTeam","AwayTeam","FTR","PFTR")])

        #
        S4 <-S3[,c("Date","HomeTeam","AwayTeam","FTR","PFTR")]
        #
        #pdf("output\\Projected_Results.pdf", height=11, width=8.5) 
        maxrow = 30; 
        npages = ceiling(nrow(S4)/maxrow); 
        pdf("output\\Projected_Results.pdf", height=11, width=8.5); 
        for (i in 1:npages) {
            idx = seq(1+((i-1)*maxrow), i*maxrow); 
            grid.newpage(); 
            grid.table(S4[idx, ],gp=gpar(fontsize=10))
        }; 
        dev.off()

#        pdf("output\\Projected_Results.pdf") 
#        grid.table(S4,gp=gpar(fontsize=6)) 
#        dev.off()

        #
        return(S3)
}

football.final_league_table.v1 = function(S3,teamsfile,winPoints = 3, drawPoints = 1, lossPoints = 0)
{
        print("Final predicted league table")
        print(S3[,c("Date","HomeTeam","AwayTeam","FTR","PFTR")])
        tmpteams = read.csv(teamsfile)[,c("team")]
        teams = tmpteams

	## Create Empty League Table
        # All new elements must be here...
        # not very convienient
        #########################################################
	tmpTable = data.frame(Team = teams, juliandate=0,Games = 0, Win = 0, Draw = 0, Loss = 0, HomeGames = 0, 
                              HomeWin = 0, HomeDraw = 0, HomeLoss = 0,
	                      AwayGames = 0, 
                              AwayWin = 0, AwayDraw = 0, AwayLoss = 0, 
                              Points = 0, 
                              GoalDifference = 0 
        )
	tmpTable$HomeGames = as.numeric(table(S3$HomeTeam))
	tmpTable$AwayGames = as.numeric(table(S3$AwayTeam))

	## Count Number of Wins/Draws/Losses
        ## IF FTR is 'X' use predicted PFTR
        ##
        # vector ifelse not returning correct value???
        S3$FTR <- ifelse (S3$FTR == "X", as.character(S3$PFTR) ,as.character(S3$FTR) )

        #print(S3[,c("FTR","PFTR")])

        ##
        ## 
	tmpTable$HomeWin  = as.numeric(table(S3$HomeTeam[S3$FTR == "H" ]))
	tmpTable$HomeDraw = as.numeric(table(S3$HomeTeam[S3$FTR == "D" ]))
	tmpTable$HomeLoss = as.numeric(table(S3$HomeTeam[S3$FTR == "A" ]))
	tmpTable$AwayWin  = as.numeric(table(S3$AwayTeam[S3$FTR == "A" ]))
	tmpTable$AwayDraw = as.numeric(table(S3$AwayTeam[S3$FTR == "D" ]))
	tmpTable$AwayLoss = as.numeric(table(S3$AwayTeam[S3$FTR == "H" ]))

	tmpTable$Games  = tmpTable$HomeGames + tmpTable$AwayGames
	tmpTable$Win    = tmpTable$HomeWin + tmpTable$AwayWin
	tmpTable$Draw   = tmpTable$HomeDraw + tmpTable$AwayDraw
	tmpTable$Loss   = tmpTable$HomeLoss + tmpTable$AwayLoss
	tmpTable$Points = winPoints * tmpTable$Win + drawPoints * tmpTable$Draw + lossPoints * tmpTable$Loss

        tmpTable = tmpTable[order(tmpTable$Points),]
        sql_string <-  "SELECT
                Team,HomeWin,HomeDraw,HomeLoss,AwayWin,AwayDraw,AwayLoss,Games,Win,Draw,Loss,Points 
                FROM tmpTable
                ORDER BY Points DESC 
                "
        sqlAlreadyResults <- sqldf(sql_string,stringsAsFactors = FALSE)
        print("sqlAleadyResults......")
        print(sqlAlreadyResults)

        #pdf("output\\Projected_league_table.pdf", height=11, width=8.5) 
        pdf("output\\Projected_league_table.pdf") 
        grid.table(sqlAlreadyResults,gp=gpar(fontsize=6)) 
        dev.off()
}

#
# MAIN
# The order is important for calcualting the league table.
# The table function will produce tabulated calculations in same order as teams listed!!!
#
#####################################################################################################

print("MYARGS")
args <- commandArgs(trailingOnly = TRUE)
print(args[1]) # datafile
print(args[2]) # teams
print(args[3]) # country
print(args[4]) # div
print(args[5]) # year
print(args[6]) # fixtures
print(args[7]) # Analysis or predict

# trailingOnly=TRUE means that only arguments after --args are returned
# if trailingOnly=FALSE then you got:
# [1] "--no-restore" "--no-save" "--args" "2010-01-28" "example" "100"




#hardcopy(width = 3.75, height = 3.75, color = FALSE, trellis = FALSE,
#                 device = c("pdf"), path = getwd(), file = "output\\hardcopy_football_analysis.pdf"
#                 , format = c("nn-nn", "name"), split = "\\.",
#                 pointsize = c(8, 4), fonts=NULL, horiz = FALSE)


#
#
#S2<-football.process.v1("data\\E0_2012.csv", "England", "Premiership", "2010-2011", "data\\E0teams_2012.csv")
S2<-football.process.v1(args[1], args[3],args[4], args[5], args[2],args[6])


if (args[7] == "ANALYSIS")
{
	print("START ANALYSIS")
        print(S2)
	football_ctree = football.model.v1(S2)
	print("Save model")
	save(football_ctree,file = "models\\football_ctree.Rdata") 

	print("Load model")
	load("models\\football_ctree.Rdata")   

	football.predict.v1 (football_ctree,S2)
}

if (args[7] == "PREDICT")
{
	print("START PREDICT")
	print("Load model")
	load("models\\football_ctree.Rdata")   
        print("S2")
        print(S2)

	S4 = football.predict_newdata.v1 (football_ctree,S2)
        football.final_league_table.v1(S4,args[2]) 
}

