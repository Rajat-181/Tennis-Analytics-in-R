##Tennis Analytics - Improving Performance of Emerging Players

(Please note: I cannot upload the dataset files for this project as the data was obtained from a confidential client)

Improving the winning chances of emerging players for future matches by optimizing the decision variables
Business/ Research Problem Definition 

Over the past few years, sports analytics has penetrated many sports including NBA, NHL and NFL but most of these sports focus on team level dynamics. However, such analytics has become a standard and our analytics solution is based upon individual player statistics in Tennis and how the history of the performance of the players can be used to improve the winning chances of uprising players in future matches

Analytics Problem Definition

We are leveraging the data provided by one of the leading sports website and our team has developed a multiple linear regression model to compute the key decision variables. These variables are inputted in the decision support system to predict the player winning score of any player and compares it with the better ranking player. This functionality can be used by individual players/coaches and compare their statistics with target players, and find out the key areas of improvement to improve the chances of winning with the target player

Data 

Data for our analysis is taken from various API’s from a leading sports website. It consists various API’s with match level/player level statistics, top 500 ranking players of Association of Tennis Professionals(ATP) and Women’s Tennis Association(WPA). We prepare the final dataset aggregated on player level after performing the exploratory data analysis, necessary missing value computation using various algorithms, and scaling/normalization of the dataset. The key variables used in our final analysis are mentioned as follows: 

 
Methodology Building

Our purpose was to focus on the uprising players and help them to improve their winning chances in future matches. For this we initially thought of comparing each of the match level statistics of that player. But this would have created a bias of less data for few players based on the history of their played matches. Hence, we compared the features per match level statistics of all the players and computed the key variables

Model Building 

Our decision support system takes player level summaries as an input and predicts the winning score based on the selected values of the input variables. We have fitted multiple linear regression to compute the winning score.  The primary reason to go with the regression method was its interpretability to get the coefficients of the decisions variables which are used as in input in our R shiny application

Functionality

Part 1 Player Level Statistics (Fig: 1): The first part of our shiny applications displays the player level statistics and compares these statistics with the average of other players. This gives a notion of the current performance of the selected player compared to other players
Part 2 Predicting the winning score (Fig: 2): This part displays the actual and the predicted winning score of the selected variables. Different input variables can be tweaked to optimize the winning score and evaluate the metrices of improvement for the uprising player 

GUI Design and Quality 

The theme of our Shiny application is ‘Darkly’ and it was designed in accordance with various sports website with tangy colors and fonts. The User Interface of the shiny application enables a user to select various sliders, drop downs and see the various plots accordingly. After reviewing various R shiny application on RStudios.com, our design appears to be high quality and various steps (on top of the application) gives an overview of the usage of the application 

  
