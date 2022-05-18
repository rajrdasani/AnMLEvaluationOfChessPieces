The four files included in the data folder are the following:

- chess_games.csv: LiChess game database with 20,000 games, downloaded from Kaggle (original link here: https://www.kaggle.com/datasets/datasnaek/chess)
- chess_games_analyzed.csv: chess_games.csv, with the ScriptAnalysisFunction.R ran on it, outputting the number of moves, number of checks, pieces taken, value of pieces taken as columns for each piece (pawn, knight, bishop, rook, queen) for white and black. Ran, created, and saved via the AggregateScriptAnalysis.Rmd notebook.
- chess_summ.csv: chess_games_analyzed.csv but with only the relevant columns (all the added columns, player rating, game outcome)
- chess_games_pieces_remaining.csv: Used for Model 2 and created via the Model2_LogOdds.Rmd script (commented at the end of it). Contains the remaining pieces for each player as columns for every game in the dataset
