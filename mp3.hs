import System.IO  
import Data.List.Split
import Control.Monad
import Data.Char



main = do  

		handle <- openFile "mp3files.txt" ReadWriteMode
		contents <- hGetContents handle
		let singlewords = words contents ;  
			splitNames = map (\a -> splitOn "%%" a) singlewords;
				mp3Tuple = linesToTupleList splitNames

		putStrLn "Commands"  
		putStrLn "display"  
		putStrLn "add"  
		putStrLn "remove"  
		putStrLn "searchTitle"
		putStrLn "searchArtist"		
		putStrLn "searchGenre"
		putStrLn "device"
		putStrLn "History"
				
		putStrLn "Enter Command"  
		command <- getLine	
		
		if(command=="display")
			then do display mp3Tuple
		else if(command=="add")
			then do add
		else if(command=="remove")
			then do remove 
		else if(command=="searchTitle")
			then do searchTitle mp3Tuple
		else if(command=="searchArtist")
			then do searchArtist mp3Tuple
		else if(command=="searchGenre")
			then do searchGenre mp3Tuple
		else if(command=="device")
			then do device mp3Tuple 
		else if(command=="history")
			then do history mp3Tuple 
			else do hClose handle
			
			

display xs = do
		printList xs
				
add = do 
		putStrLn "Enter Title"  
		title <- getLine
		
		putStrLn "Enter Artist"  
		artist <- getLine
					
		putStrLn "Enter Genre"  
		genre <- getLine
					
		putStrLn "Enter plays"  
		plays <- getLine
					
		putStrLn "Enter size"  
		size <- getLine
		
		let newContents = ("\n"++title ++ "%%" ++ artist++"%%"++genre++"%%"++plays++"%%"++size)
		
		appendFile "mp3files.txt" newContents
		
		
remove = do

		putStrLn "Enter Title To Remove"  
		removeTitle <- getLine
		putStrLn removeTitle
		
searchTitle xs = do

		putStrLn "Enter Title To Search"  
		searchedTitle <- getLine
		
		let results = filter(\(a,_,_,_,_)-> a == searchedTitle ) xs
		print results
		
searchArtist xs = do

		putStrLn "Enter Artist To Search"  
		searchedArtist <- getLine
		
		let results = filter(\(_,a,_,_,_)-> a == searchedArtist ) xs
		printList results
		
searchGenre xs = do

		putStrLn "Enter Genre To Search"  
		searchedGenre <- getLine
		
		let results = filter(\(_,_,a,_,_)-> a == searchedGenre ) xs
		printList results

		
device xs = do
		let deviceStorage = show deviceS
		putStr "Device Storage: "
		putStrLn deviceStorage
		
		putStr "Used Storage: "
		let usedStorage = sum(f( map lastEl xs))
		print usedStorage
		
		putStr "Available Storage: "
		let availableStorage = deviceS - usedStorage
		print availableStorage

history xs = do
		putStr "Most Played Song: "
		
		
		
linesToTupleList :: [[String]] -> [(String, String, String, String, String)]
linesToTupleList [] = []

linesToTupleList ((title:artist:genre:plays:size:[]):xs) = (title, artist, genre, plays, size ) : linesToTupleList xs
linesToTupleList (x:xs)	= linesToTupleList xs

printList xs  = mapM_ (\(title, artist, genre, plays, size ) -> putStrLn $ title ++ " " ++  artist++"  "++genre++"  "++plays++"  "++size) xs


getTitle:: (a, b, c, d ,e) -> (a)
getTitle (a, b, c, d ,e) = (a)

deviceS = 2000


lastEl:: (a, b, c, d ,e) -> (e)
lastEl (a, b, c, d ,e) = (e)

f :: [String] -> [Int]
f = map read
















