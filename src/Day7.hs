module Day7 (processPart1,processPart2) where

import Data.List (sort, notElem, nub, foldr, group, delete)
import qualified Data.Heap as Heap
import Data.Maybe (listToMaybe)

type Task = Char
type Graph = [ (Char, Char) ]
data Project =
  Project { graph :: Graph, tasks :: [Task] }


sortAndRemoveDuplicate :: Ord a => [a] -> [a]
sortAndRemoveDuplicate =
  map head
  . group
  . sort

parseLine :: String -> (Task, Task)
parseLine text =
  (text !! 5, text !! 36)


vertices :: Graph -> [ Task ]
vertices =
  sortAndRemoveDuplicate
  . concat
  . map (\(s,t) -> [s,t])


parseProject :: String -> Project
parseProject =
  toProject
  . map parseLine
  . lines
  where
    toProject arcs = Project arcs (vertices arcs)

isAvailable :: Project -> Task -> Bool
isAvailable project = flip notElem (map snd $ graph project)
  

findSmallestAvailableTask :: Project -> Maybe Task
findSmallestAvailableTask project =
  listToMaybe . filter (isAvailable project) . tasks $ project


deliverTask :: Project -> Task ->  Project
deliverTask project task =
  Project
    (filter ((/= task) . fst) $ graph project)
    (tasks project)

startTask :: Project -> Task -> Project
startTask project task =
  Project
    (graph project)
    (delete task $ tasks project)

removeTask :: Project -> Task -> Project
removeTask project task =
  flip deliverTask task $ startTask project task




findTopologicalOrder :: Project -> [ Task ]
findTopologicalOrder project = 
  case findSmallestAvailableTask project of
    Nothing -> []
    Just nextTask ->
      nextTask : findTopologicalOrder (removeTask project nextTask)


    


-- JNOIKSYABEQRUVWXGTZFDMHLPC


processPart1 :: String -> String
processPart1 =
  findTopologicalOrder . parseProject


 
data Event =
  Event { time :: Int, task :: Char }
  deriving Show

instance Eq Event where
  ev1 == ev2 =
    time ev1 == time ev2 && task ev1 == task ev2
instance Ord Event where
  ev1 <= ev2 = time ev1 < time ev2 || (time ev1 == time ev2 && task ev1< task ev2)
  
type ScheduledTask = (Char, Int)


taskLength :: Task -> Int
taskLength t =  60 + fromEnum t - fromEnum 'A' + 1
      
processNextEvent :: Project -> Heap.MinHeap Event -> Int -> [ Event ]
processNextEvent project events workers =
  case Heap.view events of
    Nothing -> []
    Just (endingTask, events) ->
      schedule
        (deliverTask project $ task endingTask)
        events
        (workers + 1)
        (time endingTask)

startNewTask :: Project -> Heap.MinHeap Event -> Int -> Int -> [ Event ]
startNewTask project events workers time = 
 case findSmallestAvailableTask project of
    Nothing ->
      processNextEvent project events workers
    Just task ->
      newEvent
      : schedule
          (startTask project task)
          (Heap.insert newEvent events)
          (workers - 1)
          time
      where newEvent = Event (time + taskLength task) task

schedule :: Project -> Heap.MinHeap Event -> Int -> Int -> [ Event ]
schedule project events workers time =
  if workers == 0 then
    processNextEvent project events workers
  else
    startNewTask project events workers time

processPart2 :: String -> String
processPart2 =
  show
  . maximum
  . map time
  . (\ project -> schedule project Heap.empty 5 0)
  . parseProject
  
         
         

                                          
   





