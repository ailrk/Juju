{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where


import Data.List ( group, sortOn, find, groupBy )
import System.IO
import Text.Pretty.Simple
import Debug.Trace
import Data.Function (on)
import Control.Monad (forM_)

formatSession :: Session -> String
formatSession session = timeSlotFormatted ++ ", " ++ show session.tutor ++ ", " ++ show status
  where 
    status = case session.status of
               Empty levels -> show levels
               Scheduled lesson students -> show lesson ++ ", booked: " ++ (concat $ fmap (++" ") students)
               _ -> "X"

    timeSlotFormatted = 
      let TimeSlot date from to = session.timeSlot 
       in show date ++ " " ++ show from ++ ":00 - " ++ show to ++ ":00"
      

main :: IO ()
main = loop (Schedule exampleSchedule1)
  where 
    printSchedule schedule = do 
      let (Schedule sessions) = schedule

      forM_ (fmap formatSession (sortOn (.timeSlot) sessions)) $ \s -> do 
        putStrLn s
    loop schedule = do 
      putStr "> " >> hFlush stdout
      line <- getLine
      case words line of 
        ".p" : _ -> printSchedule schedule >> loop schedule
        ".s" : sid : _ -> do 
          let mStudent = find ((== sid) . (.id)) allStudents
          case mStudent of 
            Just student -> do
              pPrint student
              putStrLn "Your possible next sessions: "
              let sessions = availableSlots student schedule
              forM_ ([0..] `zip` sessions) $ \(idx, session) -> putStrLn (show idx ++ ": " ++ formatSession session)
              putStr "choose a session> " >> hFlush stdout
              n :: Int <- read <$> getLine
              case lookup n ([0..] `zip` sessions) of 
                Just session -> do
                  let newSchedule = pick student session schedule
                  printSchedule newSchedule
                  loop newSchedule
                _ -> putStrLn "invalid option " >> loop schedule
            Nothing -> putStrLn "No such student" 
        _ -> putStrLn "unknonw command" >> loop schedule
      
type UserId = String


data Date = Mon | Tue | Wed | Thu | Fri deriving (Show, Eq, Ord)
data Level = B1 | B2 | A2 deriving (Eq, Show)
data TimeSlot = TimeSlot Date Int Int deriving (Show, Eq, Ord)
type ModNum = Int


data Lesson = Lesson Level ModNum deriving (Eq, Show)


data Tutor = Tutor { id :: UserId, availableLevels :: [Level]  }


data Student = Student
  { id :: UserId
  , finishedLesson :: Maybe Lesson
  , nextLesson :: Lesson
  }
  deriving (Eq, Show)


data SlotStatus
  = Scheduled Lesson [UserId]
  | Empty [Level]
  | Full Lesson [UserId]
  deriving (Show, Eq)

data Session = Session 
    { timeSlot :: TimeSlot 
    , status :: SlotStatus 
    , tutor :: UserId
    }
 
  deriving (Show, Eq)

data Schedule = Schedule [Session] deriving (Show, Eq)



data Demand = Demand 
  {

  }


-- assume 2 tutors
exampleSchedule1 :: [Session]
exampleSchedule1 =
  [ Session
      { timeSlot = TimeSlot Mon 7 8
      , status = Empty [B1, B2]
      , tutor = "x123" 
      }
  , Session
      { timeSlot = TimeSlot Mon 8 9
      , status = Empty [B1, B2]
      , tutor = "x123" 
      }
  , Session
      { timeSlot = TimeSlot Tue 9 10
      , status = Empty [B1, B2]
      , tutor = "x123" 
      }
  , Session
      { timeSlot = TimeSlot Mon 7 8
      , status = Empty [B1]
      , tutor = "x001" 
      }
  , Session
      { timeSlot = TimeSlot Mon 8 9
      , status = Empty [B1]
      , tutor = "x001" 
      }
  , Session
      { timeSlot = TimeSlot Tue 12 13
      , status = Empty [B1, B2]
      , tutor = "x001" 
      }
 ]



allStudents :: [Student]
allStudents = 
  [ Student { id = "s1" , finishedLesson = Nothing,  nextLesson = Lesson B1 1 }
  , Student { id = "s2" , finishedLesson = Nothing, nextLesson = Lesson B1 1 }
  , Student { id = "s3" , finishedLesson = Nothing, nextLesson = Lesson B1 1 }
  , Student { id = "s4" , finishedLesson = Just (Lesson B2 1), nextLesson = Lesson B2 2 }
  , Student { id = "s5" , finishedLesson = Nothing, nextLesson = Lesson B2 1 }
  , Student { id = "s6" , finishedLesson = Just (Lesson B1 40), nextLesson = Lesson B2 1 }
  , Student { id = "s7" , finishedLesson = Just (Lesson B2 2) , nextLesson = Lesson B2 3 }
  , Student { id = "s8" , finishedLesson = Just (Lesson B2 1) , nextLesson = Lesson B2 2 }
  , Student { id = "s8" , finishedLesson = Just (Lesson B2 1) , nextLesson = Lesson B2 2 }
  , Student { id = "s9" , finishedLesson = Just (Lesson B2 1) , nextLesson = Lesson B2 2 }
  , Student { id = "s10" , finishedLesson = Just (Lesson B2 1) , nextLesson = Lesson B2 2 }
  , Student { id = "s11" , finishedLesson = Just (Lesson B2 1) , nextLesson = Lesson B2 2 }
  , Student { id = "s12" , finishedLesson = Nothing , nextLesson = Lesson B2 1 }
  , Student { id = "s13" , finishedLesson = Nothing, nextLesson = Lesson B2 1 }
  , Student { id = "s14" , finishedLesson = Just (Lesson B2 1) , nextLesson = Lesson B2 2 }
  , Student { id = "s15" , finishedLesson = Just (Lesson B1 1) , nextLesson = Lesson B1 2 }
  , Student { id = "s16" , finishedLesson = Just (Lesson B1 1) , nextLesson = Lesson B1 2 }
  , Student { id = "s17" , finishedLesson = Just (Lesson B2 1) , nextLesson = Lesson B2 2 }
  , Student { id = "s18" , finishedLesson = Nothing, nextLesson = Lesson B1 1 }
  , Student { id = "s19" , finishedLesson = Nothing, nextLesson = Lesson B1 1 }
  , Student { id = "s20" , finishedLesson = Nothing, nextLesson = Lesson B1 1 }
  , Student { id = "s21" , finishedLesson = Nothing, nextLesson = Lesson B1 1 }
  , Student { id = "s22" , finishedLesson = Nothing, nextLesson = Lesson B1 1 }
  , Student { id = "s23" , finishedLesson = Nothing, nextLesson = Lesson B1 1 }
  , Student { id = "s24" , finishedLesson = Nothing, nextLesson = Lesson B1 1 }
  , Student { id = "s25" , finishedLesson = Nothing, nextLesson = Lesson B1 1 }
  , Student { id = "s26" , finishedLesson = Nothing, nextLesson = Lesson B1 1 }
  , Student { id = "s27" , finishedLesson = Nothing, nextLesson = Lesson B1 1 }
  , Student { id = "s28" , finishedLesson = Nothing, nextLesson = Lesson B1 1 }
  , Student { id = "s29" , finishedLesson = Nothing, nextLesson = Lesson B1 1 }
  , Student { id = "s30" , finishedLesson = Nothing, nextLesson = Lesson B1 1 }
  ]



availableSlots :: Student -> Schedule -> [Session]
availableSlots student@(Student _ _ lesson@(Lesson level modNum)) (Schedule timeSlots) =
  fmap pickSession 
  . groupBy ((==) `on` (.timeSlot))
  . sortOn (.timeSlot) 
  . filter isAvailable 
  $ timeSlots
  where 
    pickSession :: [Session] -> Session
    pickSession slots = head $ sortOn priority slots
      where 
        priority s@(Session _ (Scheduled _ _) _) = 0
        priority (Session _ (Empty levels) _) =  length levels
        priority _ = 100

    isAvailable (Session timeSlot status tutor) = 
      case status of 
        Scheduled lesson' students -> lesson == lesson'  && length students <= 8
        Empty levels -> level `elem` levels 
        Full _ _ -> False
       



pick :: Student -> Session -> Schedule -> Schedule
pick student@(Student studentId _ lesson ) ts@(Session timeSlot (Empty _) tutor) (Schedule timeSlots) 
  | ts `elem` timeSlots = Schedule $ (Session timeSlot (Scheduled lesson [studentId]) tutor) : filter (/= ts) timeSlots
  | otherwise = Schedule timeSlots
pick student ts@(Session timeSlot (Scheduled lesson students)  tutor) (Schedule timeSlots) = 
  Schedule $ (Session timeSlot (Scheduled lesson (student.id : students) ) tutor) : filter (/= ts) timeSlots
