{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}

{-|
Module      : Main
Description : A concurrent social network simulator using threads
Copyright   : (c) 2024
License     : MIT
Maintainer  : student@example.com

This module implements a social network simulator where multiple user threads
concurrently send messages to each other. The system uses MVars for thread-safe
communication and coordination.

Extensions implemented:
1. Message timestamps for chronological tracking
2. Message sentiment analysis (positive/neutral/negative)
3. Real-time statistics with formatted output
-}

module Main where

import Control.Concurrent
import Control.Monad (forM_, when, void)
import System.Random
import Data.Time.Clock
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import Text.Printf

-- | Represents a user in the social network with a unique username and inbox
data User = User
  { userName :: String           -- ^ The user's unique username
  , userInbox :: MVar [Message]  -- ^ Thread-safe inbox for receiving messages
  , messageCount :: MVar Int     -- ^ Count of messages received by this user
  }

-- | Represents a message sent between users
data Message = Message
  { sender :: String      -- ^ Username of the message sender
  , recipient :: String   -- ^ Username of the message recipient
  , content :: String     -- ^ The message content
  , timestamp :: UTCTime  -- ^ When the message was sent (Extension 1)
  , sentiment :: Sentiment -- ^ Message sentiment (Extension 2)
  } deriving (Show)

-- | Sentiment classification for messages (Extension)
data Sentiment = Positive | Neutral | Negative
  deriving (Show, Eq)

-- | Shared state for the entire social network
data NetworkState = NetworkState
  { totalMessages :: MVar Int     -- ^ Total number of messages sent
  , allUsers :: [User]            -- ^ List of all users in the network
  , networkActive :: MVar Bool    -- ^ Flag to signal when simulation should stop
  , startTime :: UTCTime          -- ^ When the simulation started
  }

-- | Message templates categorized by sentiment (Extension 2)
positiveMessages :: [String]
positiveMessages = 
  [ "Hello! How are you? Hope you're doing great!"
  , "What's up? I'm having an amazing day!"
  , "Did you see the latest news? It's fantastic!"
  , "Let's catch up soon! I'd love to hear from you!"
  , "Hope you're having a wonderful day!"
  , "Just wanted to say hi and send good vibes!"
  , "Thinking of you! You're awesome!"
  , "Check this out! You'll love it!"
  ]

neutralMessages :: [String]
neutralMessages = 
  [ "Hi, just checking in."
  , "Did you see that?"
  , "Random message incoming."
  , "What do you think about this?"
  , "Here's an update."
  , "Here's some info."
  ]

negativeMessages :: [String]
negativeMessages = 
  [ "Not sure about this..."
  , "This isn't working well."
  , "I'm concerned about the situation."
  , "This could be problematic."
  ]

-- | Main entry point for the social network simulator
main :: IO ()
main = do
  putStrLn "+================================================+"
  putStrLn "|   SOCIAL NETWORK SIMULATOR - Enhanced Edition |"
  putStrLn "+================================================+"
  putStrLn "\n>> Starting simulation with 10 users...\n"
  
  startT <- getCurrentTime
  
  -- Create 10 users
  users <- mapM createUser ["User1", "User2", "User3", "User4", "User5",
                             "User6", "User7", "User8", "User9", "User10"]
  
  -- Initialize network state
  totalMsgCount <- newMVar 0
  activeFlag <- newMVar True
  
  let networkState = NetworkState totalMsgCount users activeFlag startT
  
  -- Spawn a thread for each user
  void $ mapM (forkIO . userThread networkState) users
  
  putStrLn "[OK] All user threads spawned. Simulation running...\n"
  
  -- Wait until 100 messages have been sent
  waitForCompletion networkState
  
  -- Signal all threads to stop
  modifyMVar_ activeFlag (const $ return False)
  
  -- Give threads time to finish
  threadDelay 500000  -- 0.5 seconds
  
  endT <- getCurrentTime
  
  -- Display final statistics
  displayStatistics users startT endT

-- | Creates a new user with an empty inbox
createUser :: String -> IO User
createUser name = do
  inbox <- newMVar []
  msgCount <- newMVar 0
  return $ User name inbox msgCount

-- | Simulates a user's behavior: periodically sends messages to random users
userThread :: NetworkState -> User -> IO ()
userThread NetworkState{..} currentUser = loop
  where
    loop = do
      -- Check if simulation is still active
      active <- readMVar networkActive
      when active $ do
        -- Random delay between 100ms and 1000ms
        delay <- randomRIO (100000, 1000000)
        threadDelay delay
        
        -- Select a random recipient (not self)
        let otherUsers = filter (\u -> userName u /= userName currentUser) allUsers
        recipientIdx <- randomRIO (0, length otherUsers - 1)
        let recipient = otherUsers !! recipientIdx
        
        -- Send a message
        sendMessage currentUser recipient totalMessages
        
        -- Continue loop
        loop

-- | Sends a message from one user to another
sendMessage :: User -> User -> MVar Int -> IO ()
sendMessage sender recipient totalMsgVar = do
  -- Get current message count
  currentCount <- readMVar totalMsgVar
  
  -- Only send if we haven't reached 100 messages
  when (currentCount < 100) $ do
    -- Create the message with sentiment (Extension 2)
    (msgContent, msgSentiment) <- pickRandomMessageWithSentiment
    now <- getCurrentTime
    let msg = Message (userName sender) (userName recipient) msgContent now msgSentiment
    
    -- Add message to recipient's inbox
    modifyMVar_ (userInbox recipient) $ \inbox -> return (msg : inbox)
    
    -- Increment recipient's message count
    modifyMVar_ (messageCount recipient) $ \count -> return (count + 1)
    
    -- Increment total message count
    newCount <- modifyMVar totalMsgVar $ \count -> 
      let newCount = count + 1 
      in return (newCount, newCount)
    
    -- Print progress with sentiment indicator
    let sentimentEmoji = case msgSentiment of
          Positive -> "[+]"
          Neutral  -> "[~]"
          Negative -> "[-]"
    
    printf "[%3d/100] %s %s -> %s: %s\n" 
      newCount sentimentEmoji (userName sender) (userName recipient) msgContent

-- | Picks a random message with sentiment classification (Extension 2)
pickRandomMessageWithSentiment :: IO (String, Sentiment)
pickRandomMessageWithSentiment = do
  -- 60% positive, 30% neutral, 10% negative (realistic distribution)
  roll <- randomRIO (1, 10) :: IO Int
  case roll of
    n | n <= 6 -> do  -- 60% positive
      idx <- randomRIO (0, length positiveMessages - 1)
      return (positiveMessages !! idx, Positive)
    n | n <= 9 -> do  -- 30% neutral
      idx <- randomRIO (0, length neutralMessages - 1)
      return (neutralMessages !! idx, Neutral)
    _ -> do           -- 10% negative
      idx <- randomRIO (0, length negativeMessages - 1)
      return (negativeMessages !! idx, Negative)

-- | Waits until 100 messages have been sent
waitForCompletion :: NetworkState -> IO ()
waitForCompletion NetworkState{..} = loop
  where
    loop = do
      count <- readMVar totalMessages
      if count >= 100
        then putStrLn "\n[OK] 100 messages sent! Stopping simulation...\n"
        else do
          threadDelay 100000  -- Check every 0.1 seconds
          loop

-- | Displays comprehensive final statistics
displayStatistics :: [User] -> UTCTime -> UTCTime -> IO ()
displayStatistics users startT endT = do
  putStrLn "+================================================+"
  putStrLn "|           FINAL STATISTICS REPORT              |"
  putStrLn "+================================================+\n"
  
  -- Basic message count statistics
  stats <- mapM getUserStats users
  
  let sortedStats = sortBy (comparing (Down . snd)) stats
  let total = sum $ map snd stats
  let avg = fromIntegral total / fromIntegral (length users) :: Double
  
  putStrLn "Messages Received per User:"
  putStrLn "================================================"
  forM_ sortedStats $ \(name, count) -> do
    let bar = replicate (count `div` 2) '*'
    printf "%-10s: %2d messages %s\n" name count bar
  
  putStrLn "================================================"
  printf "Total:       %d messages\n" total
  printf "Average:     %.1f messages per user\n\n" avg
  
  -- Extension: Sentiment analysis
  displaySentimentAnalysis users
  
  -- Performance metrics
  let duration = realToFrac (diffUTCTime endT startT) :: Double
  printf "Simulation Duration: %.2f seconds\n" duration
  printf "Messages per second: %.2f\n\n" (fromIntegral total / duration)

-- | Retrieves statistics for a single user
getUserStats :: User -> IO (String, Int)
getUserStats User{..} = do
  count <- readMVar messageCount
  return (userName, count)

-- | Displays sentiment analysis of messages (Extension)
displaySentimentAnalysis :: [User] -> IO ()
displaySentimentAnalysis users = do
  putStrLn "Message Sentiment Analysis:"
  putStrLn "================================================"
  
  allMessages <- concat <$> mapM (readMVar . userInbox) users
  
  let positiveCount = length $ filter (\m -> sentiment m == Positive) allMessages
  let neutralCount = length $ filter (\m -> sentiment m == Neutral) allMessages
  let negativeCount = length $ filter (\m -> sentiment m == Negative) allMessages
  let totalCount = length allMessages
  
  let posPercent = (fromIntegral positiveCount / fromIntegral totalCount * 100) :: Double
  let neuPercent = (fromIntegral neutralCount / fromIntegral totalCount * 100) :: Double
  let negPercent = (fromIntegral negativeCount / fromIntegral totalCount * 100) :: Double
  
  printf "  [+] Positive: %2d messages (%.1f%%)\n" positiveCount posPercent
  printf "  [~] Neutral:  %2d messages (%.1f%%)\n" neutralCount neuPercent
  printf "  [-] Negative: %2d messages (%.1f%%)\n\n" negativeCount negPercent