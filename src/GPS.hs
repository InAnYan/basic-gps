{-# LANGUAGE MultiParamTypeClasses #-}

module GPS (Operator (..), gps) where

import Control.Monad (foldM)

-- | Class for representing the operators.
-- The property must hold: if doesAchieve o g then elem g (changeState s).
class (Eq g) => Operator o g where
  preconditions :: o -> [g]
  changeState :: o -> [g] -> [g]
  doesAchieve :: o -> g -> Bool

-- | The General Problem Solver algorithm.
gps :: (Operator o g) => [o] -> [g] -> [g] -> Maybe [o]
gps ops goals state = snd <$> achieveAll goals state
  where
    achieveAll state = foldM achieve (state, [])
      where
        achieve (state, acc) goal =
          if goal `elem` state
            then Just $ (state, acc)
            else firstJust tryApply findApplicable
          where
            findApplicable = filter (`doesAchieve` goal) ops

            tryApply o = do
              (state', actions) <- achieveAll state $ preconditions o
              return (state', acc ++ actions ++ [o])

-- It took me some time to make it. It looks very simple, but I've spend
-- a lot of time looking at different function types.

-- A little problem: I would like to write the types of functions in the code.
-- But there are problems with `ridid type` or `rigid variable` stuff.
-- So I decided not to write the types explicitly and it worked!
-- At first times, I wrote all the functions as global functions (I mean outside
-- the local scope of gps) and it worked. But with local functions I can create
-- closures and reference the list of operators in the gps argument instead of passing
-- it around and makind partial applications.

-- Types of functions in gps:
-- achieveAll :: (Operator o g) => [g] -> [g] -> Maybe ([g], [o])
-- achieve :: (Operator o g) => ([g], [o]) -> g -> Maybe ([g], [o])
-- findApplicable :: [o]
-- tryApply :: o -> Maybe ([g], [o])

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f (x : xs) = case f x of
  Just r -> Just r
  Nothing -> firstJust f xs
firstJust _ _ = Nothing