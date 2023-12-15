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
            then Just (state, acc)
            else firstJust tryApply findApplicable
          where
            findApplicable = filter (`doesAchieve` goal) ops

            tryApply o = do
              (state', actions) <- achieveAll state $ preconditions o
              return (state', acc ++ actions ++ [o])

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f (x : xs) = case f x of
  Just r -> Just r
  Nothing -> firstJust f xs
firstJust _ _ = Nothing