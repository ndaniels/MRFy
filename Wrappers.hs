module Wrappers where

import Debug.Trace (trace)

import qualified Data.Vector as V

import Constants

vslice msg start len vector = 
  if slicing debug then
    trace ("DEBUG: slicing (" ++ (show start) ++ 
           ", " ++ (show len) ++ ") of " ++ (show $ V.length vector) ++ " : " ++ msg) 
          $ V.slice start len vector
  else
    V.slice start len vector

