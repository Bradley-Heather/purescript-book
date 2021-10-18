module Test.MySolutions where

import Data.AddressBook

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubByEq)
import Data.Maybe (Maybe)


findEntryByStreet :: String -> AddressBook -> Maybe Entry 
findEntryByStreet strt = head <<< filter filterEntryBS
   where
   filterEntryBS :: Entry -> Boolean 
   filterEntryBS entry = _.address.street entry == strt 

isInBook :: String -> String -> AddressBook -> Boolean 
isInBook fn ln bk = not null $ filter filterEntry bk
     where 
     filterEntry entry = entry.firstName == fn && entry.lastName == ln

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates bk = nubByEq bk 