module Test.MySolutions where

import Data.AddressBook (AddressBook, Entry)

import Prelude

import Data.List (filter, head, null, nubByEq)
import Data.Maybe (Maybe)


findEntryByStreet :: String -> AddressBook -> Maybe Entry 
findEntryByStreet strt = head <<< filter filterEntryBS
   where
   filterEntryBS :: Entry -> Boolean 
   filterEntryBS entry = _.address.street entry == strt 

findEntryByStreet' :: String -> AddressBook -> Maybe Entry 
findEntryByStreet' strt = head <<< filter (_.address.street >>> eq strt) 

isInBook :: String -> String -> AddressBook -> Boolean 
isInBook fn ln bk = not null $ filter filterEntry bk
     where 
     filterEntry entry = entry.firstName == fn && entry.lastName == ln

-- nubByEq :: (a -> a -> Boolean) -> List a -> List a

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq duplicateEntries
   where 
   duplicateEntries :: Entry -> Entry -> Boolean 
   duplicateEntries a b = a.firstName == b.firstName && a.lastName == b.lastName
