{-# LANGUAGE DeriveGeneric #-} -- Source : Stackoverflow
{-# LANGUAGE OverloadedStrings #-}
module AdressBook where

import Data.Sequence (Seq)
import qualified Data.Sequence as S

import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import GHC.Generics

data Contact = Contact {
  nom :: String
  , prenom :: String
  , email :: Email
  , tel :: Telephone
  } deriving (Generic, Show, Eq, Ord)

instance ToJSON Contact where
instance FromJSON Contact where
data Email = Email {
  basePart :: String
  , atPart :: String
  } deriving (Generic, Eq, Ord)

instance ToJSON Email where
instance FromJSON Email where
instance Show Email where
  show (Email basePart atPart) =
    basePart ++ "@" ++ atPart

data Telephone = Telephone {
  part1 :: String
  , part2 :: String
  , part3 :: String
  , part4 :: String
  , part5 :: String
  } deriving (Generic, Eq, Ord)

instance ToJSON Telephone where
instance FromJSON Telephone where
instance Show Telephone where
  show (Telephone part1 part2 part3 part4 part5) =
    part1 ++ "." ++ part2 ++ "." ++ part3 ++ "." ++ part4 ++ "." ++ part5
    
data AdressBook = AdressBook (Seq Contact)
  deriving (Show, Eq)

presentContact :: Contact -> String
presentContact (Contact nom prenom email tel) =
  "  Nom : " ++ nom ++ "\n"
  ++ "  Prénom : " ++ prenom ++ "\n"
  ++ "  Email : " ++ (show email) ++ "\n"
  ++ "  Tél. : " ++ (show tel) ++ "\n"


presentBook :: AdressBook -> String
presentBook (AdressBook contacts) = S.foldlWithIndex folder "" contacts
  where folder str num contact =
          str ++ "Contact #" ++ (show (num + 1)) ++ " :\n"
          ++ (presentContact contact)
          
saveContact :: Contact -> B.ByteString
saveContact = encode

chargeContact :: B.ByteString -> Maybe Contact
chargeContact = decode