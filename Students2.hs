{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.Students2 where

import Language.Pads.Padsc hiding (take)
import Language.Forest.Forestc

import Language.Pads.GenPretty
import Language.Forest.Auth
import Language.Forest.Graph
import Language.Forest.Shell

import System.FilePath.Posix
import System.Directory
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)

import Data.Map hiding (size)
import Data.List hiding (sort)





ws  = REd "[ \t]+" " "
ows  = REd "[ \t]*" " "
junk = REd ".*"     " "
space = ' '
quote = '\''
comma = ','


[pads| 
  type Grade = Pre "[ABCD][+-]?|F|AUD|N|INC|P"

  data Course = 
    { sort         :: Pre "[dto]",           ws
    , departmental :: Pre "[.D]",            ws
    , passfail     :: Pre "[.p]",            ws
    , level        :: Pre "[1234]",          ws
    , department   :: Pre "[A-Z][A-Z][A-Z]", ws
    , number       :: Pint where <| 100 <= number && number < 600 |>, ws
    , grade        :: Grade,                 junk                               
    } 

  data Middle_name = {space, middle :: Pre "[a-zA-Z]+[.]?" }           
 
  data Student_Name(myname::String) = 
    { lastname   :: Pre "[a-zA-Z]*"  where <| toString lastname ==  myname |>,  comma, ows     
    , firstname  :: Pre "[a-zA-Z]*" 
    , middlename :: Maybe Middle_name
    }

  data School = AB | BSE

  data Person (myname::String) =
    { fullname   :: Student_Name myname,    ws
    , school     :: School,                 ws, quote
    , year       :: Pre "[0-9][0-9]"
    }

  type Header  = [Line (Pre ".*")] with length 7 
  type Trailer = [Line (Pre ".*")] with term Eof 
  data Student (name::String) = 
    { person  :: Line (Person name)
    , header :: Header  
    , courses :: [Line Course]
    , trailer :: Trailer
    }
|]

-- Auxiliary code
template' s = or [ s == "SSSS.txt"
                , s == "SSS.txt"
                , s == "sxx.txt"
                , s == "sss.txt"
                , s == "ssss.txt" ]

template s = s `elem` ["SSSS.txt", "SSS.txt", "sxx.txt", "sss.txt", "ssss.txt"]

not_template = not . template

getYear :: String -> Integer
getYear s = read (reverse (take 2 (reverse s)))
--toStr :: Integer -> Integer -> String
toStrN i n = (replicate  (n - length (show i)) '0') ++ (show i)
mkClass y = "classof" ++ (toStrN y 2)


transferRE  = RE "TRANSFER|Transfer"
leaveRE     = RE "LEAVE|Leave"
withdrawnRE = RE "WITHDRAWN|WITHDRAWAL|Withdrawn|Withdrawal|WITHDREW"
cRE         = RE "classof[0-9][0-9]" 
txt         = GL "*.txt"

[forest|
  -- Collection of files containing all students in a particular major.
  type Major = Map 
    [ s :: File (Student <| dropExtension s |>) 
    | s <- matches txt,  <| (not . template) s |>  ]  

  -- Directory containing all students in a particular year
  type Class (y :: Integer) = Directory
    { bse is <|"BSE" ++ (toStrN y 2)|> :: Major
    , ab  is <|"AB"  ++ (toStrN y 2)|> :: Major   
    , transfer  matches transferRE  :: Maybe Major 
    , withdrawn matches withdrawnRE :: Maybe Major 
    , leave     matches leaveRE     :: Maybe Major 
    }

  -- Collection of directories containing graduated students
  type Grads = 
     Map [ c :: Class <| getYear c |> | c <- matches cRE ] 

  -- Root of the hierarchy
  type PrincetonCS (y::Integer) = Directory
    { notes is "README" :: Text
    , seniors   is <|mkClass y      |> :: Class y
    , juniors   is <|mkClass (y + 1)|> :: Class <| y + 1 |>
    , graduates :: Grads
    }
|]






mkPrettyInstance ''PrincetonCS
mkPrettyInstance ''PrincetonCS_md

cs_dir = "data/CS"
(cs_rep, cs_md) = unsafePerformIO $ princetonCS_load 11 cs_dir

doit = 
 do  { (cs_rep,cs_md) <- princetonCS_load 11 "data/CS"
--     ; return (findFiles cs_md (\(r::FileInfo) -> (kind r) == DirectoryK))
     ; return  (findFiles cs_md (\(r::FileInfo) -> 
                                (owner r) /= "dpw"))
     }

permissions = checkAuth cs_md  "data/CS/graduates/classof07/BSE07/clark.txt" "kfisher"
readStatus = canRead cs_md  "data/CS/graduates/classof07/BSE07/clark.txt" "kathleenfisher"
-- Right (False,["data/CS/graduates","data/CS/graduates/classof07/BSE07"])

noRead = readProhibited cs_md "kathleenfisher"
problemPaths = restrictingPaths noRead
-- ["data/CS/graduates","data/CS/graduates/classof07/BSE07"]

cd_md md f = f $ snd md  -- should this change the paths?
cd_rep rep f = f $ rep

{- print graph of students -}
resultIO =  mdToPDF cs_md "StudentsNew.pdf"

{- tar the student repostitory -}
doTar = tar cs_md "Princeton.tar"

{- get directory listing, opt is something like "-al" -}
doLs opt = do { r <- ls cs_md opt; putStrLn r}

{- grep for HST by calling with "HST" -}
doGrep opt = do { r <- grep cs_md opt; putStrLn r}

{- cp repository -}
doCopy = cp cs_md "/Users/kfisher/Work/temp"

{- remove repository -}
doRemove = do { result <- rm cs_md ""; putStrLn result}


princetonCS_tarFiles filePath name = do
 { ~(rep,md) <- princetonCS_load 12 filePath
 ; tar md name
 }

major_tarFiles filePath name = do
 { ~(rep,md) <- major_load filePath¡
 ; tar md name
 }

grads_tarFiles filePath name = do
 { ~(rep,md) <- grads_load filePath
 ; tar md name
 }

class_tarFiles arg filePath name = do
 { ~(rep,md) <- class_load arg filePath
 ; tar md name
 }


doShellTar = do
 { [descName, outputName] <- getArgs
 ; absCurrentDir <- getCurrentDirectory
 ; currentDir <- makeRelativeToCurrentDirectory absCurrentDir
 ; case getLoadArgs descName of 
      ("PrincetonCS", Nothing) -> princetonCS_tarFiles currentDir outputName
      ("Grads", Nothing)       -> grads_tarFiles currentDir outputName
      ("Major", Nothing)       -> major_tarFiles currentDir outputName
      ("Class", Just arg)      -> class_tarFiles (read arg) currentDir outputName
 }



-- Find all files mentioned in cs
files = listFiles cs_md

grad09_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/facadm/graduates/classof09"
(grad09_rep, grad09_md) = unsafePerformIO $ (class_load 09) grad09_dir

majorBSE09_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/facadm/graduates/classof09/BSE09"
(bse09_rep, bse09_md) = unsafePerformIO $ major_load  majorBSE09_dir

Grads grads = graduates cs_rep
grads07 = grads ! "classof07" 
Major bse_grads07 = bse grads07

errs = fst cs_md

errsP = pretty 80 (ppr errs)


clark = bse_grads07 ! "clark.txt"
clark_doc = student_ppr clark
clark_output n = putStrLn (pretty n clark_doc)

ppBseGrads07 n = putStrLn (pretty n (major_ppr (bse grads07)))

student_input_file = "/Users/kfisher/pads/dirpads/src/Examples/data/facadm/classof10/AB10/APPS.txt"
student_result :: (Student, Student_md) = unsafePerformIO $ parseFile1 "APPS" student_input_file
-- (Student apps apps_courses, sfmd) = unsafePerformIO $ load1 "APPS" student_input_file

finger_input_file = "/Users/kfisher/pads/dirpads/src/Examples/data/facadm/classof11/WITHDREW/finger.txt"
finger_result :: (Student, Student_md) = unsafePerformIO $ parseFile1 "finger" finger_input_file

course_input = "d D . 3 JPN 238 INC"
course_result = course_parseS course_input

major_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/facadm/classof11/AB11"
(major_rep, major_md) = unsafePerformIO $ major_load  major_dir

bse11_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/facadm/classof11/BSE11"
(bse11_rep, bse11_md) = unsafePerformIO $ major_load  bse11_dir

withdrawn_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/facadm/classof11/WITHDREW"
(withdrawnt_rep, withdrawnt_md) = unsafePerformIO $ major_load  withdrawn_dir

class_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/facadm/classof11"
(class_rep, class_md) = unsafePerformIO $ (class_load 11) class_dir

class07_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/facadm/graduates/classof07"
(class07_rep, class07_md) = unsafePerformIO $ (class_load 07) class07_dir

class10_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/facadm/classof10"
(class10_rep, class10_md) = unsafePerformIO $ (class_load 10) class10_dir

class11_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/facadm/classof11"
(class11_rep, class11_md) = unsafePerformIO $ (class_load 11) class11_dir

grad_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/facadm/graduates"
(grad_rep, grad_md) = unsafePerformIO $ grads_load grad_dir



