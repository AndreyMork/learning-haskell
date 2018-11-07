patientInfo1 :: FirstName -> LastName -> Age -> Height -> String
patientInfo1 fname lname age height =
  name ++ " " ++ ageHeight
  where
    name = fname ++ ", " ++ lname
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

type FirstName = String
type LastName = String
type Age = Int
type Height = Int
type Weight = Int
type PatientName = (String, String)

firstName :: PatientName -> String
firstName patient = fst(patient)

lastName :: PatientName -> String
lastName patient = snd(patient)

patientInfo2 :: PatientName -> Age -> Height -> String
patientInfo2 patientName age height =
  name ++ " " ++ ageHeight
  where
    name = firstName patientName ++ ", " ++ lastName patientName
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

data Gender = Male | Female | Other
genderInitial :: Gender -> Char
genderInitial Male = 'M'
genderInitial Female = 'F'
genderInitial Other = 'O'

data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType

patient1BT :: BloodType
patient1BT = BloodType A Pos
patient2BT :: BloodType
patient2BT = BloodType O Neg
patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"
showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"
showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

type MiddleName = String
data Name =
  Name FirstName LastName |
  NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name first last) = first ++ " " ++ last
showName (NameWithMiddle first middle last) =
  first ++ " " ++ middle ++ " " ++ last

-- data Patient = Patient Name Gender Age Height Weight BloodType
johnDoe :: Patient
johnDoe =
  Patient (Name "John" "Doe")
  Male 30 74 200 (BloodType AB Pos)

data Patient = Patient {
  name :: Name,
  gender :: Gender,
  age :: Int,
  height :: Int,
  weight :: Int,
  bloodType :: BloodType
}

jackieSmith :: Patient
jackieSmith = Patient {
  name = Name "Jackie" "Smith",
  age = 43,
  gender = Female,
  height = 62,
  weight = 115,
  bloodType = BloodType O Neg
}

janeESmith :: Patient
janeESmith = Patient {
  name = NameWithMiddle "Jane" "Elizabeth" "Smith",
  age = 28,
  gender = Female,
  height = 62,
  weight = 140,
  bloodType = BloodType B Neg
}
-- Q12.1
canDonateToPatient :: Patient -> Patient -> Bool
canDonateToPatient patient1 patient2 =
  canDonateTo (bloodType patient1) (bloodType patient2)

-- Q12.2

sep = take 14 (cycle "*") ++ "\n"
showGender Female = "Female"
showGender Male = "Male"
showGender Other = "Other"

patientSummary :: Patient -> String
patientSummary patient =
  sep ++
  "Patient Name: " ++ showName (name patient) ++ "\n" ++
  "Gender: " ++ showGender (gender patient) ++ "\n" ++
  "Age: " ++ show (age patient) ++ "\n" ++
  "Height: " ++ show (height patient) ++ " in.\n" ++
  "Weight: " ++ show (weight patient) ++ " lbs.\n" ++
  "Blood Type: " ++ showBloodType (bloodType patient) ++ "\n" ++
  sep
