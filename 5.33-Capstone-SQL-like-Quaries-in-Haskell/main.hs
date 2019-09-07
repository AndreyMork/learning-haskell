import Control.Monad
import Control.Applicative

-- 33.1
data Name = Name {
  firstName :: String,
  lastName :: String
}

instance Show Name where
  show (Name first last) = mconcat [first, " ", last]

-- 33.2
data GradeLevel
  = Freshman
  | Sophmore
  | Junior
  | Senior deriving (Eq, Ord, Enum, Show)

-- 33.3
data Student = Student {
  studentId :: Int,
  gradeLevel :: GradeLevel,
  studentName :: Name
} deriving Show

-- 33.4
students :: [Student]
students = [
  (Student 1 Senior (Name "Audre" "Lorde")),
  (Student 2 Junior (Name "Leslie" "Silko")),
  (Student 3 Freshman (Name "Judith" "Butler")),
  (Student 4 Senior (Name "Guy" "Debord")),
  (Student 5 Sophmore (Name "Jean" "Baudrillard")),
  (Student 6 Junior (Name "Julia" "Kristeva"))]

-- 33.5
-- _select :: (a -> b) -> [a] -> [b]
_select property values = do
  value <- values
  return (property value)

-- 33.6
-- _where :: (a -> Bool) -> [a] -> [a]
_where predicate values = do
  value <- values
  guard (predicate value)
  return value

-- 33.7
startsWith :: Char -> String -> Bool
startsWith char string = char == (head string)

-- 33.8
data Teacher = Teacher {
  teacherId :: Int,
  teacherName :: Name
} deriving Show

-- 33.9
teachers :: [Teacher]
teachers = [
  Teacher 100 (Name "Simone" "De Beauvior"),
  Teacher 200 (Name "Susan" "Sontag")]

-- 33.10
data Course = Course {
  courseId :: Int,
  courseTitle :: String,
  teacher :: Int
} deriving Show

-- 33.11
courses :: [Course]
courses = [
  Course 101 "French" 100,
  Course 201 "English" 200]

-- _join :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a , b)]
_join data1 data2 property1 property2 = do
  d1 <- data1
  d2 <- data2
  let dataPairs = (d1, d2)
  guard (property1 d1 == property2 d2)
  return dataPairs

-- 33.12
joinData = (_join teachers courses teacherId teacher)
whereResult = _where ((== "English") . courseTitle . snd) joinData
selectResult = _select (teacherName . fst) whereResult

-- 33.13
-- _hinq selectQuery joinQuery whereQuery =
--   (\joinData ->
--     (\whereResult -> selectQuery whereResult)
--     (whereQuery joinData)
--   ) joinQuery

_hinq selectQuery joinQuery whereQuery = selectQuery (whereQuery joinQuery)

-- 33.14
finalResult :: [Name]
finalResult = _hinq
  (_select (teacherName . fst))
  (_join teachers courses teacherId teacher)
  (_where ((== "English") . courseTitle . snd))

-- 33.15
teacherFirstName = _hinq
  (_select firstName)
  finalResult
  (_where (\_ -> True))

-- 33.16
_select :: Monad m => (a -> b) -> m a -> m b
_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a, b)

type Select m a b = m a -> m b
type Join m a = m a
type Where m a = m a -> m a

data HINQ m a b
  = HINQ (Select m a b) (Join m a) (Where m a)
  | HINQ_ (Select m a b) (Join m a)
  | HINQ_Empty

-- 33.17
runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ selectClause joinClause whereClause) = _hinq selectClause joinClause whereClause
runHINQ (HINQ_ selectClause joinClause) = _hinq selectClause joinClause (_where (\_ -> True))

query1 :: HINQ [] (Teacher, Course) Name
query1 = HINQ
  (_select (teacherName . fst))
  (_join teachers courses teacherId teacher)
  (_where ((== "English") . courseTitle . snd))

query2 :: HINQ [] Teacher Name
query2 = HINQ_
  (_select teacherName)
  teachers


-- 33.18
possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

-- 33.19
maybeQuery1 :: HINQ Maybe (Teacher, Course) Name
maybeQuery1 = HINQ
  (_select (teacherName . fst))
  (_join possibleTeacher possibleCourse teacherId teacher)
  (_where ((== "French") . courseTitle . snd))


-- 33.20
missingCourse :: Maybe Course
missingCourse = Nothing

maybeQuery2 :: HINQ Maybe (Teacher, Course) Name
maybeQuery2 = HINQ
  (_select (teacherName . fst))
  (_join possibleTeacher missingCourse teacherId teacher)
  (_where ((== "French") . courseTitle . snd))

-- 33.21
data Enrollment = Enrollment {
  student :: Int,
  course :: Int
} deriving Show

-- 33.22
enrollments :: [Enrollment]
enrollments = [
  (Enrollment 1 101),
  (Enrollment 2 101),
  (Enrollment 2 201),
  (Enrollment 3 101),
  (Enrollment 4 201),
  (Enrollment 4 101),
  (Enrollment 5 101),
  (Enrollment 6 201)]

-- 33.23
studentEnrollmentsQuery = HINQ_
  (_select
    (\(student, enrollment) -> (studentName student, course enrollment))
  )
  (_join students enrollments studentId student)

-- 33.24
studentEnrollments :: [(Name, Int)]
studentEnrollments = runHINQ studentEnrollmentsQuery

-- 33.25
englishStudentsQuery = HINQ
  (_select (fst . fst))
  (_join studentEnrollments courses snd courseId)
  (_where ((== "English") . courseTitle . snd))

-- 33.26
englishStudents :: [Name]
englishStudents = runHINQ englishStudentsQuery

-- 33.27
getEnrollments :: String -> [Name]
getEnrollments courseName = runHINQ courseQuery
  where
    courseQuery = HINQ
      (_select (fst . fst))
      (_join studentEnrollments courses snd courseId)
      (_where ((== courseName) . courseTitle . snd))
