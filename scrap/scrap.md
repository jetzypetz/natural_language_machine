## What I want from the chatbot

stages i want to reach

1. "alice likes bob" -> True/False

2. "alice likes bob and bob likes alice" -> True/False

3. "alice likes bob and alice" -> True/False

4. "who likes alice" -> [people]

5. "who does alice like" -> [people]

6. "bob likes who likes alice" -> T/F

7. "bob likes who alice likes" -> T/F

8. "song1 matches song2" -> T/F

9. "what matches song1" -> [songs]

## lambek calculus

alice       : np
bob         : np

likes       : np \ ( s / np )

everyone    : s / ( np \ s )


everyone likes bob:

s / ( np \ s ) np \ ( s / np ) np
<=
s / ( np \ s ) np \ s
<=
s

