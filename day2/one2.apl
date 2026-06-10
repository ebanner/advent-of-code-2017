n←⎕
rows←{⎕}¨⍳n
differences←(⌈/-⌊/)¨rows
⎕←+/differences
