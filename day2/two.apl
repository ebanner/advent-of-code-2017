n ← ⎕
rows ← ↑ {⎕}¨ ⍳n

_ m ← ⍴ rows
I ← (⍳m) ∘.= (⍳m)

quot ← {÷/ ⌽ ⍵[⊃ ⍸ 0 = (⍵ ∘.| ⍵) + I]}

⎕ ← +/ quot ⍤ 1 ⊢ rows
