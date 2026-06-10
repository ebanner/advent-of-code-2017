n ← ⎕
rows ← ↑ {⎕}¨ ⍳n

_ m ← ⍴ rows

quot ← {÷/ ⌽ ⍵[⊃ ⍸ 0 = (⍵ ∘.| ⍵) + (⍳m) ∘.= (⍳m)]}

⎕ ← +/ quot ⍤ 1 ⊢ rows
