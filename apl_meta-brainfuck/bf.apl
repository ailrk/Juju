)copy dfns display

⍝ Brain buck.
⍝ The machine has two tapes: a `token stream` and an `infinite tape`.
⍝ 8 operations:
⍝   . : output
⍝   , : input
⍝   < / >: move tape ptr one left/right
⍝   + / -: inc of dec value on current tape cell
⍝   [ / ]: [ if 0 jump to ]; ] if not 0 jump to [

⍝ -----------------------------------------------------------------------------
⍝ brainfuck impl
⍝ -----------------------------------------------------------------------------

⍝ A tape is a cell with three elements:
⍝  left | curent | right
⍝ extending the tape means creating a new layer of nesting.
⍝                       ┌→──────────┐
⍝   ┌→────┐             │     ┌→──┐ │
⍝   │0 1 0│    →        │ 0 0 │1 0│ │
⍝   └~────┘ move left   │     └~──┘ │
⍝                       └∊──────────┘


bf←{
  ⎕IO ⎕ML←0

  ⍝ extends tape if exceeds.
  left←{(l1 l2) m r←⍵⋄l1 l2(m r)} ⋄ right←{l m (r1 r2)←⍵⋄(l m)r1 r2}
  get←{_ m _←⍵⋄m} ⋄ set←{l _ r←⍵⋄l ⍺ r}
  if←{(⍺⍺⍣⍵)⍺}    ⍝ double recursion

  ⍝ note in performance: O(1) with pattern matching. If we do +∘1 0 1 it will
  ⍝ be O(n) because we need to traverse each spine at least once.
  inc←{l m r←⍵⋄l(m+1)r} ⋄ dec←{l m r←⍵⋄l(m-1)r}
  input←{(⎕UCS ⍬⍴⍞)put ⍵} ⋄ output←{⍵⊣⍞←⎕UCS get ⍵}

  jump←{
    from to←{⍵,'[]'~⍵}get ⍵
  }

  run←{
    token←get ⍵
    '>'≡token:(right ⍺)∇right ⍵ ⋄ '<'≡token:(left ⍺)∇right ⍵
    '+'≡token:(inc ⍺)∇ right ⍵ ⋄ '-'≡token:(dec ⍺)∇ right ⍵
    '['≡token:⍺ ∇ right⍵ right skip if 0=get ⍺
    ']'≡token:⍺ ∇ right⍵ left skip if 0≠get ⍺
    '.'≡token:(output ⍺)∇ right ⍵ ⋄ ','≡token:(input ⍺)∇ right ⍵
    '∘'≡token:shy←1↓∊⍺
    ⍺ ∇ right ⍵
  }

  ⍺←0 ⋄ tape←0, ↑{⍺ ⍵}/⍺,0 ⋄ tape run '∘',↑{⍺ ⍵}/⍵,'∘'
}


⍝ -----------------------------------------------------------------------------
⍝ macro
⍝ -----------------------------------------------------------------------------



