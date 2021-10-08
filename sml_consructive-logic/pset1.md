# Proofs PS1

1. Prove A ⊃ A true.
```
   ---------- u
      A true
  -------------- I^u
    A ⊃ A true
```


2. Prove (A ∧ B) ⊃ B true
```
  ----------- u    -- discharge here.
    A ∧ B true
  ------------- ∧ER
     B true
 ------------------ ⊃Iᵘ
   A ∧ B ⊃ B true
```


4. Prove A ∧ B ⊃ B ∧ A
```
   ----------- u        ---------- u
     A ∧ B true           A ∧ B true
   ------------ ∧ER     ----------- ∧EL
      B true               A true
  ------------------------------------ ∧I
      B ∧ A true
   -------------------- ⊃Iᵘ
     A ∧ B ⊃ B ∧ A true
```

5. Prove A ∧ (A ⊃ B) ⊃ B true
 This one says A and A implies B, we know A implies B and A, so B is immediately true.
 it's like a lazy function (a: t , f : t -> s).
```
  ----------------------- u     ----------------------- u
      A ∧ (A ⊃ B) true             A ∧ (A ⊃ B) true
      -------- ∧EL                -------------------- A
          A true                       A  ⊃ B true
      ------------------------------------------------- ⊃E
                        B true
------------------------------------------------------------- ⊃Iᵘ
                 A ∧ (A ⊃ B) ⊃ B true
```

6 . Prove A ∧ (A ⊃ B) ∧ (B ⊃ C) ⊃ C true

```
  --------------- u
  A∧(A⊃B)∧(B⊃C) true
 ------------------∧ER ------------------ u   --------------- u
  (A⊃B)∧(B⊃C) true      A∧(A⊃B)∧(B⊃C) true      A∧(A⊃B)∧(B⊃C) true
  ----------------     ------------------∧E   ------------------- ∧ER
      (A⊃B) true                A true           (A⊃B)∧(B⊃C) true
---------------------------------------- ⊃EL ------------------∧EL
              B true                                   (B ⊃ C) true
     ------------------------------------------------------------- ⊃E
            C true
     ---------------------- ⊃Iᵘ
        A∧(A⊃B)∧(B⊃C)⊃C true
```

7. Prove A ⊃ (B ⊃ A) true
```
     ----------- u
       A true
    -------------- ⊃Iᵛ
     B⊃A true
 ----------------- ⊃Iᵘ
    A⊃(B⊃A)
```


8. Prove A ⊃ (B ⊃ (A ∧ B))
discharging u and w (two hypothetical judgements used only for hypothetical derivation. Once they are used for the introduction of their corresponding impliation, they can no longer be used afterwards).
```
    ------- u       ---------- w
     A true             B true
    ----------------------------- ∧I
            (A∧B) true
      ------------------ ⊃Iʷ
          B⊃(A∧B) true
    ----------------------- ⊃Iᵘ
         A⊃(B⊃(A∧B)) true
```


