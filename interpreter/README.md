https://xavierleroy.org/mpri/2-4/semantics.pdf

  # PS: Normal order reduction
  The original evaluation order is normal ordedr reduction, which always
  evaluate the outer most redexes outside in.

  - For lambda expression, if it has a normal form, it has unique normal form
  - Normal order evaluation always find the unique normal form (if exists).
  - For non halting reduction there is no normal form what so ever.

  https://www.cs.cornell.edu/~kozen/Papers/ChurchRosser.pdf
  http://www.cs.columbia.edu/~aho/cs3261/Lectures/L24-Lambda_Calculus_II.html
  # PS: Church Rosser Theorems (Confluence under beta reduction)
  If there are two reduction path for a expression e to take such that
  e ->* f and e ->* g, then there exists a h that f -> * h and g ->* h
