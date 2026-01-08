Argument validation: errors and exit codes.

  $ interpolation --step 0 2>&1
  Error: --step must be > 0
  [1]

  $ interpolation -n 1 2>&1
  Error: -n must be >= 2
  [1]
