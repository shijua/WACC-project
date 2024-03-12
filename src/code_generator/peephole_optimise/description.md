# Peephole Optimisation

Generally Speaking, peephole optimisations are optimisations that will look at adjacent IRs after code generation.

## Remove Self-Movements

This including removal of 
1. `mov X X`
2. `MovS X X`

## Redundant Load & Store

```text
LD a R0
ST R0 a
```

## Eliminating Unreachable Code

This is completed by our control-flow optimisation

However, we would need to still 

## Flow-of-Control Optimisations

## Algebraic Simplification and Reduction in Strength

- a ± 0 = a
- a - a = 0
- a * 2 = a + a
- a * 1 = a
- a * 0 = 0
- a / 1 = a
- a / a = 0 (given a != 0)


## Use of Machine Idioms


